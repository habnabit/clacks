#![feature(proc_macro, generators)]

#[macro_use] extern crate slog;

extern crate byteorder;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate clacks_rpc;
extern crate clacks_transport;
extern crate futures_await as futures;
extern crate kabuki_extras;
extern crate rand;
extern crate serde;
extern crate serde_json;
extern crate sexpr;
extern crate slog_async;
extern crate slog_envlogger;
extern crate slog_scope;
extern crate slog_term;
extern crate tokio;
extern crate tokio_io;

use byteorder::{BigEndian, ByteOrder, LittleEndian};
use clacks_crypto::csrng_gen;
use clacks_mtproto::{BoxedDeserialize, BoxedSerialize, IntoBoxed, mtproto};
use futures::{Future, Sink, Stream, future};
use futures::prelude::*;
use rand::Rng;
use std::io;
use tokio_io::{AsyncRead, AsyncWrite};


struct ElispFormatter(sexpr::ser::CompactFormatter);

macro_rules! enquote_integer {
    ($ty:ident, $meth:ident) => {

        fn $meth<W: ?Sized>(&mut self, writer: &mut W, value: $ty) -> io::Result<()>
            where W: io::Write,
        {
            self.enquote(writer, |this, w| (this.0).$meth(w, value))
        }

    };
}

impl sexpr::ser::Formatter for ElispFormatter {
    enquote_integer!(u32, write_u32);
    enquote_integer!(i32, write_i32);
    enquote_integer!(u64, write_u64);
    enquote_integer!(i64, write_i64);

    fn begin_object_key<W: ?Sized>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
        where W: io::Write,
    {
        if first {
            writer.write_all(b"(")
        } else {
            writer.write_all(b" (")
        }
    }

    fn begin_object_value<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
        where W: io::Write,
    {
        writer.write_all(b". ")
    }

    fn end_object_value<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
        where W: io::Write,
    {
        writer.write_all(b")")
    }
}

impl ElispFormatter {
    fn to_string<S>(obj: &S) -> io::Result<String>
        where S: serde::Serialize,
    {
        let mut ret: Vec<u8> = vec![];
        obj.serialize(&mut sexpr::Serializer::with_formatter(&mut ret, ElispFormatter(sexpr::ser::CompactFormatter)))?;
        Ok(String::from_utf8(ret).unwrap())
    }

    fn enquote<W: ?Sized, F>(&mut self, writer: &mut W, func: F) -> io::Result<()>
        where W: io::Write, F: FnOnce(&mut Self, &mut W) -> io::Result<()>,
    {
        writer.write_all(b"\"")?;
        func(self, writer)?;
        writer.write_all(b"\"")?;
        Ok(())
    }
}

fn into_error<T>(x: T) -> clacks_transport::error::Error
    where T: Into<clacks_transport::error::Error>
{
    x.into()
}

fn u32_bytes(n: u32) -> mtproto::bytes {
    let mut ret = vec![0u8; 4];
    BigEndian::write_u32(&mut ret, n);
    ret.into()
}

#[async]
fn kex(log: slog::Logger) -> clacks_rpc::error::Result<()> {
    let executor = tokio::executor::current_thread::TaskExecutor::current();
    let socket: kabuki_extras::RealShutdownTcpStream = await!(
        tokio::net::TcpStream::connect(&"149.154.167.50:443".parse().unwrap()))?.into();
    let app_id = clacks_transport::session::AppId {
        api_id: 0,
        api_hash: "".into(),
    };
    let client = clacks_rpc::client::RpcClient::spawn(&executor, log, app_id, socket)?;
    let nonce = csrng_gen();
    let mtproto::ResPQ::ResPQ(pq) = await!(client.ask_plain(mtproto::rpc::ReqPq { nonce }))?;
    println!("got back: {:#?}", pq);
    assert_eq!(nonce, pq.nonce);
    let server_nonce = pq.server_nonce;
    let pq_int = byteorder::BigEndian::read_u64(&pq.pq);
    let (p, q) = clacks_crypto::asymm::decompose_pq(pq_int)?;
    let (pubkey, public_key_fingerprint) = clacks_crypto::asymm::find_first_key(&pq.server_public_key_fingerprints)?.unwrap();
    let new_nonce = csrng_gen();
    let inner = mtproto::p_q_inner_data::PQInnerData {
        nonce, new_nonce, server_nonce,
        p: u32_bytes(p), q: u32_bytes(q),
        pq: pq.pq,
    }.into_boxed();
    let aes = clacks_crypto::symm::AesParams::from_pq_inner_data(&inner)?;
    println!("inner: {:#?}", inner);
    let encrypted_data = pubkey.encrypt(&inner.boxed_serialized_bytes()?)?.into();
    let dh_req = mtproto::rpc::ReqDHParams {
        nonce, server_nonce, public_key_fingerprint, encrypted_data,
        p: u32_bytes(p), q: u32_bytes(q),
    };
    println!("dh_req: {:#?}", dh_req);
    let dh_params = await!(client.ask_plain(dh_req))?;
    println!("got back: {:#?}", dh_params);
    let dh_params = match dh_params {
        mtproto::ServerDHParams::Ok(x) => x,
        _ => unimplemented!(),
    };
    let decrypted = aes.ige_decrypt(&dh_params.encrypted_answer)?;
    let server_dh_data = {
        let (sha_part, data_part) = decrypted.split_at(20);
        mtproto::ServerDHInnerData::boxed_deserialized_from_bytes(&data_part)?
    };
    println!("dh data: {:#?}", server_dh_data);
    let (auth_key, g_b) = clacks_crypto::asymm::calculate_auth_key(&server_dh_data)?;
    let inner = mtproto::client_dh_inner_data::ClientDHInnerData {
        nonce, server_nonce, g_b,
        retry_id: 0,
    }.into_boxed();
    println!("inner: {:#?}", inner);
    let encrypted_data = aes.ige_encrypt(&inner.boxed_serialized_bytes()?, true)?.into();
    let set_dh = mtproto::rpc::SetClientDHParams {
        nonce, server_nonce, encrypted_data,
    };
    println!("set_dh: {:#?}", set_dh);
    let answer = await!(client.ask_plain(set_dh))?;
    println!("answer: {:#?}", answer);
    let expected_new_nonce_hash1 = auth_key.new_nonce_hash(1, new_nonce)?;
    match answer {
        mtproto::SetClientDHParamsAnswer::Ok(ref n) if n.new_nonce_hash1 == expected_new_nonce_hash1 => (),
        _ => unimplemented!(),
    }
    let mut new_salt = [0u8; 8];
    for ((loc, &a), &b) in new_salt.iter_mut().zip(&new_nonce[..8]).zip(&server_nonce[..8]) {
        *loc = a ^ b;
    }
    println!("results: {:?} {:?}", auth_key, new_salt);
    let now = chrono::Utc::now();
    let salt = clacks_transport::session::future_salt_from_negotiated_salt(LittleEndian::read_i64(&new_salt));
    let init = mtproto::rpc::InvokeWithLayer {
        layer: mtproto::LAYER,
        query: mtproto::rpc::InitConnection {
            api_id: 0,
            device_model: "test".into(),
            system_version: "test".into(),
            app_version: "0.0.1".into(),
            lang_code: "en".into(),
            system_lang_code: "en".into(),
            lang_pack: "".into(),
            query: mtproto::rpc::help::GetConfig,
        }
    };
    let send_code = mtproto::rpc::auth::SendCode {
        allow_flashcall: false,
        phone_number: "".into(),
        current_number: None,
        api_id: 0,
        api_hash: "".to_string(),
    };
    let answer = await!(client.ask(send_code))?;
    println!("answer: {:#?}", answer);
    Ok(())
}

fn main() {
    use futures::{Future, Stream};
    use slog::Drain;

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_envlogger::new(drain);
    let drain = slog_async::Async::new(drain).build().fuse();
    let log = slog::Logger::root(drain, o!());
    let _scoped = slog_scope::set_global_logger(log.new(o!("subsystem" => "implicit logger")));

    let mut runtime = tokio::runtime::current_thread::Runtime::new().unwrap();
    runtime.spawn({
        kex(log)
            .map_err(|e| panic!("fatal {:?}", e))
    });
    runtime.run().unwrap();
}
