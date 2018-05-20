#![feature(proc_macro, generators)]

extern crate byteorder;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate clacks_transport;
extern crate futures_await as futures;
extern crate rand;
extern crate serde;
extern crate serde_json;
extern crate sexpr;
extern crate tokio;
extern crate tokio_io;

use byteorder::{BigEndian, ByteOrder, LittleEndian};
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

struct TelegramFramed<S>
    where S: AsyncRead + AsyncWrite + 'static
{
    framed: tokio_io::codec::Framed<S, clacks_transport::TelegramCodec>,
    session: clacks_transport::session::Session,
}

impl<S> TelegramFramed<S>
    where S: AsyncRead + AsyncWrite + 'static
{
    fn new(session_id: i64, stream: S) -> Self {
        let framed = stream.framed(clacks_transport::TelegramCodec::new());
        let session = clacks_transport::session::Session::new(session_id, clacks_transport::session::AppId {
            api_id: 0,
            api_hash: "".into(),
        });
        TelegramFramed { framed, session }
    }

    #[async]
    fn send_and_await_reply<R>(mut self, request: R) -> clacks_transport::error::Result<(Self, R::Reply)>
        where R: clacks_mtproto::Function + 'static,
    {
        let payload = self.session.plain_payload(request)?;
        let mut framed = await!(self.framed.send(payload.message))?;
        let received = await_item!(framed)?.unwrap();
        let processed = self.session.process_message(&received)?;
        let parsed = R::Reply::boxed_deserialized_from_bytes(&processed.payload)?;
        Ok((TelegramFramed { framed, session: self.session }, parsed))
    }

    #[async]
    fn send_and_await_reply_enc<R>(mut self, request: R) -> clacks_transport::error::Result<(Self, R::Reply)>
        where R: clacks_mtproto::Function + 'static,
    {
        let payload = self.session.encrypted_payload(request)?;
        let mut framed = await!(self.framed.send(payload.message))?;
        let received = await_item!(framed)?.unwrap();
        let processed = self.session.process_message(&received)?;
        let parsed = mtproto::TLObject::boxed_deserialized_from_bytes(&processed.payload)?;
        println!("got: {:?}", parsed);
        let result = parsed.downcast::<mtproto::manual::MessageContainer>().expect("not a container");
        println!("downcast: {:?}", result);
        println!("---sexpr---\n{}\n", ElispFormatter::to_string(&result).expect("not serialized"));
        println!("---json---\n{}\n---", serde_json::to_string_pretty(&result).expect("not serialized"));
        panic!();
        //Ok((TelegramFramed { framed, session: self.session }, parsed))
    }
}

fn u32_bytes(n: u32) -> mtproto::bytes {
    let mut ret = vec![0u8; 4];
    BigEndian::write_u32(&mut ret, n);
    ret.into()
}

#[async]
fn kex<S>(stream: S) -> clacks_transport::error::Result<()>
    where S: AsyncRead + AsyncWrite + 'static,
{
    let mut rng = rand::OsRng::new().unwrap();
    let framed = TelegramFramed::new(rng.gen(), stream);
    let nonce = rng.gen();
    let (framed, mtproto::ResPQ::ResPQ(pq)) = await!(framed.send_and_await_reply(mtproto::rpc::ReqPq { nonce }))?;
    println!("got back: {:#?}", pq);
    assert_eq!(nonce, pq.nonce);
    let server_nonce = pq.server_nonce;
    let pq_int = byteorder::BigEndian::read_u64(&pq.pq);
    let (p, q) = clacks_crypto::asymm::decompose_pq(pq_int)?;
    let (pubkey, public_key_fingerprint) = clacks_crypto::asymm::find_first_key(&pq.server_public_key_fingerprints)?.unwrap();
    let new_nonce = rng.gen();
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
    let (framed, dh_params) = await!(framed.send_and_await_reply(dh_req))?;
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
    let (mut framed, answer) = await!(framed.send_and_await_reply(set_dh))?;
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
    framed.session.adopt_key(auth_key);
    framed.session.add_server_salts(std::iter::once(salt));
    println!("session: {:#?}", framed.session);
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
    let (framed, answer) = await!(framed.send_and_await_reply_enc(send_code))?;
    println!("answer: {:#?}", answer);
    Ok(())
}

fn main_future() -> Box<Future<Item = (), Error = ()> + Send> {
    let fut = tokio::net::TcpStream::connect(&"149.154.167.50:443".parse().unwrap())
        .map_err(into_error)
        .and_then(kex)
        .map_err(|e| panic!("error: {:?}", e));
    Box::new(fut)
}

fn main() {
    tokio::run(main_future())
}
