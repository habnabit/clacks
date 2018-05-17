#![feature(proc_macro, generators)]

extern crate byteorder;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate clacks_transport;
extern crate futures_await as futures;
extern crate rand;
extern crate tokio;
extern crate tokio_io;

use byteorder::{BigEndian, ByteOrder, LittleEndian};
use clacks_mtproto::{BoxedDeserialize, BoxedSerialize, IntoBoxed, mtproto};
use futures::{Future, Sink, Stream, future};
use futures::prelude::*;
use rand::Rng;
use tokio_io::{AsyncRead, AsyncWrite};


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
        println!("downcast: {:?}", parsed.downcast::<mtproto::manual::MessageContainer>());
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
    let (framed, mtproto::ResPQ::ResPQ(pq)) = await!(framed.send_and_await_reply(mtproto::rpc::req_pq { nonce }))?;
    println!("got back: {:#?}", pq);
    assert_eq!(nonce, pq.nonce);
    let server_nonce = pq.server_nonce;
    let pq_int = byteorder::BigEndian::read_u64(&pq.pq);
    let (p, q) = clacks_crypto::asymm::decompose_pq(pq_int)?;
    let (pubkey, public_key_fingerprint) = clacks_crypto::asymm::find_first_key(&pq.server_public_key_fingerprints)?.unwrap();
    let new_nonce = rng.gen();
    let inner = mtproto::p_q_inner_data::P_Q_inner_data { 
        nonce, new_nonce, server_nonce,
        p: u32_bytes(p), q: u32_bytes(q),
        pq: pq.pq,
    };
    let aes = clacks_crypto::symm::AesParams::from_pq_inner_data(&inner)?;
    println!("inner: {:#?}", inner);
    let encrypted_data = pubkey.encrypt(
        &inner.into_boxed().boxed_serialized_bytes()?)?.into();
    let dh_req = mtproto::rpc::req_DH_params {
        nonce, server_nonce, public_key_fingerprint, encrypted_data,
        p: u32_bytes(p), q: u32_bytes(q),
    };
    println!("dh_req: {:#?}", dh_req);
    let (framed, dh_params) = await!(framed.send_and_await_reply(dh_req))?;
    println!("got back: {:#?}", dh_params);
    let dh_params = match dh_params {
        mtproto::Server_DH_Params::ok(x) => x,
        _ => unimplemented!(),
    };
    let decrypted = aes.ige_decrypt(&dh_params.encrypted_answer)?;
    let mtproto::Server_DH_inner_data::Server_DH_inner_data(server_dh_data) = {
        let (sha_part, data_part) = decrypted.split_at(20);
        mtproto::Server_DH_inner_data::boxed_deserialized_from_bytes(&data_part)?
    };
    println!("dh data: {:#?}", server_dh_data);
    let (auth_key, g_b) = clacks_crypto::asymm::calculate_auth_key(&server_dh_data)?;
    let inner = mtproto::client_DH_inner_data::Client_DH_Inner_Data {
        nonce, server_nonce, g_b,
        retry_id: 0,
    }.into_boxed();
    println!("inner: {:#?}", inner);
    let encrypted_data = aes.ige_encrypt(&inner.boxed_serialized_bytes()?, true)?.into();
    let set_dh = mtproto::rpc::set_client_DH_params {
        nonce, server_nonce, encrypted_data,
    };
    println!("set_dh: {:#?}", set_dh);
    let (mut framed, answer) = await!(framed.send_and_await_reply(set_dh))?;
    println!("answer: {:#?}", answer);
    let expected_new_nonce_hash1 = auth_key.new_nonce_hash(1, new_nonce)?;
    match answer {
        mtproto::Set_client_DH_params_answer::ok(ref n) if n.new_nonce_hash1 == expected_new_nonce_hash1 => (),
        _ => unimplemented!(),
    }
    let mut new_salt = [0u8; 8];
    for ((loc, &a), &b) in new_salt.iter_mut().zip(&new_nonce[..8]).zip(&server_nonce[..8]) {
        *loc = a ^ b;
    }
    println!("results: {:?} {:?}", auth_key, new_salt);
    let now = chrono::Utc::now();
    let salt = mtproto::future_salt::FutureSalt {
        salt: LittleEndian::read_i64(&new_salt),
        valid_since: now.timestamp() as i32,
        valid_until: (now + chrono::Duration::minutes(10)).timestamp() as i32,
    }.into_boxed();
    framed.session.adopt_key(auth_key);
    framed.session.add_server_salts(std::iter::once(salt));
    println!("session: {:#?}", framed.session);
    let init = mtproto::rpc::invokeWithLayer {
        layer: mtproto::LAYER,
        query: mtproto::rpc::initConnection {
            api_id: 0, 
            device_model: "test".into(),
            system_version: "test".into(),
            app_version: "0.0.1".into(),
            lang_code: "en".into(),
            system_lang_code: "en".into(),
            lang_pack: "".into(),
            query: mtproto::rpc::help::getConfig,
        }
    };
    let send_code = mtproto::rpc::auth::sendCode {
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
