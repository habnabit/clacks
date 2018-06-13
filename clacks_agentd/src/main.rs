#![warn(unused_extern_crates)]
#![feature(proc_macro, proc_macro_non_items, generators)]

#[macro_use] extern crate slog;

extern crate byteorder;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate clacks_rpc;
extern crate clacks_transport;
extern crate failure;
extern crate futures_await as futures;
extern crate futures_cpupool;
extern crate kabuki;
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
use kabuki_extras::ext_traits::*;
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
    fn write_null<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
        where W: io::Write,
    {
        writer.write_all(b"nil")
    }

    fn write_bool<W: ?Sized>(&mut self, writer: &mut W, value: bool) -> io::Result<()>
        where W: io::Write,
    {
        writer.write_all(if value {b"t"} else {b"nil"})
    }

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


struct Delegate;

impl kabuki::Actor for Delegate {
    type Request = clacks_rpc::client::Event;
    type Response = ();
    type Future = BoxFuture<()>;

    fn call(&mut self, request: Self::Request) -> Self::Future {
        use clacks_rpc::client::Event::*;
        println!("event {:?}", request);
        match request {
            Unhandled(o) => {
                println!("---sexpr---\n{}\n", ElispFormatter::to_string(&o).expect("not serialized"));
                println!("---json---\n{}\n---", serde_json::to_string_pretty(&o).expect("not serialized"));
            },
            _ => (),
        }
        Box::new(future::ok(()))
    }
}

fn kex(log: slog::Logger) -> impl FailureFuture<()> { async_block! {
    let executor = tokio::executor::current_thread::TaskExecutor::current();
    let socket: kabuki_extras::RealShutdown<tokio::net::TcpStream> = await!(
        tokio::net::TcpStream::connect(&"149.154.167.50:443".parse().unwrap()))?.into();
    let app_id = clacks_transport::session::AppId {
        api_id: 0,
        api_hash: "".into(),
    };
    let client = clacks_rpc::client::RpcClient::spawn(&executor, log, app_id, socket)?;
    let delegate = kabuki::Builder::new().spawn(&executor, Delegate)?;
    let () = await!(client.set_delegate(kabuki_extras::ErasedService::new_erased(delegate)))?;
    let perm_key = await!(clacks_rpc::kex::new_auth_key(
        client.clone(), futures_cpupool::CpuPool::new(1), chrono::Duration::hours(24)))?;
    println!("perm_key: {:?}", perm_key);
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
    let answer = await!(client.ask(init))?;
    println!("answer: {:#?}", answer);
    println!("---sexpr---\n{}\n", ElispFormatter::to_string(&answer).expect("not serialized"));
    println!("---json---\n{}\n---", serde_json::to_string_pretty(&answer).expect("not serialized"));
    let answer = await!(client.ask(send_code))?;
    Ok(())
}}

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
