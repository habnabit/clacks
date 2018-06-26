#![warn(unused_extern_crates)]
#![feature(proc_macro, proc_macro_non_items, generators)]

#[macro_use] extern crate delegate;
#[macro_use] extern crate failure;
#[macro_use] extern crate jsonrpc_macros;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate slog;
extern crate actix;
extern crate byteorder;
extern crate bytes;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate clacks_rpc;
extern crate clacks_transport;
extern crate futures_await as futures;
extern crate futures_cpupool;
extern crate jsonrpc_core;
extern crate rand;
extern crate keyring;
extern crate serde;
extern crate serde_json;
extern crate slog_async;
extern crate slog_envlogger;
extern crate slog_scope;
extern crate slog_term;
extern crate tokio;
extern crate tokio_codec;
extern crate tokio_io;
extern crate tokio_uds;

use actix::prelude::*;
use clacks_mtproto::{BoxedDeserialize, BoxedSerialize, IntoBoxed, mtproto};
use futures::{Future, Sink, Stream, future};
use futures::prelude::*;
use std::io;

mod real_shutdown;
use real_shutdown::RealShutdown;

mod agent_connection;
mod jsonrpc_handler;
mod secrets;
mod tg_manager;


struct Delegate;

impl Handler<clacks_rpc::client::Unhandled> for Delegate {
    type Result = ();

    fn handle(&mut self, unhandled: clacks_rpc::client::Unhandled, _: &mut Self::Context) {
        println!("unhandled {:?}", unhandled.0);
        println!("---json---\n{}\n---", serde_json::to_string_pretty(&unhandled.0).expect("not serialized"));
    }
}

impl Actor for Delegate {
    type Context = Context<Self>;
}

fn kex(log: slog::Logger) -> impl Future<Item = (), Error = failure::Error> { async_block! {
    let app_key = read_app_key()?;
    let socket: RealShutdown<tokio::net::TcpStream> = await!(
        tokio::net::TcpStream::connect(&"149.154.167.50:443".parse().unwrap()))?.into();
    let delegate = Delegate.start();
    let app_id = app_key.as_app_id();
    let client = clacks_rpc::client::RpcClientActor::create(|ctx| {
        clacks_rpc::client::RpcClientActor::from_context(ctx, log, app_id, socket)
    });
    await!(client.send(clacks_rpc::client::SetDelegates {
        delegates: clacks_rpc::client::EventDelegates {
            unhandled: Some(delegate.recipient()),
        },
    }))?;
    let perm_key = await!(clacks_rpc::kex::new_auth_key(
        client.clone(), futures_cpupool::CpuPool::new(1), chrono::Duration::hours(24)))?;
    println!("perm_key: {:?}", perm_key);
    let init = mtproto::rpc::InvokeWithLayer {
        layer: mtproto::LAYER,
        query: mtproto::rpc::InitConnection {
            api_id: app_key.api_id,
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
        api_id: app_key.api_id,
        api_hash: app_key.api_hash.clone(),
    };
    let answer = await!(client.send(clacks_rpc::client::SendMessage::encrypted(init)))??;
    println!("answer: {:#?}", answer);
    println!("---json---\n{}\n---", serde_json::to_string_pretty(&answer).expect("not serialized"));
    let answer = await!(client.send(clacks_rpc::client::SendMessage::encrypted(send_code)))??;
    Ok(())
}}

fn read_app_key() -> Result<secrets::AppKeyV1, failure::Error> {
    let entry = secrets::Entry::new(secrets::AppKey);
    entry.get()
        .and_then(|opt| {
            opt.map(Ok)
                .unwrap_or_else(|| {
                    let value = secrets::AppKeyV1 {
                        api_id: 0,
                        api_hash: "".to_string(),
                    };
                    entry.set(&value)?;
                    Ok(value)
                })
        })
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

    System::run(move || {
        let tg_manager = {
            let log = log.new(o!("subsystem" => "tg manager"));
            tg_manager::TelegramManagerActor::new(log).start()
        };
        let jsonrpc = {
            let log = log.new(o!("subsystem" => "jsonrpc handler"));
            jsonrpc_handler::JsonRpcHandlerActor::new(log, tg_manager).start()
        };
        Arbiter::spawn({
            let log = log.clone();
            tokio_uds::UnixListener::bind("socket")
                .into_future()
                .and_then(|l| l.incoming().for_each(move |conn| {
                    let log = log.new(o!("connection" => format!("{:?}", conn)));
                    let jsonrpc = jsonrpc.clone();
                    info!(log, "spawning");
                    let addr = agent_connection::AgentActor::create(|ctx| {
                        agent_connection::AgentActor::from_context(ctx, log, conn, jsonrpc)
                    });
                    Ok(())
                }))
                .map_err(|e| panic!("fatal {:?}", e))
        });
    });
}
