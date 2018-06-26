//use actix::{self, Actor, Addr, Arbiter, AsyncContext, Context, StreamHandler, System};
use actix::prelude::*;
use clacks_mtproto::mtproto;
use clacks_rpc::client::{self, RpcClientActor, SendMessage};
use failure::Error;
use futures::prelude::*;
use futures::{Future, IntoFuture};
use slog::Logger;
use std::collections::BTreeMap;
use std::io;
use tokio_codec::{FramedRead, LinesCodec};
use tokio_io;

use secrets::{self, AppKeyV1};


#[derive(Debug, Clone, Fail)]
#[fail(display = "")]
pub struct NoAppKey;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UserDc {
    phone_number: String,
    dc: i32,
    primary: bool,
}

pub struct TelegramManagerActor {
    log: Logger,
    connections: BTreeMap<UserDc, Addr<RpcClientActor>>,
    app_key: Option<AppKeyV1>,
}

impl TelegramManagerActor {
    pub fn new(log: Logger) -> Self {
        TelegramManagerActor {
            log,
            connections: BTreeMap::new(),
            app_key: None,
        }
    }

    fn app_key_ref(&self) -> Result<&AppKeyV1, Error> {
        self.app_key.as_ref().ok_or_else(|| NoAppKey.into())
    }
}

impl Actor for TelegramManagerActor {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Self::Context) {
        self.app_key = secrets::Entry::new(secrets::AppKey).get().unwrap();
    }
}

pub struct Connect {
    pub phone_number: String,
}

impl Message for Connect {
    type Result = ();
}

impl Handler<Connect> for TelegramManagerActor {
    type Result = ();

    fn handle(&mut self, req: Connect, ctx: &mut Self::Context) {
        let log = self.log.new(o!());
        let app_id_res = self.app_key_ref().map(|k| k.as_app_id());
        ctx.spawn(async_block! {
            let app_id = app_id_res?;
            let socket: ::real_shutdown::RealShutdown<::tokio::net::TcpStream> = await!(
                ::tokio::net::TcpStream::connect(&"149.154.167.50:443".parse().unwrap()))?.into();
            let client = RpcClientActor::create({
                let app_id = app_id.clone();
                move |ctx| RpcClientActor::from_context(ctx, log, app_id, socket)
            });
            let delegate = Delegate.start();
            await!(client.send(client::SetDelegates {
                delegates: client::EventDelegates {
                    unhandled: Some(delegate.recipient()),
                },
            }))?;
            let user = secrets::Entry::new(secrets::UserAuthKey { phone_number: req.phone_number.clone() });
            match user.get()? {
                Some(k) => {
                    println!("got back a key {:?}", k);
                    let perm_key = ::clacks_crypto::symm::AuthKey::new(&k.auth_key)?;
                    await!(::clacks_rpc::kex::adopt_auth_key(
                        client.clone(), ::futures_cpupool::CpuPool::new(1), ::chrono::Duration::hours(24),
                        perm_key))?;
                },
                None => {
                    let perm_key = await!(::clacks_rpc::kex::new_auth_key(
                        client.clone(), ::futures_cpupool::CpuPool::new(1), ::chrono::Duration::hours(24)))?;
                    user.set(&secrets::UserAuthKeyV1 { auth_key: (&perm_key.into_inner()[..]).into() })?;
                },
            }
            let init = mtproto::rpc::InvokeWithLayer {
                layer: mtproto::LAYER,
                query: mtproto::rpc::InitConnection {
                    api_id: app_id.api_id,
                    device_model: "test".into(),
                    system_version: "test".into(),
                    app_version: "0.0.1".into(),
                    lang_code: "en".into(),
                    system_lang_code: "en".into(),
                    lang_pack: "".into(),
                    query: mtproto::rpc::help::GetConfig,
                }
            };
            let config = await!(client.send(client::CallFunction::encrypted(init)))??;
            Ok((UserDc {
                phone_number: req.phone_number,
                dc: *config.this_dc(),
                primary: true,
            }, client))
        }.into_actor(self).then(|r: Result<_, Error>, this, _ctx| {
            println!("connect: {:?}", r.as_ref().err());
            if let Ok((user_dc, client)) = r {
                this.connections.insert(user_dc, client);
            }
            actix::fut::ok(())
        }));
    }
}

impl Handler<SendMessage> for TelegramManagerActor {
    type Result = ResponseFuture<mtproto::TLObject, Error>;

    fn handle(&mut self, req: SendMessage, ctx: &mut Self::Context) -> Self::Result {
        Box::new({
            self.connections.values().next()
                .ok_or_else(|| format_err!("no connection"))
                .map(|c| {
                    c.send(req).map_err(|e| -> Error { e.into() })
                })
                .into_future()
                .and_then(|f| f)
                .and_then(|r| r)
        })
    }
}

struct Delegate;

impl Handler<client::Unhandled> for Delegate {
    type Result = ();

    fn handle(&mut self, unhandled: client::Unhandled, _: &mut Self::Context) {
        println!("unhandled {:?}", unhandled.0);
        println!("---json---\n{}\n---", ::serde_json::to_string_pretty(&unhandled.0).expect("not serialized"));
    }
}

impl Actor for Delegate {
    type Context = Context<Self>;
}
