//use actix::{self, Actor, Addr, Arbiter, AsyncContext, Context, StreamHandler, System};
use actix::prelude::*;
use failure::Error;
use futures::{Future, IntoFuture};
use slog::Logger;
use std::io;
use tokio_codec::{FramedRead, LinesCodec};
use tokio_io;


pub type JsonRpcAddr = Addr<::jsonrpc_handler::JsonRpcHandlerActor>;

pub struct AgentActor {
    tx: Option<actix::io::FramedWrite<Box<tokio_io::AsyncWrite>, LinesCodec>>,
    rx: actix::SpawnHandle,
    jsonrpc: JsonRpcAddr,
}

impl AgentActor {
    pub fn from_context<S>(ctx: &mut Context<Self>, log: Logger, stream: S, jsonrpc: JsonRpcAddr) -> Self
        where S: tokio_io::AsyncRead + tokio_io::AsyncWrite + 'static,
    {
        let (rx, tx) = stream.split();
        let rx = ctx.add_stream2(FramedRead::new(rx, LinesCodec::new()));
        let tx: Box<tokio_io::AsyncWrite> = Box::new(tx);
        let tx = Some(actix::io::FramedWrite::new(tx, LinesCodec::new(), ctx));
        AgentActor {
            rx, tx, jsonrpc,
        }
    }
}

impl Actor for AgentActor {
    type Context = Context<Self>;
}

impl actix::io::WriteHandler<io::Error> for AgentActor {

}

impl StreamHandler2<String, io::Error> for AgentActor {
    fn handle(&mut self, input: io::Result<Option<String>>, ctx: &mut Self::Context) {
        let input = match input {
            Ok(Some(s)) => Ok(s),
            Err(e) => Err(e),
            Ok(None) => return,
        };
        ctx.spawn({
            input.map_err(|e| -> Error { e.into() })
                .and_then(|line| {
                    ::serde_json::from_str(&line)
                        .map_err(|e| -> Error { e.into() })
                })
                .map(|request: ::jsonrpc_core::Request| {
                    self.jsonrpc.send(::jsonrpc_handler::HandleRequest { request })
                        .map_err(|e| -> Error { e.into() })
                })
                .into_future()
                .and_then(|f| f)
                .into_actor(self)
                .then(|r, this, ctx| {
                    println!("got: {:#?}", r);
                    match r {
                        Ok(Ok(Some(resp))) => {
                            if let Some(ref mut tx) = this.tx {
                                // XXX
                                let out = ::serde_json::to_string(&resp).unwrap();
                                tx.write(out);
                            }
                        },
                        Ok(Ok(None)) |
                        Ok(Err(())) => (),
                        Err(e) => {
                            println!("agent failed: {:#?}", e);
                        },
                    }
                    actix::fut::ok(())
                })
        });
    }
}

// pub struct SetDelegates {
//     pub delegates: EventDelegates,
// }

// impl Message for SetDelegates {
//     type Result = ();
// }

// impl Handler<SetDelegates> for RpcClientActor {
//     type Result = ();

//     fn handle(&mut self, delegates: SetDelegates, _: &mut Self::Context) {
//         self.delegates = delegates.delegates;
//     }
// }
