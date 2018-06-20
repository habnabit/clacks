use actix::{self, Actor, Addr, Arbiter, AsyncContext, Context, StreamHandler, System};
use actix::prelude::*;
use chrono::{Duration, Utc};
use clacks_crypto::symm::AuthKey;
use clacks_mtproto::{AnyBoxedSerialize, BoxedDeserialize, ConstructorNumber, mtproto};
use clacks_transport::{AppId, Session, TelegramCodec, session};
use failure::Error;
use futures::{self, Future, IntoFuture, Sink, Stream, future, stream};
use futures::unsync::oneshot;
use jsonrpc_core::{self, FutureResult, Metadata, MetaIoHandler, Middleware, NoopMiddleware};
use slog::Logger;
use std::io;
use std::collections::BTreeMap;
use std::marker::PhantomData;
use tokio_codec::{FramedRead, LinesCodec};
use tokio_io;


pub struct JsonRpcHandlerActor {
    handler: MetaIoHandler<(), NoopMiddleware>,
}

impl JsonRpcHandlerActor {
    pub fn new(log: Logger) -> Self {
		use self::rpc_trait::Rpc;
		let mut handler = MetaIoHandler::default();
		handler.extend_with(RpcImpl.to_delegate());
		JsonRpcHandlerActor {
			handler,
		}
    }
}

impl Actor for JsonRpcHandlerActor {
    type Context = Context<Self>;
}

pub struct HandleRequest {
    pub request: jsonrpc_core::Request,
}

impl Message for HandleRequest {
    type Result = Result<Option<jsonrpc_core::Response>, ()>;
}

impl Handler<HandleRequest> for JsonRpcHandlerActor {
    type Result = ResponseFuture<Option<jsonrpc_core::Response>, ()>;

    fn handle(&mut self, request: HandleRequest, _: &mut Self::Context) -> Self::Result {
		Box::new({
			self.handler.handle_rpc_request(request.request, ())
		})
    }
}

type JsonRpcResponseFuture<T> = Box<Future<Item = T, Error = jsonrpc_core::Error> + Send + Sync>;

mod rpc_trait {
	use jsonrpc_core::{Error, FutureResult, Result};
	use actix::ResponseFuture;
	use super::JsonRpcResponseFuture;

	build_rpc_trait! {
		pub trait Rpc {
			/// Get One type.
			#[rpc(name = "getOne")]
			fn one(&self) -> Result<u64>;

			/// Adds two numbers and returns a result
			#[rpc(name = "setTwo")]
			fn set_two(&self, String) -> Result<()>;

			/// Performs asynchronous operation
			#[rpc(name = "beFancy")]
			fn call(&self, u64) -> JsonRpcResponseFuture<(u64, String)>;
		}
	}
}

struct RpcImpl;

impl rpc_trait::Rpc for RpcImpl {
	fn one(&self) -> jsonrpc_core::Result<u64> {
		Ok(100)
	}

	fn set_two(&self, x: String) -> jsonrpc_core::Result<()> {
		println!("{}", x);
		Ok(())
	}

	fn call(&self, num: u64) -> JsonRpcResponseFuture<(u64, String)> {
		Box::new({
			Ok((num + 999, "hello".into())).into_future()
		})
	}
}
