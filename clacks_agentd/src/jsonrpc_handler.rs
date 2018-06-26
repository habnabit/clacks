use actix::prelude::*;
use clacks_mtproto::mtproto;
use failure::Error;
use futures::{Future, IntoFuture};
use jsonrpc_core::{self, MetaIoHandler, NoopMiddleware};
use slog::Logger;
use std::sync::Mutex;


pub struct JsonRpcHandlerActor {
    handler: MetaIoHandler<(), NoopMiddleware>,
}

impl JsonRpcHandlerActor {
    pub fn new(log: Logger, tg_manager: Addr<::tg_manager::TelegramManagerActor>) -> Self {
		use self::rpc_trait::Rpc;
		let mut handler = MetaIoHandler::default();
		handler.extend_with(RpcImpl::new(tg_manager).to_delegate());
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SendRawFailure;
type JsonRpcResponseFuture<T> = Box<Future<Item = T, Error = jsonrpc_core::Error> + Send>;

mod rpc_trait {
	use clacks_mtproto::mtproto::TLObject;
	use jsonrpc_core::Result;
	use super::{JsonRpcResponseFuture, SendRawFailure};

	build_rpc_trait! {
		pub trait Rpc {
			#[rpc(name = "connect")]
			fn connect(&self, String) -> JsonRpcResponseFuture<()>;

			#[rpc(name = "hazmat.send_raw")]
			fn send_raw(&self, TLObject) -> JsonRpcResponseFuture<::std::result::Result<TLObject, SendRawFailure>>;
		}
	}
}

struct InnerRpcImpl {
	tg_manager: Addr<::tg_manager::TelegramManagerActor>,
}

struct RpcImpl {
	inner: Mutex<InnerRpcImpl>,
}

impl RpcImpl {
	fn new(tg_manager: Addr<::tg_manager::TelegramManagerActor>) -> Self {
		RpcImpl {
			inner: Mutex::new(InnerRpcImpl { tg_manager }),
		}
	}
}

impl rpc_trait::Rpc for RpcImpl {
	fn connect(&self, phone_number: String) -> JsonRpcResponseFuture<()> {
		Box::new({
			self.inner.lock()
				.map_err(|e| format_err!("failed to lock"))
				.map(|i| i.tg_manager.send(::tg_manager::Connect { phone_number }))
				.into_future()
				.and_then(|f| f.map_err(|e| -> Error { e.into() }))
				.map_err(|e: Error| {
					println!("failed? {:?}", e);
					jsonrpc_core::Error::internal_error()
				})
		})
	}

	fn send_raw(&self, req: mtproto::TLObject) -> JsonRpcResponseFuture<::std::result::Result<mtproto::TLObject, SendRawFailure>>
	{
		Box::new({
			self.inner.lock()
				.map_err(|e| format_err!("failed to lock"))
				.map(|i| i.tg_manager.send(::clacks_rpc::client::SendMessage::encrypted(req)))
				.into_future()
				.and_then(|f| f.map_err(|e| -> Error { e.into() }))
				.map(|r| r.map_err(|e| {
					println!("failed? {:?}", e);
					SendRawFailure
				}))
				.map_err(|e| {
					println!("failed? {:?}", e);
					jsonrpc_core::Error::internal_error()
				})
		})
	}
}
