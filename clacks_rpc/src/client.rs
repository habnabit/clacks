use chrono::{Duration, Utc};
use clacks_crypto::symm::AuthKey;
use clacks_mtproto::{BoxedDeserialize, ConstructorNumber, mtproto};
use clacks_transport::{AppId, Session, TelegramCodec, session};
use futures::{Future, IntoFuture, Sink, Stream, future, stream};
use futures::unsync::oneshot;
use kabuki::{self, Actor, ActorRef, ActorRefOf};
use kabuki_extras::{
    BoxService, ErasedService, SinkActor, StreamConsumerActorFeeder, StreamEnded};
use kabuki_extras::ext_traits::*;
use slog::Logger;
use std::io;
use std::collections::BTreeMap;
use tokio_io::{AsyncRead, AsyncWrite};
use tokio_service::Service;

use error::{self, BoxFuture, FutureExtUnit, LocalFuture, Result};


#[derive(Debug)]
pub enum Event {
    Updates(mtproto::Updates),
    Unhandled(mtproto::TLObject),
    ConnectionClosing,
}

pub type EventDelegate = BoxService<Event, (), ()>;
type Responder = oneshot::Sender<error::Result<mtproto::TLObject>>;

struct RpcClientActor {
    session: Session,
    tg_tx: Option<ActorRef<Vec<u8>, (), error::Error>>,
    sink_error_rx: Option<oneshot::Receiver<error::Error>>,
    delegate: EventDelegate,
    pending_rpcs: BTreeMap<i64, (ConstructorNumber, Responder)>,
}

#[derive(Clone)]
pub struct RpcClient {
    actor_tx: ActorRefOf<RpcClientActor>,
}

pub(crate) enum InternalEvent {
    Inbound(Vec<u8>),
    Outbound(session::EitherMessageBuilder, Responder),
    ConnectionClosed,
    SetDelegate(EventDelegate),
    BindAuthKey {
        perm_key: AuthKey,
        temp_key: AuthKey,
        temp_key_duration: Duration,
        salt: mtproto::FutureSalt,
    },
}

impl From<Vec<u8>> for InternalEvent {
    fn from(v: Vec<u8>) -> Self {
        InternalEvent::Inbound(v)
    }
}

impl From<StreamEnded<io::Error>> for InternalEvent {
    fn from(e: StreamEnded<io::Error>) -> Self {
        InternalEvent::ConnectionClosed
    }
}

impl RpcClientActor {
    fn spawn<E, S>(executor: &E, log: Logger, app_id: AppId, stream: S) -> Result<ActorRefOf<RpcClientActor>>
        where E: future::Executor<Box<Future<Item = (), Error = ()>>>,
              S: AsyncRead + AsyncWrite + 'static,
    {
        let session = Session::new(app_id);
        let (tg_tx, tg_rx) = stream.framed(TelegramCodec::new()).split();
        let sink = SinkActor::new(log.clone(), tg_tx.sink_map_err(error::into_error));
        let tg_tx = kabuki::Builder::new().spawn(executor, sink.actor)?;
        let actor = StreamConsumerActorFeeder::new(log, tg_rx, RpcClientActor {
            session,
            tg_tx: Some(tg_tx),
            sink_error_rx: Some(sink.error_rx),
            delegate: ErasedService::new_erased(kabuki::null()),
            pending_rpcs: BTreeMap::new(),
        }, 5);
        Ok(kabuki::Builder::new().spawn(executor, actor)?)
    }

    fn process_message(&mut self, vec: Vec<u8>) -> <Self as Actor>::Future {
        let message = match self.session.process_message(&vec) {
            Ok(m) => m,
            Err(e) => return Box::new(future::err(e.into())),
        };
        let payload = mtproto::TLObject::boxed_deserialized_from_bytes(&message.payload)
            .map_err(error::into_error);
        if message.was_encrypted {
            self.maybe_ack(message.seq_no, message.message_id);
            return Box::new({
                future::result(payload.map(|o| self.scan_replies(o)))
                    .and_then(|f| f)
            });
        } else if self.pending_rpcs.len() != 1 {
            // XXX: can't dispatch this message
        } else {
            let key = *self.pending_rpcs.keys().next().unwrap();
            let (_, sender) = self.pending_rpcs.remove(&key).unwrap();
            let _ = sender.send(payload);
        }
        Box::new(future::ok(()))
    }

    fn maybe_ack(&mut self, seq_no: Option<i32>, message_id: i64) {
        match seq_no {
            Some(s) if s & 1 != 0 => self.session.ack_id(message_id),
            _ => (),
        }
    }

    fn send_message(&mut self, message: session::EitherMessageBuilder, responder: Responder) -> <Self as Actor>::Future {
        use std::collections::btree_map::Entry::*;
        let tg_tx = match self.tg_tx {
            Some(ref c) => c,
            None => {
                let _ = responder.send(Err(error::ErrorKind::ConnectionClosed.into()));
                return Box::new(future::ok(()));
            },
        };
        match self.pending_rpcs.entry(message.message_id()) {
            Vacant(e) => {
                e.insert((message.constructor(), responder));
            },
            Occupied(_) => {
                let _ = responder.send(Err(error::ErrorKind::DuplicateMessageId.into()));
                return Box::new(future::ok(()));
            },
        }
        Box::new({
            self.session.serialize_message(message)
                .map_err(error::into_error)
                .map(|v| tg_tx.call(v).map_err(error::into_error))
                .into_future()
                .and_then(|f| f)
        })
    }

    fn close_connection(&mut self) -> <EventDelegate as Service>::Future {
        self.tg_tx = None;
        self.sink_error_rx = None;
        self.delegate.call(Event::ConnectionClosing)
    }

    fn bind_auth_key(&mut self, perm_key: AuthKey, temp_key: AuthKey, temp_key_duration: Duration, salt: mtproto::FutureSalt) -> <Self as Actor>::Future {
        self.session.adopt_key(temp_key);
        self.session.add_server_salts(::std::iter::once(salt));
        let (tx, rx) = oneshot::channel();
        let bound = self.session.bind_auth_key(perm_key, temp_key_duration)
            .map(|message| self.send_message(message.lift(), tx));
        Box::new({
            future::result(bound)
                .map_err(error::into_error)
                .and_then(|f| rx.map_err(error::into_error).join(f))
                .and_then(|(r, ())| r)
                .and_then(|o| match AuthKey::downcast_bind_temp_auth_key(o) {
                    Ok(true) => Ok(()),
                    Ok(false) => unimplemented!(),
                    Err(e) => Err(error::ErrorKind::WrongReplyType(e).into())
                })
        })
    }

    fn process_sync(&mut self, req: InternalEvent) -> <Self as Actor>::Future {
        use self::InternalEvent::*;
        match req {
            Inbound(message) => self.process_message(message),
            Outbound(message, responder) => self.send_message(message, responder),
            SetDelegate(delegate) => {
                self.delegate = delegate;
                Box::new(future::ok(()))
            },
            ConnectionClosed => Box::new({
                self.close_connection()
                    .map_unit_err("closing connection")
            }),
            BindAuthKey { perm_key, temp_key, temp_key_duration, salt } =>
                self.bind_auth_key(perm_key, temp_key, temp_key_duration, salt),
        }
    }
}

struct Scan<'a, S>(&'a mut RpcClientActor, S);

macro_rules! scan_type_impl {
    (@block_phase(($this:ident, $name:ident: $ty:ty) -> Future $block:block $($rin:tt)*) $($rout:tt)*) => {
        impl<'a> Scan<'a, $ty> {
            fn scan(self) -> BoxFuture<()> {
                let Scan($this, $name) = self;
                Box::new($block)
            }
        }

        scan_type_impl! { @block_phase($($rin)*) @out($ty) $($rout)* }
    };
    (@block_phase(($this:ident, $name:ident: $ty:ty) -> Result $block:block $($rin:tt)*) $($rout:tt)*) => {
        impl<'a> Scan<'a, $ty> {
            fn scan(self) -> BoxFuture<()> {
                let Scan($this, $name) = self;
                Box::new(future::result((|| $block)()))
            }
        }

        scan_type_impl! { @block_phase($($rin)*) @out($ty) $($rout)* }
    };

    (@block_phase() $($rest:tt)*) => {
        impl RpcClientActor {
            fn scan_replies(&mut self, mut obj: mtproto::TLObject) -> BoxFuture<()> {
                scan_type_impl! { @obj(self, obj) $($rest)* }
            }
        }
    };

    (@obj($self:ident, $obj:ident) @out($ty:ty) $($rest:tt)*) => {
        $obj = match $obj.downcast::<$ty>() {
            Ok(d) => return Scan::<$ty>($self, d).scan(),
            Err(o) => o,
        };
        scan_type_impl! { @obj($self, $obj) $($rest)* }
    };

    (@obj($self:ident, $obj:ident)) => {
        $self.delegate.call(Event::Unhandled($obj))
            .map_unit_err("sending Unhandled to the delegate")
    };
}

macro_rules! scan_type {
    ($($everything:tt)*) => {
        scan_type_impl! { @block_phase($($everything)*) }
    }
}

scan_type! {
    (this, mc: mtproto::manual::MessageContainer) -> Future {
        let mtproto::manual::MessageContainer::MsgContainer(mc) = mc;
        let mut ret = stream::FuturesUnordered::new();
        for msg in mc.messages.0 {
            this.maybe_ack(Some(msg.seqno), msg.msg_id);
            ret.push(this.scan_replies(msg.body.0));
        }
        ret.for_each(|()| Ok(()))
    }

    (this, rpc: mtproto::manual::RpcResult) -> Result {
        let mtproto::manual::RpcResult::RpcResult(rpc) = rpc;
        let (_, replier) = match this.pending_rpcs.remove(&rpc.req_msg_id) {
            Some(t) => t,
            None => {
                println!("no matching rpc for {:?}", rpc);
                return Ok(())
            }
        };
        let result = match rpc.result.downcast::<mtproto::RpcError>() {
            Ok(err) => Err(error::ErrorKind::RpcError(err).into()),
            Err(obj) => Ok(obj),
        };
        let _ = replier.send(result);
        Ok(())
    }
}

impl Actor for RpcClientActor {
    type Request = InternalEvent;
    type Response = ();
    type Error = error::Error;
    type Future = BoxFuture<()>;

    fn call(&mut self, req: Self::Request) -> Self::Future {
        self.process_sync(req)
    }
}

fn one_cpupool() -> ::futures_cpupool::CpuPool {
    ::futures_cpupool::Builder::new()
        .pool_size(1)
        .create()
}

impl RpcClient {
    pub fn spawn<E, S>(executor: &E, log: Logger, app_id: AppId, stream: S) -> Result<Self>
        where E: future::Executor<Box<Future<Item = (), Error = ()>>>,
              S: AsyncRead + AsyncWrite + 'static,
    {
        RpcClientActor::spawn(executor, log, app_id, stream)
            .map(|actor_tx| RpcClient { actor_tx })
    }

    pub fn ask<M: ::clacks_mtproto::Function>(&self, query: M) -> impl LocalFuture<M::Reply> {
        self.ask_inner::<M>(session::EitherMessageBuilder::encrypted(query))
    }

    pub(crate) fn ask_plain<M: ::clacks_mtproto::Function>(&self, query: M) -> impl LocalFuture<M::Reply> {
        self.ask_inner::<M>(session::EitherMessageBuilder::plain(query))
    }

    fn ask_inner<M: ::clacks_mtproto::Function>(&self, query: session::EitherMessageBuilder) -> impl LocalFuture<M::Reply> {
        self.actor_tx.ask(InternalEvent::Outbound, query)
                .and_then(|r| r)
                .and_then(|obj| match obj.downcast::<M::Reply>() {
                    Ok(r) => Ok(r),
                    Err(b) => Err(error::ErrorKind::WrongReplyType(b).into()),
                })
    }

    pub fn set_delegate(&self, delegate: EventDelegate) -> impl LocalFuture<()> {
        self.actor_tx.call(InternalEvent::SetDelegate(delegate))
    }

    pub fn new_auth_key(&self, temp_key_duration: Duration) -> impl LocalFuture<AuthKey> {
        ::kex::kex(self.clone(), one_cpupool(), None)
            .map(|(k, _)| k)
    }

    pub(crate) fn call(&self, req: InternalEvent) -> impl LocalFuture<()> {
        self.actor_tx.call(req)
    }
}
