use clacks_mtproto::{BoxedDeserialize, ConstructorNumber, mtproto};
use clacks_transport::{AppId, Session, TelegramCodec, session};
use futures::{Future, Stream, future};
use futures::unsync::oneshot;
use kabuki::{self, Actor, ActorRef, ActorRefOf};
use kabuki_extras::{
    BoxService, ErasedService, RealShutdownTcpStream, SinkActor, StreamConsumerActorFeeder, StreamEnded};
use slog::Logger;
use std::collections::BTreeMap;
use tokio_io::{AsyncRead, AsyncWrite, codec};
use tokio_service::Service;

use error::{self, BoxFuture, FutureExtInto, FutureExtUnit, Result};


#[derive(Debug)]
pub enum Event {
    Updates(mtproto::Updates),
    ConnectionClosing,
}

pub type EventDelegate = BoxService<Event, (), ()>;
type Responder = oneshot::Sender<error::Result<mtproto::TLObject>>;

struct RpcClientActor {
    tg_tx: Option<ActorRef<session::OutboundMessage, (), ::clacks_transport::error::Error>>,
    sink_error_rx: Option<oneshot::Receiver<::clacks_transport::error::Error>>,
    delegate: EventDelegate,
    pending_rpcs: BTreeMap<i64, (ConstructorNumber, Responder)>,
}

pub struct RpcClient {
    actor_tx: ActorRefOf<RpcClientActor>,
}

enum InternalEvent {
    Inbound(session::InboundMessage),
    Outbound(session::OutboundMessage, Responder),
    ConnectionClosed,
    SetDelegate(EventDelegate),
}

impl From<session::InboundMessage> for InternalEvent {
    fn from(m: session::InboundMessage) -> Self {
        InternalEvent::Inbound(m)
    }
}

impl From<StreamEnded<::clacks_transport::error::Error>> for InternalEvent {
    fn from(e: StreamEnded<::clacks_transport::error::Error>) -> Self {
        InternalEvent::ConnectionClosed
    }
}

impl RpcClientActor {
    fn spawn<E>(executor: &E, log: Logger, app_id: AppId, stream: RealShutdownTcpStream) -> Result<ActorRefOf<RpcClientActor>>
        where E: future::Executor<Box<Future<Item = (), Error = ()>>>,
    {
        let session = Session::new(app_id);
        let (tg_tx, tg_rx) = stream.framed(TelegramCodec::new(session)).split();
        let sink = SinkActor::new(log.clone(), tg_tx);
        let tg_tx = kabuki::Builder::new().spawn(executor, sink.actor)?;
        let actor = StreamConsumerActorFeeder::new(log, tg_rx, RpcClientActor {
            tg_tx: Some(tg_tx),
            sink_error_rx: Some(sink.error_rx),
            delegate: ErasedService::new_erased(kabuki::null()),
            pending_rpcs: BTreeMap::new(),
        }, 5);
        Ok(kabuki::Builder::new().spawn(executor, actor)?)
    }

    fn process_message(&mut self, message: session::InboundMessage) -> <Self as Actor>::Future {
        let payload =  mtproto::TLObject::boxed_deserialized_from_bytes(&message.payload)
            .map_err(error::into_error);
        if message.was_encrypted {
            self.maybe_ack(message.seq_no, message.message_id);
            return Box::new(future::result({
                payload.and_then(|o| self.scan_replies(o))
            }));
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
            Some(s) if s & 1 != 0 => (),  //self.session.ack_id(message_id),
            _ => (),
        }
    }

    fn send_message(&mut self, message: session::OutboundMessage, responder: Responder) -> <Self as Actor>::Future {
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
            tg_tx.call(message)
                .map_err(error::into_error)
        })
    }

    fn close_connection(&mut self) -> <EventDelegate as Service>::Future {
        self.tg_tx = None;
        self.sink_error_rx = None;
        self.delegate.call(Event::ConnectionClosing)
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
        }
    }
}

struct Scan<'a, S>(&'a mut RpcClientActor, S);

macro_rules! scan_type_impl {
    (@block_phase(($this:ident, $name:ident: $ty:ty) $block:block $($rin:tt)*) $($rout:tt)*) => {
        impl<'a> Scan<'a, $ty> {
            fn scan(self) -> Result<()> {
                let Scan($this, $name) = self;
                $block
            }
        }

        scan_type_impl! { @block_phase($($rin)*) @out($ty) $($rout)* }
    };

    (@block_phase() $($rest:tt)*) => {
        impl RpcClientActor {
            fn scan_replies(&mut self, mut obj: mtproto::TLObject) -> Result<()> {
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
        println!("unknown thing to scan {:?}", $obj);
        Ok(())
    };
}

macro_rules! scan_type {
    ($($everything:tt)*) => {
        scan_type_impl! { @block_phase($($everything)*) }
    }
}

scan_type! {
    (this, gz: mtproto::manual::GzipPacked) {
        use flate2::bufread::GzDecoder;
        use std::io::Read;

        let mut bytes: &[u8] = gz.packed_data();
        let mut decompressed = vec![];
        GzDecoder::new(&mut bytes).read_to_end(&mut decompressed)?;
        let obj = mtproto::TLObject::boxed_deserialized_from_bytes(&decompressed)?;
        this.scan_replies(obj)
    }

    (this, mc: mtproto::manual::MessageContainer) {
        let mtproto::manual::MessageContainer::MsgContainer(mc) = mc;
        for msg in mc.messages.0 {
            this.maybe_ack(Some(msg.seqno), msg.msg_id);
            this.scan_replies(msg.body.0)?;
        }
        Ok(())
    }

    (this, rpc: mtproto::manual::RpcResult) {
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
    type Future = Box<Future<Item = (), Error = error::Error>>;

    fn call(&mut self, req: Self::Request) -> Self::Future {
        self.process_sync(req)
    }
}

impl RpcClient {
    pub fn spawn<E>(executor: &E, log: Logger, app_id: AppId, stream: RealShutdownTcpStream) -> Result<Self>
        where E: future::Executor<Box<Future<Item = (), Error = ()>>>,
    {
        RpcClientActor::spawn(executor, log, app_id, stream)
            .map(|actor_tx| RpcClient { actor_tx })
    }

    pub fn ask<M: ::clacks_mtproto::Function>(&self, query: M) -> BoxFuture<M::Reply> {
        self.ask_inner::<M>(session::OutboundMessage::encrypted(query))
    }

    pub fn ask_plain<M: ::clacks_mtproto::Function>(&self, query: M) -> BoxFuture<M::Reply> {
        self.ask_inner::<M>(session::OutboundMessage::plain(query))
    }

    fn ask_inner<M: ::clacks_mtproto::Function>(&self, query: session::OutboundMessage) -> BoxFuture<M::Reply> {
        let (tx, rx) = oneshot::channel();
        Box::new({
            self.actor_tx.call(InternalEvent::Outbound(query, tx))
                .and_then(move |()| rx.map_err(error::into_error))
                .and_then(|r| r)
                .and_then(|obj| match obj.downcast::<M::Reply>() {
                    Ok(r) => Ok(r),
                    Err(b) => Err(error::ErrorKind::WrongReplyType(b).into()),
                })
        })
    }
}
