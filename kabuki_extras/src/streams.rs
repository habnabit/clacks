use futures::{Async, AsyncSink, Canceled, Future, IntoFuture, Poll, Sink, Stream, future, stream};
use futures::future::FutureResult;
use futures::unsync::oneshot;
use kabuki::{self, Actor};
use slog::Logger;
use std::fmt;
use tokio_service::Service;


pub trait AsyncReady where Self: Sized {
    fn less_ready(self, other: Self) -> Self;
    fn or_less_ready<F>(self, func: F) -> Self
        where F: FnOnce() -> Self;
}

impl AsyncReady for Async<()> {
    fn less_ready(self, other: Self) -> Self {
        use futures::Async::*;
        match (self, other) {
            (Ready(()), Ready(())) => Ready(()),
            _ => NotReady,
        }
    }

    fn or_less_ready<F>(self, func: F) -> Self
        where F: FnOnce() -> Self,
    {
        use futures::Async::*;
        match self {
            Ready(()) => func(),
            NotReady => NotReady,
        }
    }
}

pub fn ready_to_end(state: kabuki::ActorState) -> Async<()> {
    if state == kabuki::ActorState::Listening {
        Async::NotReady
    } else {
        Async::Ready(())
    }
}

pub type BoxService<Req, Res, E> = Box<Service<Request = Req, Response = Res, Error = E,
                                           Future = Box<Future<Item = Res, Error = E>>>>;

pub struct ErasedService<Se>(Se)
    where Se: Service,
          Se::Future: 'static;

impl<Se> ErasedService<Se>
    where Se: Service + 'static,
          Se::Future: 'static,
{
    pub fn new_erased(service: Se) -> BoxService<Se::Request, Se::Response, Se::Error> {
        Box::new(ErasedService(service))
    }
}

impl<Se> Service for ErasedService<Se>
    where Se: Service,
          Se::Future: 'static,
{
    type Request = Se::Request;
    type Response = Se::Response;
    type Error = Se::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error>>;

    fn call(&self, req: Self::Request) -> Self::Future {
        Box::new(self.0.call(req))
    }
}

#[derive(Debug)]
pub struct SinkErrored<E>(pub E);
#[derive(Debug)]
pub struct SinkFull<T>(pub T);

#[derive(Debug)]
enum CloseState<W> {
    Waiting(W),
    CloseNotReady,
    Closed,
}

impl<W> CloseState<W> {
    fn erase_waiting(&self) -> CloseState<()> {
        match self {
            &CloseState::Waiting(_) => CloseState::Waiting(()),
            &CloseState::CloseNotReady => CloseState::CloseNotReady,
            &CloseState::Closed => CloseState::Closed,
        }
    }
}

pub struct SinkActor<Si, EH> {
    log: Logger,
    inner: Option<SinkActorInner<Si, EH>>,
}

struct SinkActorInner<Si, EH> {
    sink: Si,
    close_state: CloseState<oneshot::Receiver<()>>,
    error_handler: oneshot::Sender<EH>,
}

impl<Si, EH> SinkActorInner<Si, EH>
    where Si: Sink + 'static,
          Si::SinkItem: 'static,
          Si::SinkError: From<SinkFull<Si::SinkItem>> + From<Canceled> + 'static,
          EH: fmt::Debug + From<SinkErrored<Si::SinkError>> + 'static,
{
    fn poll_close(&mut self) -> Poll<(), Si::SinkError> {
        loop {
            self.close_state = match self.close_state {
                CloseState::Waiting(ref mut f) => {
                    let () = try_ready!(f.poll());
                    CloseState::CloseNotReady
                },
                CloseState::CloseNotReady => {
                    let () = try_ready!(self.sink.close());
                    CloseState::Closed
                },
                CloseState::Closed => return Ok(Async::Ready(())),
            };
        }
    }

    fn force_close(&mut self) {
        if let CloseState::Waiting(_) = self.close_state {
            self.close_state = CloseState::CloseNotReady;
        }
    }
}

pub struct PreparedSinkActor<Ac, EH> {
    pub actor: Ac,
    pub error_rx: oneshot::Receiver<EH>,
    pub close_tx: oneshot::Sender<()>,
}

impl<Si, EH> SinkActor<Si, EH>
    where Si: Sink + 'static,
          Si::SinkItem: 'static,
          Si::SinkError: From<SinkFull<Si::SinkItem>> + From<Canceled> + 'static,
          EH: fmt::Debug + From<SinkErrored<Si::SinkError>> + 'static,
{
    pub fn new(log: Logger, sink: Si) -> PreparedSinkActor<Self, EH> {
        let (error_handler, error_rx) = oneshot::channel();
        let (close_tx, close_rx) = oneshot::channel();
        let actor = SinkActor {
            log, inner: Some(SinkActorInner {
                sink, error_handler,
                close_state: CloseState::Waiting(close_rx),
            }),
        };
        PreparedSinkActor { actor, error_rx, close_tx }
    }

    fn enqueue(&mut self, item: Si::SinkItem) -> Result<(), Si::SinkError> {
        if let Some(ref mut inner) = self.inner {
            match inner.sink.start_send(item)? {
                AsyncSink::Ready => Ok(()),
                AsyncSink::NotReady(item) => Err(SinkFull(item).into()),
            }
        } else {
            Ok(())
        }
    }

    fn poll_sink(&mut self) -> (Async<()>, CloseState<()>) {
        let log = &self.log;
        let mut fatal_error_opt: Option<EH> = None;
        let ret = if let Some(ref mut inner) = self.inner {
            inner.sink
                .poll_complete()
                .and_then(|a| {
                    let _: Async<()> = inner.poll_close()?;
                    Ok((a, inner.close_state.erase_waiting()))
                })
                .map_err(|e| -> () {
                    let e = SinkErrored(e).into();
                    debug!(log, "sink failure"; "error" => ?e);
                    fatal_error_opt = Some(e);
                    inner.force_close();
                })
                .ok()
        } else {
            None
        }.unwrap_or((Async::NotReady, CloseState::Closed));
        if let Some(fatal_error) = fatal_error_opt {
            match self.inner.take() {
                Some(inner_cell) => {
                    let _ = inner_cell.error_handler.send(fatal_error);
                },
                None => crit!(log, "couldn't report fatal sink error"; "error" => ?fatal_error),
            }
        }
        ret
    }
}

impl<Si, EH> Actor for SinkActor<Si, EH>
    where Si: Sink + 'static,
          Si::SinkItem: 'static,
          Si::SinkError: From<SinkFull<Si::SinkItem>> + From<Canceled> + 'static,
          EH: fmt::Debug + From<SinkErrored<Si::SinkError>> + 'static,
{
    type Request = Si::SinkItem;
    type Response = ();
    type Error = Si::SinkError;
    type Future = FutureResult<(), Si::SinkError>;

    fn call(&mut self, item: Self::Request) -> Self::Future {
        future::result(self.enqueue(item))
    }

    fn poll(&mut self, state: kabuki::ActorState) -> Async<()> {
        let (sink_flushed, sink_closed) = self.poll_sink();
        match sink_closed {
            CloseState::CloseNotReady => Async::NotReady,
            _ => sink_flushed.or_less_ready(|| ready_to_end(state)),
        }
    }

    fn poll_ready(&mut self) -> Async<()> {
        let (sink_flushed, sink_closed) = self.poll_sink();
        match sink_closed {
            CloseState::Waiting(()) => sink_flushed,
            _ => Async::NotReady,
        }
    }
}

#[derive(Debug)]
pub struct StreamEnded<E>(pub Result<(), E>);

pub struct StreamConsumerActorFeeder<S, A, Req>
    where S: Stream + 'static,
          S::Item: Into<Req>,
          S::Error: fmt::Debug,
          A: Actor<Request = Req, Response = ()> + 'static,
          Req: From<StreamEnded<S::Error>> + 'static,
{
    log: Logger,
    stream: Option<S>,
    processing: stream::FuturesUnordered<<A::Future as IntoFuture>::Future>,
    concurrency: usize,
    actor: A,
}

impl<S, A, Req> StreamConsumerActorFeeder<S, A, Req>
    where S: Stream + 'static,
          S::Item: Into<Req>,
          S::Error: fmt::Debug,
          A: Actor<Request = Req, Response = ()> + 'static,
          Req: From<StreamEnded<S::Error>> + 'static,
{
    pub fn new(log: Logger, stream: S, actor: A, concurrency: usize) -> Self {
        StreamConsumerActorFeeder {
            log, actor, concurrency,
            stream: Some(stream),
            processing: stream::FuturesUnordered::new(),
        }
    }

    fn done_processing(&self) -> Async<()> {
        if self.processing.is_empty() {
            Async::Ready(())
        } else {
            Async::NotReady
        }
    }

    fn drive_stream(&mut self) {
        while self.processing.len() < self.concurrency {
            let item = match self.stream.as_mut().map(Stream::poll) {
                None => break,
                Some(Ok(Async::Ready(Some(i)))) => i,
                Some(Ok(Async::Ready(None))) => {
                    debug!(self.log, "stream finished");
                    self.stream = None;
                    self.processing.push(self.actor.call(StreamEnded(Ok(())).into()).into_future());
                    break;
                },
                Some(Ok(Async::NotReady)) => break,
                Some(Err(e)) => {
                    debug!(self.log, "stream failed"; "error" => ?e);
                    self.stream = None;
                    self.processing.push(self.actor.call(StreamEnded(Err(e)).into()).into_future());
                    break;
                },
            };
            self.processing.push(self.actor.call(item.into()).into_future());
        }
    }
}

impl<S, A, Req> Actor for StreamConsumerActorFeeder<S, A, Req>
    where S: Stream + 'static,
          S::Item: Into<Req>,
          S::Error: fmt::Debug,
          A: Actor<Request = Req, Response = ()> + 'static,
          A::Error: fmt::Debug,
          Req: From<StreamEnded<S::Error>> + 'static,
{
    type Request = Req;
    type Response = ();
    type Error = A::Error;
    type Future = A::Future;

    fn call(&mut self, req: Req) -> A::Future {
        self.actor.call(req)
    }

    fn poll(&mut self, state: kabuki::ActorState) -> Async<()> {
        self.drive_stream();
        let processing_result = self.processing.poll()
            .map(|r| r.map(|_| ()))
            .unwrap_or_else(|e| {
                crit!(self.log, "error in stream poll"; "error" => ?e);
                Async::Ready(())
            });
        self.actor.poll(state)
            .less_ready(processing_result)
            .or_less_ready(|| ready_to_end(state))
            .or_less_ready(|| self.done_processing())
    }

    fn poll_ready(&mut self) -> Async<()> {
        self.actor.poll_ready()
    }
}
