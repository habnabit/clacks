use failure::{Error, Fail};
use futures::{Async, AsyncSink, Canceled, IntoFuture, Poll, Sink, Stream, future, stream};
use futures::future::FutureResult;
use futures::unsync::oneshot;
use kabuki::{self, Actor};
use slog::Logger;
use std::fmt;


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

#[derive(Debug, Fail)]
#[fail(display = "")]
pub struct SinkErrored;

#[derive(Debug, Fail)]
#[fail(display = "")]
pub struct SinkFull;

pub struct SinkActor<Si> {
    log: Logger,
    inner: Option<SinkActorInner<Si>>,
}

struct SinkActorInner<Si> {
    sink: Si,
    error_handler: oneshot::Sender<Error>,
    force_close: bool,
}

impl<Si> SinkActorInner<Si>
    where Si: Sink + 'static,
          Si::SinkItem: 'static,
          Si::SinkError: Fail,
{
    fn poll_maybe_close(&mut self, state: kabuki::ActorState) -> Poll<(), Si::SinkError> {
        let () = try_ready!(self.sink.poll_complete());
        if self.force_close || state == kabuki::ActorState::Finalizing {
            self.sink.close()
        } else {
            Ok(Async::NotReady)
        }
    }

    fn force_close(&mut self) {
        self.force_close = true;
    }
}

pub struct PreparedSinkActor<Ac> {
    pub actor: Ac,
    pub error_rx: oneshot::Receiver<Error>,
}

impl<Si> SinkActor<Si>
    where Si: Sink + 'static,
          Si::SinkItem: 'static,
          Si::SinkError: Fail,
{
    pub fn new(log: Logger, sink: Si) -> PreparedSinkActor<Self> {
        let (error_handler, error_rx) = oneshot::channel();
        let actor = SinkActor {
            log, inner: Some(SinkActorInner { sink, error_handler, force_close: false }),
        };
        PreparedSinkActor { actor, error_rx }
    }

    fn enqueue(&mut self, item: Si::SinkItem) -> Result<(), Error> {
        if let Some(ref mut inner) = self.inner {
            match inner.sink.start_send(item)? {
                AsyncSink::Ready => Ok(()),
                AsyncSink::NotReady(item) => Err(SinkFull)?,
            }
        } else {
            Ok(())
        }
    }

    fn poll_sink(&mut self, state: kabuki::ActorState) -> Async<()> {
        let log = &self.log;
        let mut fatal_error_opt: Option<Error> = None;
        let ret = if let Some(ref mut inner) = self.inner {
            inner.poll_maybe_close(state)
                .map_err(|e| -> () {
                    let e = Error::context(e.into(), SinkErrored).into();
                    debug!(log, "sink failure"; "error" => ?e);
                    fatal_error_opt = Some(e);
                    inner.force_close();
                })
                .ok()
        } else {
            None
        }.unwrap_or(Async::Ready(()));
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

impl<Si> Actor for SinkActor<Si>
    where Si: Sink + 'static,
          Si::SinkItem: 'static,
          Si::SinkError: Fail,
{
    type Request = Si::SinkItem;
    type Response = ();
    type Future = FutureResult<(), Error>;

    fn call(&mut self, item: Self::Request) -> Self::Future {
        future::result(self.enqueue(item))
    }

    fn poll(&mut self, state: kabuki::ActorState) -> Async<()> {
        self.poll_sink(state)
    }
}

#[derive(Debug)]
pub struct StreamEnded(pub Result<(), Error>);

pub struct StreamConsumerActorFeeder<S, A, Req>
    where S: Stream + 'static,
          S::Item: Into<Req>,
          S::Error: Fail,
          A: Actor<Request = Req, Response = ()> + 'static,
          Req: From<StreamEnded> + 'static,
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
          S::Error: Fail,
          A: Actor<Request = Req, Response = ()> + 'static,
          Req: From<StreamEnded> + 'static,
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
                    self.processing.push(self.actor.call(StreamEnded(Err(e.into())).into()).into_future());
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
          S::Error: Fail,
          A: Actor<Request = Req, Response = ()> + 'static,
          Req: From<StreamEnded> + 'static,
{
    type Request = Req;
    type Response = ();
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
