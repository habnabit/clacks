extern crate futures;
extern crate futures_spawn;
extern crate kabuki;

use futures::{Async, AsyncSink, Future, IntoFuture, Sink, Stream, future};
use futures::future::FutureResult;
use kabuki::Actor;
use std::iter;

pub struct SinkFull<T>(pub T);
pub struct SinkActor<S: 'static, F: 'static>(S, F)
    where S: Sink,
          F: FnMut(S::SinkError);

impl<S: 'static, F: 'static> SinkActor<S, F>
    where S: Sink,
          S::SinkError: From<SinkFull<S::SinkItem>>,
          F: FnMut(S::SinkError),
{
    pub fn new(sink: S, func: F) -> Self {
        SinkActor(sink, func)
    }

    pub fn spawn_default<SP>(spawn: &SP, sink: S, func: F) -> kabuki::ActorRef<S::SinkItem, (), S::SinkError>
        where SP: futures_spawn::Spawn<kabuki::ActorCell<Self>>,
    {
        kabuki::Builder::new().spawn(spawn, SinkActor::new(sink, func))
    }
}


impl<S: 'static, F: 'static> SinkActor<S, F>
    where S: Sink,
          S::SinkError: From<SinkFull<S::SinkItem>>,
          F: FnMut(S::SinkError),
{
    fn enqueue(&mut self, item: S::SinkItem) -> Result<(), S::SinkError> {
        match self.0.start_send(item)? {
            AsyncSink::Ready => Ok(()),
            AsyncSink::NotReady(item) => Err(SinkFull(item).into()),
        }
    }

    fn poll_sink(&mut self) -> Async<()> {
        match self.0.poll_complete() {
            Ok(a) => a,
            Err(e) => {
                (self.1)(e);
                Async::NotReady
            },
        }
    }
}

impl<S: 'static, F: 'static> kabuki::Actor for SinkActor<S, F>
    where S: Sink,
          S::SinkError: From<SinkFull<S::SinkItem>>,
          F: FnMut(S::SinkError),
{
    type Request = S::SinkItem;
    type Response = ();
    type Error = S::SinkError;
    type Future = FutureResult<(), S::SinkError>;

    fn call(&mut self, item: S::SinkItem) -> Self::Future {
        future::result(self.enqueue(item))
    }

    fn poll(&mut self, _: kabuki::ActorState) -> Async<()> {
        self.poll_sink();
        Async::NotReady
    }

    fn poll_ready(&mut self) -> Async<()> {
        self.poll_sink()
    }
}

pub trait MultiCall<T, U, E> {
    fn call_all(&mut self, requests: Vec<T>) -> Box<Future<Item = Vec<U>, Error = E>>;
}

impl<T: 'static, U: 'static, E: 'static> MultiCall<T, U, E> for kabuki::ActorRef<T, U, E>
    where E: From<kabuki::CallError<T>>,
{
    fn call_all(&mut self, requests: Vec<T>) -> Box<Future<Item = Vec<U>, Error = E>> {
        if requests.is_empty() {
            return Box::new(future::ok(vec![]));
        }
        Box::new(future::join_all({
            iter::repeat(self.clone())
                .zip(requests)
                .map(|(mut rf, rq)| rf.call(rq))
                .collect::<Vec<_>>()
        }))
    }
}

pub struct StreamConsumerActorFeeder<S: 'static, A: 'static>
    where S: Stream,
          A: Actor<Request = S::Item, Response = (), Error = S::Error>,
{
    stream: Option<S>,
    processing: Vec<Option<<A::Future as IntoFuture>::Future>>,
    actor: A,
}

impl<S: 'static, A: 'static> StreamConsumerActorFeeder<S, A>
    where S: Stream,
          A: Actor<Request = S::Item, Response = (), Error = S::Error>,
{
    pub fn new(stream: S, actor: A) -> Self {
        StreamConsumerActorFeeder {
            stream: Some(stream),
            processing: Vec::with_capacity(5), // XXX configurable?
            actor: actor,
        }
    }

    fn still_processing(&self, state: kabuki::ActorState) -> Async<()> {
        if state != kabuki::ActorState::Listening && self.processing.is_empty() {
            Async::Ready(())
        } else {
            Async::NotReady
        }
    }

    fn try_poll(&mut self, state: kabuki::ActorState) -> Result<Async<()>, S::Error> {
        for fut_opt in &mut self.processing {
            match fut_opt.as_mut().map(Future::poll) {
                Some(Ok(Async::Ready(()))) => *fut_opt = None,
                Some(Err(e)) => return Err(e),
                _ => (),
            }
        }
        self.processing.retain(Option::is_some);
        while self.processing.len() < self.processing.capacity() {
            let item = match self.stream.as_mut().map(Stream::poll) {
                None => break,
                Some(Ok(Async::Ready(Some(i)))) => i,
                Some(Ok(Async::Ready(None))) => {
                    self.stream = None;
                    break;
                },
                Some(Ok(Async::NotReady)) => break,
                Some(Err(e)) => return Err(e),
            };
            let mut fut = self.actor.call(item).into_future();
            match fut.poll()? {
                Async::Ready(()) => (),
                Async::NotReady => self.processing.push(Some(fut)),
            }
        }
        Ok(self.still_processing(state))
    }
}

impl<S: 'static, A: 'static> Actor for StreamConsumerActorFeeder<S, A>
    where S: Stream,
          A: Actor<Request = S::Item, Response = (), Error = S::Error>,
{
    type Request = S::Item;
    type Response = ();
    type Error = S::Error;
    type Future = A::Future;

    fn poll(&mut self, state: kabuki::ActorState) -> Async<()> {
        self.try_poll(state)
            .unwrap_or_else(|_| panic!("XXX poll broke"))
    }

    fn call(&mut self, req: Self::Request) -> Self::Future {
        self.actor.call(req)
    }
}

#[must_use = "ResponseQueues do nothing unless returned"]
pub struct ResponseQueue<E> {
    queue: Vec<Box<Future<Item = (), Error = E>>>,
}

impl<E> Default for ResponseQueue<E> {
    fn default() -> Self {
        ResponseQueue { queue: vec![] }
    }
}

impl<E> ResponseQueue<E> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn one<I: 'static>(f: I) -> Self
        where I: IntoFuture<Item = (), Error = E>,
    {
        ResponseQueue { queue: vec![Box::new(f.into_future())] }
    }

    pub fn enqueue<I: 'static>(&mut self, f: I)
        where I: IntoFuture<Item = (), Error = E>,
    {
        self.queue.push(Box::new(f.into_future()));
    }

    pub fn merge(&mut self, other: Self) {
        self.queue.extend(other.queue);
    }

    pub fn merge_left<T>(&mut self, (other, right): (Self, T)) -> T {
        self.merge(other);
        right
    }
}

pub struct ResponseQueueActor<A: 'static, E: 'static>
    where A: Actor<Response = ResponseQueue<E>, Error = E>,
{
    actor: A,
}

impl<A: 'static, E: 'static> ResponseQueueActor<A, E>
    where A: Actor<Response = ResponseQueue<E>, Error = E>,
{
    pub fn new(actor: A) -> Self {
        ResponseQueueActor { actor: actor }
    }
}

impl<A: 'static, E: 'static> Actor for ResponseQueueActor<A, E>
    where A: Actor<Response = ResponseQueue<E>, Error = E>,
{
    type Request = A::Request;
    type Response = ();
    type Error = E;
    type Future = Box<Future<Item = (), Error = E>>;

    fn call(&mut self, req: Self::Request) -> Self::Future {
        Box::new({
            self.actor
                .call(req)
                .into_future()
                .and_then(|q| future::join_all(q.queue))
                .map(|_: Vec<()>| ())
        })
    }
}

fn main() {}
