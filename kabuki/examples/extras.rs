extern crate futures;
extern crate futures_spawn;
extern crate kabuki;

use futures::{Async, AsyncSink, Future, IntoFuture, Sink, Stream, future};
use futures::future::FutureResult;
use futures_spawn::Spawn;
use kabuki::Actor;
use std::{fmt, iter};

pub trait FutureErrorLogger<I> {
    fn log_errors_into<S: 'static>(self, sink: S) -> Box<futures::Future<Item = (), Error = ()>>
        where S: futures::Sink<SinkItem = I>;
}

pub trait ResultStderrLogger {
    type Ok;
    fn log_to_stderr(self, message: &str) -> Result<Self::Ok, ()>;
}

impl<T, E> ResultStderrLogger for Result<T, E>
    where E: fmt::Debug,
{
    type Ok = T;
    fn log_to_stderr(self, message: &str) -> Result<Self::Ok, ()> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => {
                use std::io::{Write, stderr};
                write!(stderr(), "XXX error ({}): {:?}\n\n", message, e).unwrap();
                Err(())
            },
        }
    }
}

pub trait FutureStderrLogger: futures::Future<Item = ()>
    where Self::Error: fmt::Debug,
{
    fn log_to_stderr(self, message: &'static str) -> Box<futures::Future<Item = (), Error = ()>>;
}

impl<T: 'static> FutureStderrLogger for T
    where T: futures::Future<Item = ()>,
          T::Error: fmt::Debug,
{
    fn log_to_stderr(self, message: &'static str) -> Box<futures::Future<Item = (), Error = ()>> {
        Box::new({
            self.then(move |r| {
                let _: Result<(), ()> = r.log_to_stderr(message);
                Ok(())
            })
        })
    }
}

pub trait AskActor {
    type Request;
    type Error;
    fn ask<Q, FS, A: 'static, F>(&mut self, constructor: F, question: Q)
                                 -> Box<futures::Future<Item = A, Error = Self::Error>>
        where F: FnOnce(Q, FS) -> Self::Request,
              FS: From<futures::sync::oneshot::Sender<A>>;
}

impl<T: 'static, E: 'static> AskActor for kabuki::ActorRef<T, (), E>
    where E: From<futures::Canceled> + From<kabuki::CallError<T>>,
{
    type Request = T;
    type Error = E;
    fn ask<Q, FS, A: 'static, F>(&mut self, constructor: F, question: Q)
                                 -> Box<futures::Future<Item = A, Error = Self::Error>>
        where F: FnOnce(Q, FS) -> Self::Request,
              FS: From<futures::sync::oneshot::Sender<A>>,
    {
        let (tx, rx) = futures::sync::oneshot::channel();
        Box::new({
            self.call(constructor(question, tx.into()))
                .and_then(move |()| rx.map_err(Into::into))
        })
    }
}

fn ready_to_end(state: kabuki::ActorState) -> Async<()> {
    if state == kabuki::ActorState::Listening {
        Async::NotReady
    } else {
        Async::Ready(())
    }
}

fn least_ready(a: Async<()>, b: Async<()>) -> Async<()> {
    use futures::Async::*;
    match (a, b) {
        (Ready(()), Ready(())) => Ready(()),
        _ => NotReady,
    }
}

pub struct SinkFull<T>(pub T);
pub struct SinkActor<S: 'static>(S) where S: Sink;

impl<S: 'static> SinkActor<S>
    where S: Sink,
          S::SinkError: fmt::Debug + From<SinkFull<S::SinkItem>>,
{
    pub fn new(sink: S) -> Self {
        SinkActor(sink)
    }

    pub fn spawn_default<SP>(spawn: &SP, sink: S) -> kabuki::ActorRef<S::SinkItem, (), S::SinkError>
        where SP: Spawn<kabuki::ActorCell<Self, ()>>,
    {
        kabuki::Builder::new().spawn(spawn, SinkActor::new(sink))
    }
}


impl<S: 'static> SinkActor<S>
    where S: Sink,
          S::SinkError: fmt::Debug + From<SinkFull<S::SinkItem>>,
{
    fn enqueue(&mut self, item: S::SinkItem) -> Result<(), S::SinkError> {
        match self.0.start_send(item)? {
            AsyncSink::Ready => Ok(()),
            AsyncSink::NotReady(item) => Err(SinkFull(item).into()),
        }
    }

    fn poll_sink(&mut self) -> Async<()> {
        self.0
            .poll_complete()
            .log_to_stderr("XXX poll_complete broke")
            .unwrap_or(Async::NotReady)
    }
}

impl<S: 'static> kabuki::Actor for SinkActor<S>
    where S: Sink,
          S::SinkError: fmt::Debug + From<SinkFull<S::SinkItem>>,
{
    type Request = S::SinkItem;
    type Response = ();
    type Error = S::SinkError;
    type Future = FutureResult<(), S::SinkError>;

    fn call(&mut self, item: S::SinkItem) -> Self::Future {
        future::result(self.enqueue(item))
    }

    fn poll(&mut self, state: kabuki::ActorState) -> Async<()> {
        least_ready(self.poll_sink(), ready_to_end(state))
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
          A: Actor<Request = S::Item, Response = kabuki::TailCall<S::Item, ()>, Error = S::Error>,
{
    stream: Option<S>,
    processing: Vec<Option<<A::Future as IntoFuture>::Future>>,
    actor: A,
}

impl<S: 'static, A: 'static> StreamConsumerActorFeeder<S, A>
    where S: Stream,
          A: Actor<Request = S::Item, Response = kabuki::TailCall<S::Item, ()>, Error = S::Error>,
{
    pub fn new(stream: S, actor: A) -> Self {
        StreamConsumerActorFeeder {
            stream: Some(stream),
            processing: Vec::with_capacity(5), // XXX configurable?
            actor: actor,
        }
    }

    fn done_processing(&self) -> Async<()> {
        if self.processing.is_empty() {
            Async::Ready(())
        } else {
            Async::NotReady
        }
    }

    fn try_poll(&mut self) -> Result<(), S::Error> {
        for fut_opt in &mut self.processing {
            poll_future_in_place(&mut self.actor, fut_opt)?;
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
            self.processing.push(Some(self.actor.call(item).into_future()));
            let future_complete = {
                let fut_opt = self.processing.last_mut().unwrap();
                poll_future_in_place(&mut self.actor, fut_opt)?
            };
            if future_complete {
                self.processing.pop();
            }
        }
        Ok(())
    }
}

fn poll_future_in_place<A>(actor: &mut A, fut_opt: &mut Option<<A::Future as IntoFuture>::Future>)
                           -> Result<bool, A::Error>
    where A: Actor<Response = kabuki::TailCall<<A as Actor>::Request, ()>>,
{
    use kabuki::TailCall::*;
    loop {
        match fut_opt.as_mut().map(Future::poll) {
            None |
            Some(Ok(Async::NotReady)) => return Ok(false),
            Some(Err(e)) => {
                *fut_opt = None;
                return Err(e);
            },

            Some(Ok(Async::Ready(Complete(())))) => {
                *fut_opt = None;
                return Ok(true);
            },
            Some(Ok(Async::Ready(Call(c)))) => {
                *fut_opt = Some(actor.call(c).into_future());
            },
        }
    }
}

impl<S: 'static, A: 'static> Actor for StreamConsumerActorFeeder<S, A>
    where S: Stream,
          S::Error: fmt::Debug,
          A: Actor<Request = S::Item, Response = kabuki::TailCall<S::Item, ()>, Error = S::Error>,
{
    type Request = S::Item;
    type Response = kabuki::TailCall<S::Item, ()>;
    type Error = S::Error;
    type Future = A::Future;

    fn call(&mut self, req: Self::Request) -> Self::Future {
        self.actor.call(req)
    }

    fn poll(&mut self, state: kabuki::ActorState) -> Async<()> {
        let _: Result<(), ()> = self.try_poll()
            .log_to_stderr("XXX poll_complete broke");
        least_ready(least_ready(self.done_processing(), ready_to_end(state)), self.actor.poll(state))
    }

    fn poll_ready(&mut self) -> Async<()> {
        self.actor.poll_ready()
    }
}

#[must_use = "SelfCalls do nothing unless returned"]
pub struct SelfCall<T> {
    call: Option<T>,
}

impl<T> Default for SelfCall<T> {
    fn default() -> Self {
        SelfCall { call: None }
    }
}

impl<T> From<()> for SelfCall<T> {
    fn from(_: ()) -> Self {
        Default::default()
    }
}

impl<T> SelfCall<T> {
    pub fn one(x: T) -> Self {
        SelfCall { call: Some(x) }
    }

    pub fn into_tail_call(self) -> kabuki::TailCall<T, ()> {
        match self.call {
            Some(c) => kabuki::TailCall::Call(c),
            None => kabuki::TailCall::Complete(()),
        }
    }
}

pub trait Response: Default {
    fn merge(self, other: Self) -> Self;
}

impl Response for () {
    fn merge(self, (): ()) -> () {}
}

impl<T> Response for SelfCall<T> {
    fn merge(self, other: Self) -> Self {
        let call = match (self.call, other.call) {
            (Some(_), Some(_)) => unimplemented!(),
            (x, None) | (None, x) => x,
        };
        SelfCall { call: call }
    }
}

#[must_use = "ResponseQueues do nothing unless returned"]
pub struct ResponseQueue<T: Response, E: 'static> {
    queue: Vec<Box<Future<Item = T, Error = E>>>,
}

impl<T: Response, E: 'static> Default for ResponseQueue<T, E> {
    fn default() -> Self {
        ResponseQueue { queue: vec![] }
    }
}

impl<T: Response, E: 'static> ResponseQueue<T, E> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn one<IF: 'static, IE: 'static>(f: IF) -> Self
        where IF: IntoFuture<Item = (), Error = IE>,
              IE: Into<E>,
    {
        let mut ret = ResponseQueue::new();
        ret.enqueue(f);
        ret
    }

    pub fn enqueue<IF: 'static, IE: 'static>(&mut self, f: IF)
        where IF: IntoFuture<Item = (), Error = IE>,
              IE: Into<E>,
    {
        self.queue.push(Box::new({
            f.into_future()
                .map(|()| Default::default())
                .map_err(Into::into)
        }));
    }

    pub fn merge(&mut self, other: Self) {
        self.queue.extend(other.queue);
    }

    pub fn merge_left<R>(&mut self, (other, right): (Self, R)) -> R {
        self.merge(other);
        right
    }
}

impl<T: 'static, E: 'static> ResponseQueue<SelfCall<T>, E> {
    pub fn enqueue_tail<IF: 'static, IE: 'static>(&mut self, f: IF)
        where IF: IntoFuture<Item = SelfCall<T>, Error = IE>,
              IE: Into<E>,
    {
        self.queue.push(Box::new({
            f.into_future()
                .map_err(Into::into)
        }));
    }
}

impl<T: 'static + Response, E: 'static> IntoFuture for ResponseQueue<T, E> {
    type Item = T;
    type Error = E;
    type Future = Box<Future<Item = T, Error = E>>;

    fn into_future(self) -> Self::Future {
        Box::new({
            future::join_all(self.queue)
                .map(|v| {
                    v.into_iter()
                        .fold(Default::default(), Response::merge)
                })
        })
    }
}

pub type ActorSC<A> = SelfCall<<A as Actor>::Request>;

pub struct ResponseQueueActor<A: 'static>
    where A: Actor<Response = ActorSC<A>>,
{
    actor: A,
}

impl<A: 'static> ResponseQueueActor<A>
    where A: Actor<Response = ActorSC<A>>,
{
    pub fn new(actor: A) -> Self {
        ResponseQueueActor { actor: actor }
    }
}

impl<A: 'static> Actor for ResponseQueueActor<A>
    where A: Actor<Response = ActorSC<A>>,
{
    type Request = A::Request;
    type Response = kabuki::TailCall<A::Request, ()>;
    type Error = A::Error;
    type Future = Box<Future<Item = Self::Response, Error = A::Error>>;

    fn call(&mut self, req: Self::Request) -> Self::Future {
        Box::new({
            self.actor
                .call(req)
                .into_future()
                .map(|s| s.into_tail_call())
        })
    }

    fn poll(&mut self, state: kabuki::ActorState) -> Async<()> {
        self.actor.poll(state)
    }

    fn poll_ready(&mut self) -> Async<()> {
        self.actor.poll_ready()
    }
}

fn main() {}
