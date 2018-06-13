//! Actor library built on top of the Tokio platform.
//!
//! Kabuki provides a simple way to structure concurrent applications built for
//! the Tokio / Futures platform. It is based on the [actor
//! model](https://en.wikipedia.org/wiki/Actor_model). An actor is a small unit
//! of computation that is used to manage state and resources. It receives
//! messages from other actors and performs some kind of action based on that
//! input. This way, instead of having state and resources accessed
//! concurrently, only a single thread access it and concurrent access is
//! handled through message passing.
//!
//! # Motivation
//!
//! [Tokio](https://docs.rs/tokio-core/0.1.2/tokio_core/reactor/index.html) and
//! [Futures](https://docs.rs/futures/0.1.7/futures/task/index.html) provide a
//! lightweight task primitive. However, it leaves the details of managing
//! concurrency up to the developer.

#![deny(missing_docs, private_in_public, unused_extern_crates)]

// TODO:
// - Use UnparkEvent once in-flight passes some threshold
// - Conditionally depend on tokio-core

#[macro_use] extern crate failure;
#[macro_use] extern crate scoped_tls;
extern crate futures;
extern crate tokio_service;

use failure::Error;
use futures::{Future, Stream, IntoFuture, Async, AsyncSink, Sink, Poll, future};
use futures::sync::{mpsc, oneshot};
use tokio_service::Service;

use std::mem;
use std::marker::PhantomData;


type Result<T> = ::std::result::Result<T, Error>;

/// A value that manages state and responds to messages
pub trait Actor {
    /// The message sent to the actor
    type Request: 'static;

    /// The response sent back from the actor
    type Response: 'static;

    /// The internal response future. This will remain on the actor's task and
    /// will be polled to completion before being sent back to the caller.
    type Future: IntoFuture<Item = Self::Response, Error = Error>;

    /// Poll the `Actor` to see if it has completed processing.
    ///
    /// Allows the actor to advance its internal state without receiving a
    /// message. For example, if the actor performs sets a timeout, the task
    /// managing the actor will get notified and this function will be called.
    fn poll(&mut self, state: ActorState) -> Async<()> {
        if state == ActorState::Listening {
            Async::NotReady
        } else {
            Async::Ready(())
        }
    }

    /// Indicates that the actor is ready to process the next inbox message
    ///
    /// Before the `ActorCell` attempts to read a message off the inbox, it will
    /// call this function. If the actor value is not ready to process the a new
    /// message, the cell will wait until it is.
    fn poll_ready(&mut self) -> Async<()> {
        Async::Ready(())
    }

    /// Process an inbound message and return a response.
    ///
    /// The response future will be polled until the value is realized and the
    /// response value will be sent back to the sender of the message.
    fn call(&mut self, req: Self::Request) -> Self::Future;
}

/// An actor implemented by a closure.
pub struct ActorFn<F, T> {
    f: F,
    _ty: PhantomData<fn() -> T>, // don't impose Sync on T
}

/// The return value of `ActorRef::call`
pub struct ActorFuture<U> {
    state: CallState<U>,
}

/// Tracks the state of the actor
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum ActorState {
    /// Listening for inbound messages
    Listening,
    /// Inbox is closed
    Finalizing,
}

/// Internal representation of the actor state
enum ActorStatePriv {
    Listening,
    Finalizing,
    Shutdown,
}

enum CallState<U> {
    Waiting(oneshot::Receiver<Result<U>>),
    Error(CallError),
    Consumed,
}

/// Builds an actor
pub struct Builder {
    // Inbox capacity
    inbox: usize,
    // Max number of in-flight requests
    in_flight: usize,
}

struct ActorAndOutbox<A: Actor> {
    actor: A,
    outbox: Outbox<A::Request, A::Response>,
}

/// Manages the runtime state of an `Actor`.
pub struct ActorCell<A: Actor> {
    // The actor value
    actor: ActorAndOutbox<A>,

    // Current state of the actor
    state: ActorStatePriv,

    // The actors inbox
    rx: mpsc::Receiver<Envelope<A::Request, A::Response>>,

    // A slab of futures that are being executed. Each slot in this vector is
    // either an active future or a pointer to the next empty slot. This is used
    // to get O(1) deallocation in the slab and O(1) allocation.
    //
    // The `next_future` field is the next slot in the `futures` array that's a
    // `Slot::Next` variant. If it points to the end of the array then the array
    // is full.
    futures: Vec<Slot<Processing<A>>>,
    next_future: usize,

    // Number of active futures running in the `futures` slab
    active: usize,

    // Maximum number of in-flight futures
    max: usize,
}

/// The sender to a potentially running actor
pub struct Outbox<T, U>(mpsc::Sender<Envelope<T, U>>);

impl<T, U> Clone for Outbox<T, U> {
    fn clone(&self) -> Self {
        Outbox(self.0.clone())
    }
}

/// Handle to an actor, used to send messages
pub struct ActorRef<T, U>(Outbox<T, U>);

/// The `ActorRef` for a given `Actor`
pub type ActorRefOf<A> = ActorRef<<A as Actor>::Request, <A as Actor>::Response>;

/// Used to represent `call` errors
#[derive(Debug, Fail)]
pub enum CallError {
    /// The actor's inbox is full
    #[fail(display = "actor's inbox was full")]
    Full,
    /// The actor has shutdown
    #[fail(display = "actor's inboxed closed due to shutdown")]
    Disconnected,
    /// The actor aborted processing the request for some reason
    #[fail(display = "actor aborted processing")]
    Aborted,
}

/// Used to represent an actor failing to start
#[derive(Debug, Fail)]
#[fail(display = "actor failed to spawn: {:?}", _0)]
pub struct SpawnError(pub futures::future::ExecuteErrorKind);

struct Envelope<T, U> {
    arg: T,
    ret: oneshot::Sender<Result<U>>,
}

struct Processing<A: Actor> {
    future: <A::Future as IntoFuture>::Future,
    ret: Option<oneshot::Sender<Result<A::Response>>>,
}

// Stores in-flight responses
enum Slot<T> {
    Next(usize),
    Data(T),
}

/// Returns an `Actor` backed by the given closure.
pub fn actor_fn<F, T, U>(f: F) -> ActorFn<F, T>
    where F: FnMut(T) -> U,
          U: IntoFuture,
{
    ActorFn {
        f: f,
        _ty: PhantomData,
    }
}

scoped_thread_local!(static CURRENT_ACTOR_REF: *const ::std::any::Any);

impl<A: Actor> ActorAndOutbox<A> {
    fn with<F, R>(&mut self, func: F) -> R
        where F: FnOnce(&mut A) -> R,
    {
        let ptr: *const ::std::any::Any = &self.outbox.0;
        CURRENT_ACTOR_REF.set(&ptr, move || func(&mut self.actor))
    }
}

/*
 *
 * ===== impl Builder =====
 *
 */

impl Builder {
    /// Returns a new actor `Builder` with default settings
    pub fn new() -> Builder {
        Default::default()
    }

    /// Sets the actor's inbox queue capacity.
    ///
    /// The default value is 1024.
    pub fn inbox_capacity(mut self, n: usize) -> Self {
        self.inbox = n;
        self
    }

    /// Sets the max number of in-flight requests the actor can process
    /// concurrently.
    ///
    /// The default value is 16
    pub fn max_in_flight(mut self, n: usize) -> Self {
        self.in_flight = n;
        self
    }

    /// Spawn a new actor
    pub fn spawn<A, Ex>(self, ex: &Ex, actor: A) -> Result<ActorRefOf<A>>
        where A: Actor + 'static,
              Ex: future::Executor<Box<Future<Item = (), Error = ()>>>,
    {
        let (actor_ref, actor_cell) = self.pair(actor);
        ex.execute(Box::new(actor_cell))
            .map(|()| actor_ref)
            .map_err(|e| SpawnError(e.kind()).into())
    }

    /// Spawn the given closure as an actor
    pub fn spawn_fn<F, T, U, Ex>(self, ex: &Ex, f: F) -> Result<ActorRef<T, U::Item>>
        where F: Fn(T) -> U + 'static,
              T: 'static,
              U: IntoFuture<Error = Error> + 'static,
              Ex: future::Executor<Box<Future<Item = (), Error = ()>>>,
    {
        self.spawn(ex, actor_fn(f))
    }

    fn pair<A>(self, actor: A) -> (ActorRefOf<A>, ActorCell<A>)
        where A: Actor,
              A::Request: 'static,
    {
        // TODO: respect inbox bound
        let (tx, rx) = mpsc::channel(self.inbox);

        let rx = ActorCell::new(actor, tx.clone(), rx, self.in_flight);
        let tx = ActorRef(Outbox(tx));

        (tx, rx)
    }
}

impl Default for Builder {
    fn default() -> Builder {
        Builder {
            inbox: 1024,
            in_flight: 16,
        }
    }
}

/*
 *
 * ===== impl ActorFn =====
 *
 */

impl<F, T: 'static, U: 'static> Actor for ActorFn<F, T>
    where F: FnMut(T) -> U,
          U: IntoFuture<Error = Error>,
{
    type Request = T;
    type Response = U::Item;
    type Future = U;

    fn call(&mut self, req: Self::Request) -> Self::Future {
        (self.f)(req)
    }
}

/*
 *
 * ===== impl ActorRef =====
 *
 */


impl<T: 'static, U: 'static> ActorRef<T, U> {
    /// A handle to the currently-running actor
    pub fn current() -> Option<Self> {
        if CURRENT_ACTOR_REF.is_set() {
            CURRENT_ACTOR_REF.with(|any| {
                unsafe { &**any }.downcast_ref::<mpsc::Sender<Envelope<T, U>>>().cloned()
                    .map(|tx| ActorRef(Outbox(tx)))
            })
        } else {
            None
        }
    }
}

impl<T, U> Clone for ActorRef<T, U> {
    fn clone(&self) -> Self {
        ActorRef(self.0.clone())
    }
}

impl<T, U> Service for ActorRef<T, U> {
    type Request = T;
    type Response = U;
    type Error = Error;
    type Future = ActorFuture<U>;

    /// Send a request to the actor
    fn call(&self, request: T) -> ActorFuture<U> {
        let (tx, rx) = oneshot::channel();

        let mut outbox = (self.0).0.clone();
        let envelope = Envelope {
            arg: request,
            ret: tx,
        };

        // TODO: impl the send
        let state = match outbox.start_send(envelope) {
            Ok(AsyncSink::Ready) => {
                CallState::Waiting(rx)
            }
            Ok(AsyncSink::NotReady(request)) => {
                let Envelope { arg, .. } = request;
                CallState::Error(CallError::Full)
            }
            Err(err) => {
                let Envelope { arg, .. } = err.into_inner();
                CallState::Error(CallError::Disconnected)
            }
        };

        ActorFuture { state }
    }
}

/*
 *
 * ===== impl ActorFuture =====
 *
 */

impl<U> Future for ActorFuture<U> {
    type Item = U;
    type Error = Error;

    fn poll(&mut self) -> Poll<U, Error> {
        match self.state {
            CallState::Waiting(ref mut rx) => {
                match rx.poll() {
                    Ok(Async::Ready(Ok(v))) => Ok(Async::Ready(v)),
                    Ok(Async::Ready(Err(e))) => Err(e),
                    Ok(Async::NotReady) => Ok(Async::NotReady),
                    Err(_) => Err(CallError::Aborted.into()),
                }
            }
            _ => {
                match mem::replace(&mut self.state, CallState::Consumed) {
                    CallState::Waiting(_) => unreachable!(),
                    CallState::Error(e) => Err(e.into()),
                    CallState::Consumed => panic!("polled after complete"),
                }
            }
        }
    }
}

/*
 *
 * ===== impl ActorCell =====
 *
 */

impl<A: Actor> ActorCell<A> {
    fn new(actor: A,
           tx: mpsc::Sender<Envelope<A::Request, A::Response>>,
           rx: mpsc::Receiver<Envelope<A::Request, A::Response>>,
           max_in_flight: usize)
        -> ActorCell<A>
    {
        ActorCell {
            actor: ActorAndOutbox {
                actor: actor,
                outbox: Outbox(tx),
            },
            state: ActorStatePriv::Listening,
            rx: rx,
            futures: vec![],
            next_future: 0,
            active: 0,
            max: max_in_flight,
        }
    }

    fn poll_pending(&mut self) -> Async<()> {
        let mut ret = Async::Ready(());

        // TODO: Make this less terrible :)
        for i in 0..self.futures.len() {
            // Poll the future
            match self.futures[i] {
                Slot::Data(ref mut f) => {
                    match f.poll() {
                        Async::NotReady => {
                            ret = Async::NotReady;
                            continue;
                        },
                        Async::Ready(()) => (),
                    }
                },
                _ => continue,
            }

            // At this point, the future is complete, so reclaim the spot
            self.remove_processing(i);
        }

        ret
    }

    fn poll_actor(&mut self) -> Async<()> {
        if !self.state.is_running() {
            return Async::Ready(());
        }

        // Poke the actor
        let state = self.state.as_pub();
        if self.actor.with(move |a| a.poll(state).is_ready()) {
            // Shutdown the actor
            self.state = ActorStatePriv::Shutdown;
            self.rx.close();
            return Async::Ready(());
        }

        // As long as the actor can accept new messages...
        while self.is_call_ready() {
            match self.rx.poll() {
                Ok(Async::Ready(Some(msg))) => {
                    let Envelope { arg, ret } = msg;
                    let fut = self.actor.with(|a| a.call(arg)).into_future();

                    let mut processing = Processing {
                        future: fut,
                        ret: Some(ret),
                    };

                    if processing.poll().is_ready() {
                        // request done, move on to the next one
                        continue;
                    }

                    self.push_processing(processing);
                }
                Ok(Async::Ready(None)) => {
                    self.state = ActorStatePriv::Finalizing;
                    return Async::Ready(());
                }
                Ok(Async::NotReady) => {
                    break;
                }
                Err(_) => {
                    // TODO: can this happen?
                    unimplemented!();
                }
            }
        }

        Async::NotReady
    }

    fn is_call_ready(&mut self) -> bool {
        match self.state {
            ActorStatePriv::Listening => {
                self.active < self.max && self.actor.with(|a| a.poll_ready()).is_ready()
            }
            _ => false,
        }
    }

    fn push_processing(&mut self, processing: Processing<A>) {
        debug_assert!(self.active < self.max);

        // If the `futures` slab is at capacity, grow by 1
        if self.next_future == self.futures.len() {
            debug_assert!(self.next_future < self.max);

            self.futures.reserve(1);
            self.futures.push(Slot::Next(self.next_future + 1));
        }

        self.active += 1;

        match mem::replace(&mut self.futures[self.next_future], Slot::Data(processing)) {
            Slot::Next(next) => self.next_future = next,
            Slot::Data(_) => panic!(),
        }
    }

    fn remove_processing(&mut self, idx: usize) {
        self.active -= 1;
        self.futures[idx] = Slot::Next(self.next_future);
        self.next_future = idx;
    }
}

impl<A: Actor> Future for ActorCell<A> {
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<(), ()> {
        let mut done = true;

        // Poll pending first futures first. This makes handling "max inflight"
        // simpler
        done &= self.poll_pending().is_ready();
        done &= self.poll_actor().is_ready();

        if done {
            debug_assert!(self.active == 0);
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }
}

/*
 *
 * ===== impl Processing =====
 *
 */

impl<A: Actor> Processing<A> {
    fn poll(&mut self) -> Async<()> {
        {
            // First, check to see if there is still interest on the receiving
            // the value
            let ret = self.ret.as_mut().unwrap();

            if let Ok(Async::Ready(())) = ret.poll_cancel() {
                return Async::Ready(());
            }
        }

        let ret = match self.future.poll() {
            Ok(Async::Ready(v)) => Ok(v),
            Ok(Async::NotReady) => return Async::NotReady,
            Err(e) => Err(e),
        };

        let _ = self.ret.take().expect("return channel already consumed").send(ret);

        Async::Ready(())
    }
}

/*
 *
 * ===== impl ActorStatePriv =====
 *
 */

impl ActorStatePriv {
    fn is_running(&self) -> bool {
        match *self {
            ActorStatePriv::Shutdown => false,
            _ => true,
        }
    }

    fn as_pub(&self) -> ActorState {
        match *self {
            ActorStatePriv::Listening => ActorState::Listening,
            _ => ActorState::Finalizing,
        }
    }
}

/// A service which drops all requests and returns `()`
pub struct NullService<T>(PhantomData<fn(T)>);

impl<T> Service for NullService<T> {
    type Request = T;
    type Response = ();
    type Error = Error;
    type Future = future::Ok<(), Error>;

    fn call(&self, _: Self::Request) -> Self::Future {
        future::ok(())
    }
}

/// Returns a service which drops all requests and returns `()`
pub fn null<T>() -> NullService<T> {
    NullService(PhantomData)
}
