use futures::{Async, AsyncSink, Canceled, Future, IntoFuture, Poll, Sink, StartSend, Stream, stream};
use futures::unsync::{mpsc, oneshot};
use kabuki::{self, Actor, ActorRef};
use slog::Logger;
use smallvec::{Array, SmallVec};
use std::{fmt, mem};
use std::collections::VecDeque;
use std::rc::Rc;
use void::Void;


pub type SmallVec8<T> = SmallVec<[T; 8]>;

/// The envelope for the request and response for a call to `Ac`.
pub type FurtherCall<Ac> = (<Ac as Actor>::Request, oneshot::Sender<<Ac as Actor>::Future>);

/// A `Scrundle` which produces requests and errors for `Ac`.
pub type ActorSL<Ac> = Scrundle<<Ac as Actor>::Request, <Ac as Actor>::Error>;

/// A `Scrundle` which never produces requests but may produce errors for `Ac`.
pub type ActorNSL<Ac> = Scrundle<Void, <Ac as Actor>::Error>;

/// A `RemoteScrundle` which may submit requests or errors to a `Scrundle` for `Ac`.
pub type ActorRSL<Ac> = RemoteScrundle<<Ac as Actor>::Request, <Ac as Actor>::Error>;

/// The type of an `ActorRef` created by spawning a `ScrundleActor<Ac>`.
pub type ScrundleActorExternal<Ac> = ActorRef<
    <Ac as ScrundleCapable>::Iter, (), <Ac as Actor>::Error>;

type ActorSLS<Ac> = ScrundleStream<<Ac as Actor>::Request, <Ac as Actor>::Error>;
type NextScrundleStream<Req, E> = Box<Stream<Item = Scrundle<Req, E>, Error = E>>;
type NextScrundle<Req, E> = (Option<Scrundle<Req, E>>, NextScrundleStream<Req, E>);
type NextScrundleFuture<Req, E> = Box<Future<Item = NextScrundle<Req, E>, Error = E>>;
type NextScrundleMaybeStream<Req, E> = (Option<Scrundle<Req, E>>, Option<NextScrundleStream<Req, E>>);
type NextScrundleMaybeStreamFuture<Req, E> = Box<Future<Item = NextScrundleMaybeStream<Req, E>, Error = E>>;

pub trait ScrundleCapable: Actor<Response = ActorSL<Self>>
    where Self: Sized,
          Self::Error: 'static,
{
    type Iter: IntoIterator<Item = Self::Request>;
}

pub fn ready_to_end(state: kabuki::ActorState) -> Async<()> {
    if state == kabuki::ActorState::Listening {
        Async::NotReady
    } else {
        Async::Ready(())
    }
}

pub fn least_ready(a: Async<()>, b: Async<()>) -> Async<()> {
    use futures::Async::*;
    match (a, b) {
        (Ready(()), Ready(())) => Ready(()),
        _ => NotReady,
    }
}

pub fn smallvec_one<T, Arr>(item: T) -> SmallVec<Arr>
    where Arr: Array<Item = T>,
{
    let mut vec = SmallVec::new();
    vec.push(item);
    vec
}

struct SendUnitOnDrop(Option<oneshot::Sender<()>>);

impl SendUnitOnDrop {
    fn new() -> (Rc<Self>, oneshot::Receiver<()>) {
        let (tx, rx) = oneshot::channel();
        (Rc::new(SendUnitOnDrop(Some(tx))), rx)
    }
}

impl Drop for SendUnitOnDrop {
    fn drop(&mut self) {
        let _ = self.0.take().map(|sender| sender.send(()));
    }
}

#[derive(DebugStub)]
struct ScrundleRemotes<Req, E> {
    #[debug_stub = "<tx>"]
    tx: mpsc::Sender<Scrundle<Req, E>>,
    #[debug_stub = "<rxv>"]
    rxv: Vec<NextScrundleStream<Req, E>>,
}

impl<Req, E> ScrundleRemotes<Req, E>
    where Req: 'static,
          E: 'static,
{
    fn new() -> Self {
        let (tx, rx) = mpsc::channel(5);
        ScrundleRemotes { tx: tx, rxv: vec![Box::new({
            rx.map_err(|()| unreachable!())
        })] }
    }

    fn map_err_into<E2>(self) -> ScrundleRemotes<Req, E2>
        where E: Into<E2>,
              E2: 'static,
    {
        let mut ret = ScrundleRemotes::<Req, E2>::new();
        ret.rxv.extend({
            self.rxv.into_iter()
                .map(|stream| Box::new({
                    stream.map(Scrundle::map_err_into)
                        .map_err(Into::into)
                }) as Box<Stream<Item = Scrundle<Req, E2>, Error = E2>>)
        });
        ret
    }

    fn merge_opts_in_place(a: &mut Option<Self>, b: Option<Self>) {
        match (a, b) {
            (_, None) => (),
            (a_ref @ &mut None, b @ Some(_)) => {
                *a_ref = b;
            },
            (&mut Some(ref mut a), Some(ref mut b)) => {
                a.rxv.extend(mem::replace(&mut b.rxv, vec![]));
            },
        }
    }

    fn create_remote(&self) -> RemoteScrundle<Req, E> {
        RemoteScrundle { tx: self.tx.clone() }
    }

    fn into_scrundle_stream(self) -> ScrundleStream<Req, E> {
        let mut streams = stream::FuturesUnordered::new();
        for rx in self.rxv {
            streams.push(stream_into_step_future(rx));
        }
        ScrundleStream { streams }
    }
}

impl<E> ScrundleRemotes<Void, E>
    where E: 'static,
{
    fn never_into<Req, E2>(self) -> ScrundleRemotes<Req, E2>
        where Req: 'static,
              E: Into<E2>,
              E2: 'static,
    {
        let mut ret = ScrundleRemotes::<Req, E2>::new();
        ret.rxv.extend({
            self.rxv.into_iter()
                .map(|stream| Box::new({
                    stream.map(Scrundle::never_into)
                        .map_err(Into::into)
                }) as Box<Stream<Item = Scrundle<Req, E2>, Error = E2>>)
        });
        ret
    }
}

fn stream_into_step_future<Req, E>(stream: NextScrundleStream<Req, E>) -> NextScrundleFuture<Req, E>
    where Req: 'static,
          E: 'static,
{
    Box::new({
        stream.into_future()
            .map_err(|(e, _)| e)
    })
}

struct ScrundleStream<Req, E> {
    streams: stream::FuturesUnordered<NextScrundleFuture<Req, E>>,
}

impl<Req, E> Stream for ScrundleStream<Req, E>
    where Req: 'static,
          E: 'static,
{
    type Item = Scrundle<Req, E>;
    type Error = E;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        loop {
            match self.streams.poll()? {
                Async::Ready(Some((Some(ret), this_stream))) => {
                    self.streams.push(stream_into_step_future(this_stream));
                    return Ok(Async::Ready(Some(ret)));
                },
                Async::Ready(Some((None, _))) => continue,
                Async::Ready(None) => return Ok(Async::Ready(None)),
                Async::NotReady => return Ok(Async::NotReady),
            }
        }
    }
}

#[must_use = "Scrundles do nothing unless returned"]
#[derive(DebugStub)]
pub struct Scrundle<Req, E> {
    #[debug_stub = "<calls>"]
    calls: SmallVec8<Req>,
    #[debug_stub = "<futures>"]
    futures: SmallVec8<Box<Future<Item = Scrundle<Req, E>, Error = E>>>,
    remote: Option<ScrundleRemotes<Req, E>>,
}

impl<Req, E> Default for Scrundle<Req, E> {
    fn default() -> Self {
        Scrundle {
            calls: SmallVec::new(),
            futures: SmallVec::new(),
            remote: None,
        }
    }
}

impl<Req, E> Scrundle<Req, E>
    where Req: 'static,
          E: 'static,
{
    pub fn new() -> Self {
        Default::default()
    }

    pub fn remote(&mut self) -> RemoteScrundle<Req, E> {
        match &mut self.remote {
            &mut Some(ref remotes) => remotes.create_remote(),
            r => {
                let remotes = ScrundleRemotes::new();
                let ret = remotes.create_remote();
                *r = Some(remotes);
                ret
            },
        }
    }

    pub fn one<IF, IE>(f: IF) -> Self
        where IF: IntoFuture<Item = Self, Error = IE> + 'static,
              IE: Into<E> + 'static,
    {
        let mut ret = Scrundle::new();
        ret.enqueue(f);
        ret
    }

    pub fn one_unit<IF, IE>(f: IF) -> Self
        where IF: IntoFuture<Item = (), Error = IE> + 'static,
              IE: Into<E> + 'static,
    {
        let mut ret = Scrundle::new();
        ret.enqueue_unit(f);
        ret
    }

    pub fn one_call<IReq>(call: IReq) -> Self
        where IReq: Into<Req>,
    {
        let mut ret = Scrundle::new();
        ret.add_call(call);
        ret
    }

    pub fn enqueue<IF, IE>(&mut self, f: IF)
        where IF: IntoFuture<Item = Self, Error = IE> + 'static,
              IE: Into<E> + 'static,
    {
        self.futures.push(Box::new({
            f.into_future()
                .map_err(Into::into)
        }));
    }

    pub fn enqueue_unit<IF: 'static, IE: 'static>(&mut self, f: IF)
        where IF: IntoFuture<Item = (), Error = IE>,
              IE: Into<E>,
    {
        self.futures.push(Box::new({
            f.into_future()
                .map(|()| Default::default())
                .map_err(Into::into)
        }));
    }

    pub fn add_call<IReq>(&mut self, call: IReq)
        where IReq: Into<Req>,
    {
        self.calls.push(call.into());
    }

    pub fn merge(&mut self, other: Self) {
        self.calls.extend(other.calls);
        self.futures.extend(other.futures);
        ScrundleRemotes::merge_opts_in_place(&mut self.remote, other.remote);
    }

    pub fn merge_left<R>(&mut self, (other, right): (Self, R)) -> R {
        self.merge(other);
        right
    }

    pub fn from_never<IE>(sl: Scrundle<Void, IE>) -> Self
        where IE: Into<E> + 'static,
    {
        let mut ret = Scrundle::new();
        ret.merge_from_never(sl);
        ret
    }

    pub fn merge_from_never<IE>(&mut self, other: Scrundle<Void, IE>)
        where IE: Into<E> + 'static,
    {
        self.futures.extend({
            other.futures
                .into_iter()
                .map(|fut| -> Box<Future<Item = Self, Error = E>> {
                    Box::new(fut.map(Scrundle::from_never).map_err(Into::into))
                })
        });
        ScrundleRemotes::merge_opts_in_place(
            &mut self.remote, other.remote.map(ScrundleRemotes::never_into));
    }

    pub fn merge_left_from_never<IE, R>(&mut self, (other, right): (Scrundle<Void, IE>, R)) -> R
        where IE: Into<E> + 'static,
    {
        self.merge_from_never(other);
        right
    }

    pub fn merge_map_err_into<E2>(&mut self, other: Scrundle<Req, E2>)
        where E2: Into<E> + 'static,
    {
        self.calls.extend(other.calls);
        self.futures.extend({
            other.futures
                .into_iter()
                .map(|fut| -> Box<Future<Item = Self, Error = E>> {
                    Box::new(fut.map(Scrundle::map_err_into).map_err(Into::into))
                })
        });
        ScrundleRemotes::merge_opts_in_place(
            &mut self.remote, other.remote.map(ScrundleRemotes::map_err_into));
    }

    pub fn map_err_into<E2>(self) -> Scrundle<Req, E2>
        where E: Into<E2>,
              E2: 'static,
    {
        let mut ret = Scrundle::new();
        ret.merge_map_err_into(self);
        ret
    }

    pub fn merge_stream<S>(&mut self, stream: S)
        where S: Stream<Item = Self, Error = E> + 'static,
    {
        let fut = stream.into_future()
            .then(|r| match r {
                Ok((None, _)) => Ok(Scrundle::new()),
                Ok((Some(mut sl), st)) => {
                    sl.merge_stream(st);
                    Ok(sl)
                }
                Err((e, _)) => Err(e),
            });
        self.enqueue(fut);
    }

    pub fn from_stream<S>(stream: S) -> Self
        where S: Stream<Item = Self, Error = E> + 'static,
    {
        let mut ret = Scrundle::new();
        ret.merge_stream(stream);
        ret
    }

    pub fn on_complete(&mut self) -> impl Future<Item = (), Error = Canceled> {
        let (tx, rx) = SendUnitOnDrop::new();
        self.futures = mem::replace(&mut self.futures, Default::default())
            .into_iter()
            .map(|fut| -> Box<Future<Item = Self, Error = E>> {
                Box::new({
                    let tx = tx.clone();
                    fut
                        .inspect(move |_| {
                            let _tx = tx;
                        })
                })
            })
            .collect();
        rx
    }

    pub fn on_complete_split(mut self) -> (Self, impl Future<Item = (), Error = Canceled>) {
        let fut = self.on_complete();
        (self, fut)
    }

    pub fn on_complete_merge(&mut self, mut other: Self) -> impl Future<Item = (), Error = Canceled> {
        let ret = other.on_complete();
        self.merge(other);
        ret
    }

    pub fn and_then_trampoline_left<Ac, F, FutIn, FutOut>(&mut self, fut: FutIn, f: F)
                                                          -> impl Future<Item = FutOut::Item, Error = FutOut::Error>
        where Ac: ScrundleCapable<Request = Req, Error = E> + 'static,
              F: FnOnce(&mut Ac, FutIn::Item) -> (Self, FutOut) + 'static,
              FutIn: IntoFuture + 'static,
              FutIn::Error: Into<E>,
              FutOut: IntoFuture + 'static,
              Trampoline<Ac>: Into<Req>,
              Canceled: Into<FutOut::Error>,
    {
        let (tx, rx) = oneshot::channel();
        self.enqueue({
            fut.into_future()
                .map(|item| -> ActorSL<Ac> {
                    Self::one_call(Trampoline::new(move |actor| {
                        let (sl, out) = f(actor, item);
                        let _ = tx.send(out);
                        sl
                    }).into())
                })
                .map_err(Into::into)
        });
        rx
            .map_err(Into::into)
            .and_then(|fut| fut.into_future())
    }

    pub fn map_trampoline_left<Ac, F, FutIn, Out>(&mut self, fut: FutIn, f: F)
                                                  -> impl Future<Item = Out, Error = Canceled>
        where Ac: ScrundleCapable<Request = Req, Error = E> + 'static,
              F: FnOnce(&mut Ac, FutIn::Item) -> (Self, Out) + 'static,
              FutIn: IntoFuture + 'static,
              FutIn::Error: Into<E>,
              Out: 'static,
              Trampoline<Ac>: Into<Req>,
    {
        self.and_then_trampoline_left(fut, move |actor, item| {
            let (sl, item) = f(actor, item);
            (sl, Ok(item))
        })
    }
}

impl<E> Scrundle<Void, E>
    where E: 'static,
{
    pub fn never_into<Req, E2>(self) -> Scrundle<Req, E2>
        where E: Into<E2>,
              Req: 'static,
              E2: 'static,
    {
        Scrundle::from_never(self)
    }
}

trait FnTrampoline<Ac>
    where Ac: ScrundleCapable,
          Ac::Error: 'static,
{
    fn call(self: Box<Self>, actor: &mut Ac) -> ActorSL<Ac>;
}

impl<Ac, F> FnTrampoline<Ac> for F
    where Ac: ScrundleCapable,
          Ac::Error: 'static,
          F: FnOnce(&mut Ac) -> ActorSL<Ac>,
{
    fn call(self: Box<F>, actor: &mut Ac) -> ActorSL<Ac> {
        (*self)(actor)
    }
}

/// A trampoline for making more requests against an actor `Ac`.
///
/// An actor accepting `Trampoline<Ac>` as a request means that `&mut Ac` can be passed back in to a later callback in
/// a chain of futures while keeping the whole chain a `'static` future.
#[derive(DebugStub)]
pub struct Trampoline<Ac>
    where Ac: ScrundleCapable,
          Ac::Error: 'static,
{
    #[debug_stub="<callback>"]
    func: Box<FnTrampoline<Ac>>,
}

impl<Ac> Trampoline<Ac>
    where Ac: ScrundleCapable,
          Ac::Error: 'static,
{
    fn new<F>(f: F) -> Self
        where F: FnOnce(&mut Ac) -> ActorSL<Ac> + 'static,
    {
        Trampoline { func: Box::new(f) }
    }

    pub fn call(self, actor: &mut Ac) -> ActorSL<Ac> {
        self.func.call(actor)
    }
}

pub trait FutureTrampolineExt: Future {
    /// Like `Future::and_then`, but also trampoline to call a callback which is passed `&mut Ac` while returning a
    /// `'static` future.
    ///
    /// The callback returns another `Scrundle` to add more work in the background, and a future whose result will
    /// become the result of the returned future.
    fn and_then_trampoline_left<Ac, F, FutOut>(self, sl: &mut ActorSL<Ac>, f: F)
                                               -> Box<Future<Item = FutOut::Item, Error = FutOut::Error>>
        where Ac: ScrundleCapable + 'static,
              Ac::Request: 'static,
              F: FnOnce(&mut Ac, Self::Item) -> (ActorSL<Ac>, FutOut) + 'static,
              FutOut: IntoFuture + 'static,
              Trampoline<Ac>: Into<Ac::Request>,
              Canceled: Into<FutOut::Error>,
              Self: 'static,
              Self::Error: Into<Ac::Error>,
    ;

    /// Like `Future::map`, but also trampoline to call a callback which is passed `&mut Ac` while returning a
    /// `'static` future.
    ///
    /// The callback returns another `Scrundle` to add more work in the background, and a value which will become the
    /// result of the returned future.
    fn map_trampoline_left<Ac, F, Out>(self, sl: &mut ActorSL<Ac>, f: F)
                                       -> Box<Future<Item = Out, Error = Canceled>>
        where Ac: ScrundleCapable + 'static,
              Ac::Request: 'static,
              F: FnOnce(&mut Ac, Self::Item) -> (ActorSL<Ac>, Out) + 'static,
              Out: 'static,
              Trampoline<Ac>: Into<Ac::Request>,
              Self: 'static,
              Self::Error: Into<Ac::Error>,
    ;
}

impl<FutIn> FutureTrampolineExt for FutIn
    where FutIn: Future,
{
    fn and_then_trampoline_left<Ac, F, FutOut>(self, sl: &mut ActorSL<Ac>, f: F)
                                               -> Box<Future<Item = FutOut::Item, Error = FutOut::Error>>
        where Ac: ScrundleCapable + 'static,
              Ac::Request: 'static,
              F: FnOnce(&mut Ac, Self::Item) -> (ActorSL<Ac>, FutOut) + 'static,
              FutOut: IntoFuture + 'static,
              Trampoline<Ac>: Into<Ac::Request>,
              Canceled: Into<FutOut::Error>,
              Self: 'static,
              Self::Error: Into<Ac::Error>,
    {
        Box::new(Scrundle::and_then_trampoline_left(sl, self, f))
    }

    fn map_trampoline_left<Ac, F, Out>(self, sl: &mut ActorSL<Ac>, f: F)
                                       -> Box<Future<Item = Out, Error = Canceled>>
        where Ac: ScrundleCapable + 'static,
              Ac::Request: 'static,
              F: FnOnce(&mut Ac, Self::Item) -> (ActorSL<Ac>, Out) + 'static,
              Out: 'static,
              Trampoline<Ac>: Into<Ac::Request>,
              Self: 'static,
              Self::Error: Into<Ac::Error>,
    {
        Box::new(Scrundle::map_trampoline_left(sl, self, f))
    }
}

pub trait FutureScrundleExt<Ac>: Future
    where Ac: ScrundleCapable<Error = Self::Error>,
          Ac::Error: 'static,
{
    fn enqueue_self(self, sl: &mut ActorSL<Ac>)
        where Self: Future<Item = ActorSL<Ac>>,
    ;

    fn enqueue_unit_self(self, sl: &mut ActorSL<Ac>)
        where Self: Future<Item = ()>,
    ;
}

impl<Ac, FutIn> FutureScrundleExt<Ac> for FutIn
    where Ac: ScrundleCapable<Error = FutIn::Error>,
          Ac::Request: 'static,
          Ac::Error: 'static,
          FutIn: Future + 'static,
{
    fn enqueue_self(self, sl: &mut ActorSL<Ac>)
        where Self: Future<Item = ActorSL<Ac>>,
    {
        sl.enqueue(self)
    }

    fn enqueue_unit_self(self, sl: &mut ActorSL<Ac>)
        where Self: Future<Item = ()>,
    {
        sl.enqueue_unit::<Self, Self::Error>(self)
    }
}

pub struct RemoteScrundle<Req, E>
    where Req: 'static,
          E: 'static,
{
    tx: mpsc::Sender<Scrundle<Req, E>>,
}

impl<Req, E> Clone for RemoteScrundle<Req, E>
    where Req: 'static,
          E: 'static,
{
    fn clone(&self) -> Self {
        RemoteScrundle {
            tx: self.tx.clone(),
        }
    }
}

impl<Req, E> Sink for RemoteScrundle<Req, E>
    where Req: 'static,
          E: 'static,
{
    type SinkItem = Scrundle<Req, E>;
    type SinkError = mpsc::SendError<Self::SinkItem>;

    fn start_send(&mut self, item: Self::SinkItem) -> StartSend<Self::SinkItem, Self::SinkError> {
        self.tx.start_send(item)
    }

    fn poll_complete(&mut self) -> Poll<(), Self::SinkError> {
        self.tx.poll_complete()
    }
}

pub struct ScrundleDrainingFuture<Ac>
    where Ac: 'static + ScrundleCapable
{
    further_calls_tx: mpsc::Sender<FurtherCall<Ac>>,
    further_calls: VecDeque<Ac::Request>,
    active: stream::FuturesUnordered<NextScrundleMaybeStreamFuture<Ac::Request, Ac::Error>>,
}

impl<Ac> ScrundleDrainingFuture<Ac>
    where Ac: ScrundleCapable + 'static,
          Ac::Request: fmt::Debug,
          Ac::Error: From<Canceled>,
{
    fn new(further_calls_tx: mpsc::Sender<FurtherCall<Ac>>, calls: Ac::Iter) -> Self {
        Self {
            further_calls_tx,
            further_calls: calls.into_iter().collect(),
            active: stream::FuturesUnordered::new(),
        }
    }

    fn merge_scrundle(&mut self, mut sl: ActorSL<Ac>) {
        self.further_calls.extend(sl.calls);
        self.merge_futures(sl.futures);
        if let Some(remote) = sl.remote.take() {
            self.active.push(Box::new({
                stream_into_step_future(Box::new(remote.into_scrundle_stream()))
                    .map(|(sc, st)| (sc, Some(st)))
            }))
        }
    }

    fn merge_futures<I, IF>(&mut self, futures: I)
        where I: IntoIterator<Item = IF>,
              IF: IntoFuture<Item = ActorSL<Ac>, Error = Ac::Error> + 'static,
    {
        for fut in futures {
            self.active.push(Box::new({
                fut.into_future()
                    .map(|s| (Some(s), None))
            }))
        }
    }

    fn do_calls_send(&mut self) -> Poll<(), mpsc::SendError<FurtherCall<Ac>>> {
        while let Some(req) = self.further_calls.pop_front() {
            let (tx, rx) = oneshot::channel();
            match self.further_calls_tx.start_send((req, tx))? {
                AsyncSink::Ready => {
                    self.active.push(Box::new({
                        rx.map_err(Into::into)
                            .and_then(|f| f)
                            .map(|sc| (Some(sc), None))
                    }));
                }
                AsyncSink::NotReady((req, _)) => {
                    self.further_calls.push_front(req);
                    break;
                },
            }
        }
        let () = try_ready!(self.further_calls_tx.poll_complete());
        Ok(Async::Ready(()))
    }

    fn do_poll(&mut self) -> Poll<(), Ac::Error> {
        let () = try_ready!(self.do_calls_send().map_err(|_| Canceled.into()));
        while let Some((scrundle_opt, stream_opt)) = try_ready!(self.active.poll()) {
            if let Some(scrundle) = scrundle_opt {
                self.merge_scrundle(scrundle);
            }
            if let Some(stream) = stream_opt {
                self.active.push(Box::new({
                    stream_into_step_future(stream)
                        .map(|(sc, st)| (sc, Some(st)))
                }));
            }
        }
        Ok(Async::Ready(()))
    }
}

impl<Ac> Future for ScrundleDrainingFuture<Ac>
    where Ac: ScrundleCapable + 'static,
          Ac::Request: fmt::Debug,
          Ac::Error: From<Canceled>,
{
    type Item = ();
    type Error = Ac::Error;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        self.do_poll()
    }
}

pub struct ScrundleActor<Ac: 'static + ScrundleCapable> {
    actor: Ac,
    further_calls_tx: mpsc::Sender<FurtherCall<Ac>>,
    further_calls_rx: mpsc::Receiver<FurtherCall<Ac>>,
}

impl<Ac> ScrundleActor<Ac>
    where Ac: ScrundleCapable + 'static,
          Ac::Request: fmt::Debug,
          Ac::Error: From<kabuki::CallError<Ac::Iter>> + From<Canceled>,
{
    pub fn new(actor: Ac, buffer: usize) -> Self {
        let (further_calls_tx, further_calls_rx) = mpsc::channel(buffer);
        ScrundleActor { actor, further_calls_tx, further_calls_rx }
    }

    fn poll_further_calls(&mut self) -> kabuki::ActorState {
        loop {
            match self.further_calls_rx.poll() {
                Ok(Async::Ready(Some((req, tx)))) => {
                    if !tx.is_canceled() {
                        let resp = self.actor.call(req);
                        let _ = tx.send(resp);
                    }
                },
                Ok(Async::Ready(None)) => return kabuki::ActorState::Finalizing,
                Ok(Async::NotReady) => return kabuki::ActorState::Listening,
                Err(()) => unreachable!(),
            }
        }
    }
}

impl<Ac> Actor for ScrundleActor<Ac>
    where Ac: ScrundleCapable + 'static,
          Ac::Request: fmt::Debug,
          Ac::Error: From<kabuki::CallError<Ac::Iter>> + From<Canceled>,
{
    type Request = Ac::Iter;
    type Response = ();
    type Error = Ac::Error;
    type Future = ScrundleDrainingFuture<Ac>;

    fn call(&mut self, reqs: Self::Request) -> Self::Future {
        ScrundleDrainingFuture::new(self.further_calls_tx.clone(), reqs)
    }

    fn poll(&mut self, mut state: kabuki::ActorState) -> Async<()> {
        if self.poll_further_calls() == kabuki::ActorState::Finalizing {
            state = kabuki::ActorState::Finalizing;
        }
        self.actor.poll(state)
    }

    fn poll_ready(&mut self) -> Async<()> {
        self.actor.poll_ready()
    }
}
