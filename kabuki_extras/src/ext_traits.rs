use kabuki;
use failure::{self, Error};
use futures::unsync::oneshot;
use futures::{self, Future};
use std::fmt;
use tokio_service::Service;


pub type BoxFuture<T> = Box<Future<Item = T, Error = Error>>;

pub fn into_error<E>(e: E) -> Error
    where E: Into<Error>,
{
    e.into()
}

pub trait FailureFuture<T>: Future<Item = T, Error = Error> {}
impl<T, F> FailureFuture<T> for F
    where F: Future<Item = T, Error = Error>,
{}

pub trait FutureExtInto: Future {
    fn map_err_into<E>(self) -> BoxFuture<Self::Item>
        where E: Into<Error>,
                Self: Sized,
    ;
}

impl<F, It> FutureExtInto for F
    where F: Future<Item = It, Error = Error> + 'static,
{
    fn map_err_into<E>(self) -> BoxFuture<Self::Item>
        where E: Into<Error>,
                Self: Sized,
    {
        Box::new(self.map_err(into_error))
    }
}

pub trait AskActor {
    type Request;

    fn ask<Q, FS, A, F, I>(&self, constructor: F, question: Q) -> Box<Future<Item = A, Error = Error>>
        where F: FnOnce(Q, FS) -> I,
              FS: From<oneshot::Sender<A>>,
              A: 'static,
              I: Into<Self::Request>,
    ;
}

impl<T, Se> AskActor for Se
    where T: 'static,
          Se: Service<Request = T, Response = (), Error = Error>,
          Se::Future: 'static,
{
    type Request = T;

    fn ask<Q, FS, A, F, I>(&self, constructor: F, question: Q) -> Box<Future<Item = A, Error = Error>>
        where F: FnOnce(Q, FS) -> I,
              FS: From<oneshot::Sender<A>>,
              A: 'static,
              I: Into<Self::Request>,
    {
        let (tx, rx) = oneshot::channel();
        Box::new({
            self.call(constructor(question, tx.into()).into())
                .and_then(move |()| rx.map_err(::ext_traits::into_error))
        })
    }
}

#[derive(Debug, Fail)]
#[fail(display = "future canceled due to channel close")]
pub struct Canceled;

impl From<futures::Canceled> for Canceled {
    fn from(_: futures::Canceled) -> Self {
        Canceled
    }
}

impl Canceled {
    pub fn as_error(_: futures::Canceled) -> Error {
        Canceled.into()
    }
}
