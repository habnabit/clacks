use kabuki;
use futures::unsync::oneshot;
use futures::{Canceled, Future};
use tokio_service::Service;


pub trait AskActor {
    type Request;
    type Error;

    fn ask<Q, FS, A, F, I>(&self, constructor: F, question: Q) -> Box<Future<Item = A, Error = Self::Error>>
        where F: FnOnce(Q, FS) -> I,
              FS: From<oneshot::Sender<A>>,
              A: 'static,
              I: Into<Self::Request>,
    ;
}

impl<T, E, Se> AskActor for Se
    where T: 'static,
          E: From<Canceled> + From<kabuki::CallError<T>> + 'static,
          Se: Service<Request = T, Response = (), Error = E>,
          Se::Future: 'static,
{
    type Request = T;
    type Error = E;

    fn ask<Q, FS, A, F, I>(&self, constructor: F, question: Q) -> Box<Future<Item = A, Error = Self::Error>>
        where F: FnOnce(Q, FS) -> I,
              FS: From<oneshot::Sender<A>>,
              A: 'static,
              I: Into<Self::Request>,
    {
        let (tx, rx) = oneshot::channel();
        Box::new({
            self.call(constructor(question, tx.into()).into())
                .and_then(move |()| rx.map_err(Into::into))
        })
    }
}
