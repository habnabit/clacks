#![deny(private_in_public, unused_extern_crates)]

#[macro_use] extern crate delegate;
#[macro_use] extern crate futures;
#[macro_use] extern crate slog;
#[macro_use] extern crate debug_stub_derive;
extern crate bytes;
extern crate kabuki;
extern crate smallvec;
extern crate tokio_service;
extern crate tokio_io;
extern crate tokio_tcp;
extern crate tokio_uds;
extern crate void;

pub mod ext_traits;

mod scrundle;
pub use scrundle::*;

mod service;
pub use service::*;

mod sockets;
pub use sockets::*;

mod streams;
pub use streams::*;


#[macro_export]
macro_rules! error_chain_extras {
    ($Error:ident, $Msg:path) => {

        use futures::Future;
        use std::borrow::Borrow;

        #[allow(dead_code)]
        pub type BoxFuture<T> = Box<Future<Item = T, Error = $Error>>;

        pub fn into_error<E>(e: E) -> $Error
            where E: Into<$Error>,
        {
            e.into()
        }

        pub trait LocalFuture<T>: Future<Item = T, Error = $Error> {}
        impl<T, F> LocalFuture<T> for F
            where F: Future<Item = T, Error = $Error>,
        {}

        pub trait FutureExtInto: Future {
            fn map_err_into<E>(self) -> BoxFuture<Self::Item>
                where E: Into<$Error>,
                      Self: Sized,
            ;
        }

        pub trait FutureExtUnit: Future<Error = ()> {
            fn map_unit_err<S>(self, description: S) -> BoxFuture<Self::Item>
                where S: Borrow<str>,
                      Self: Sized,
            ;
        }

        impl<F, It> FutureExtInto for F
            where F: Future<Item = It, Error = $Error> + 'static,
        {
            fn map_err_into<E>(self) -> BoxFuture<Self::Item>
                where E: Into<$Error>,
                      Self: Sized,
            {
                Box::new(self.map_err(into_error))
            }
        }

        impl<F, It> FutureExtUnit for F
            where F: Future<Item = It, Error = ()> + 'static,
        {
            fn map_unit_err<S>(self, description: S) -> BoxFuture<Self::Item>
                where S: Borrow<str>,
                      Self: Sized,
            {
                let description = description.borrow().into();
                Box::new({
                    self.map_err(|()| $Msg(description).into())
                })
            }
        }

    }
}
