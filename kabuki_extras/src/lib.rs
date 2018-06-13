#![deny(private_in_public, unused_extern_crates)]

#[macro_use] extern crate delegate;
#[macro_use] extern crate failure;
#[macro_use] extern crate futures;
#[macro_use] extern crate slog;
#[macro_use] extern crate debug_stub_derive;
extern crate bytes;
extern crate kabuki;
//extern crate smallvec;
extern crate tokio_service;
extern crate tokio_io;
extern crate tokio_tcp;
extern crate tokio_uds;
//extern crate void;

pub mod ext_traits;

//mod scrundle;
//pub use scrundle::*;

mod service;
pub use service::*;

mod sockets;
pub use sockets::*;

mod streams;
pub use streams::*;
