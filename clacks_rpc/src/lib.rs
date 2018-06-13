#![cfg_attr(feature = "kex", feature(proc_macro, proc_macro_non_items, generators))]
#![deny(private_in_public, unused_extern_crates)]

#[macro_use] extern crate failure;
#[macro_use] extern crate kabuki_extras;
#[macro_use] extern crate slog;
extern crate byteorder;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate clacks_transport;
extern crate futures_cpupool;
extern crate kabuki;
extern crate tokio_io;
extern crate tokio_service;


#[cfg(not(feature = "kex"))]
extern crate futures;
#[cfg(feature = "kex")]
extern crate futures_await as futures;

pub mod client;
#[cfg(feature = "kex")]
pub mod kex;

pub type Result<T> = ::std::result::Result<T, ::failure::Error>;
