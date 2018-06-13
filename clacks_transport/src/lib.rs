#![deny(private_in_public, unused_extern_crates)]

#[macro_use] extern crate failure;
extern crate byteorder;
extern crate bytes;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate either;
extern crate rand;
extern crate serde_json;
extern crate tokio_io;

pub type Result<T> = ::std::result::Result<T, ::failure::Error>;

mod codec;
pub use codec::TelegramCodec;

pub mod session;
pub use session::{AppId, Session};
