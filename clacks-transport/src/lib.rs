#![recursion_limit = "128"]

#[macro_use] extern crate error_chain;
extern crate byteorder;
extern crate bytes;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate rand;
extern crate tokio_io;

pub mod error {
    error_chain! {
        links {
            Crypto(::clacks_crypto::error::Error, ::clacks_crypto::error::ErrorKind);
            Mtproto(::clacks_mtproto::error::Error, ::clacks_mtproto::error::ErrorKind);
        }

        foreign_links {
            Io(::std::io::Error);
            Utf8(::std::str::Utf8Error);
            FromUtf8(::std::string::FromUtf8Error);
        }

        errors {
            InvalidData {}
            BoxedAsBare {}
            ReceivedSendType {}
            UnsupportedLayer {}
            NoAuthKey {}
            NoSalts {}
            WrongAuthKey {}
            InvalidLength {}
            Unknown {}
            FactorizationFailure {}
            AuthenticationFailure {}
        }
    }
}

mod codec;
pub use codec::TelegramCodec;

pub mod session;
