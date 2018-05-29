#![deny(private_in_public, unused_extern_crates)]
#![recursion_limit = "128"]

#[macro_use] extern crate error_chain;
#[macro_use] extern crate slog;
extern crate byteorder;
extern crate bytes;
extern crate chrono;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate futures;
extern crate kabuki;
extern crate kabuki_extras;
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
            ErrorCode(code: i32) {}
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
            Canceled {}
            SinkErrored {}
            SinkFull {}
            ExecuteFailed(kind: ::futures::future::ExecuteErrorKind) {}
        }
    }

    impl From<::futures::Canceled> for Error {
        fn from(_: ::futures::Canceled) -> Self {
            ErrorKind::Canceled.into()
        }
    }

    impl From<::futures::future::ExecuteErrorKind> for Error {
        fn from(k: ::futures::future::ExecuteErrorKind) -> Self {
            ErrorKind::ExecuteFailed(k).into()
        }
    }

    impl From<::kabuki_extras::SinkErrored<Self>> for Error {
        fn from(e: ::kabuki_extras::SinkErrored<Self>) -> Self {
            Self::with_chain(e.0, ErrorKind::SinkErrored)
        }
    }

    impl From<::kabuki_extras::SinkFull<::session::OutboundMessage>> for Error {
        fn from(_: ::kabuki_extras::SinkFull<::session::OutboundMessage>) -> Self {
            ErrorKind::SinkFull.into()
        }
    }

    impl<T> From<::kabuki::CallError<T>> for Error {
        fn from(_: ::kabuki::CallError<T>) -> Self {
            ErrorKind::Msg("XXX".to_string()).into()
        }
    }
}

mod codec;
pub use codec::TelegramCodec;

pub mod session;
pub use session::{AppId, Session};
