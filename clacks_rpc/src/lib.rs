#![deny(private_in_public, unused_extern_crates)]
#![recursion_limit = "128"]

#[macro_use] extern crate error_chain;
#[macro_use] extern crate kabuki_extras;
#[macro_use] extern crate slog;
extern crate clacks_crypto;
extern crate clacks_mtproto;
extern crate clacks_transport;
extern crate flate2;
extern crate futures;
extern crate kabuki;
extern crate tokio_io;
extern crate tokio_service;

pub mod error {
    error_chain! {
        links {
            Crypto(::clacks_crypto::error::Error, ::clacks_crypto::error::ErrorKind);
            Mtproto(::clacks_mtproto::error::Error, ::clacks_mtproto::error::ErrorKind);
            Transport(::clacks_transport::error::Error, ::clacks_transport::error::ErrorKind);
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
            WrongReplyType(obj: ::clacks_mtproto::mtproto::TLObject) {}
            DuplicateMessageId {}
            ConnectionClosed {}
            RpcError(err: ::clacks_mtproto::mtproto::RpcError) {}
        }
    }

    error_chain_extras!(Error, ErrorKind::Msg);

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

    impl<T> From<::kabuki::CallError<T>> for Error {
        fn from(_: ::kabuki::CallError<T>) -> Self {
            ErrorKind::Msg("XXX".to_string()).into()
        }
    }

    impl From<::kabuki_extras::SinkErrored<Self>> for Error {
        fn from(e: ::kabuki_extras::SinkErrored<Self>) -> Self {
            Self::with_chain(e.0, ErrorKind::SinkErrored)
        }
    }

    impl From<::kabuki_extras::SinkFull<::clacks_transport::session::OutboundMessage>> for Error {
        fn from(_: ::kabuki_extras::SinkFull<::clacks_transport::session::OutboundMessage>) -> Self {
            ErrorKind::SinkFull.into()
        }
    }
}

pub mod client;
