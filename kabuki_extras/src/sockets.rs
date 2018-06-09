use bytes;
use std::{io, net, ops};
use futures;
use tokio_io;
use tokio_tcp::TcpStream;
use tokio_uds::UnixStream;


pub struct RealShutdown<S>(S);

impl<S> From<S> for RealShutdown<S> {
    fn from(s: S) -> Self { RealShutdown(s) }
}

impl<S> ops::Deref for RealShutdown<S> {
    type Target = S;
    fn deref(&self) -> &S { &self.0 }
}

impl<S: io::Read> io::Read for RealShutdown<S> {
    delegate! {
        target self.0 {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize>;
        }
    }
}

impl<S: io::Write> io::Write for RealShutdown<S> {
    delegate! {
        target self.0 {
            fn write(&mut self, buf: &[u8]) -> io::Result<usize>;
            fn flush(&mut self) -> io::Result<()>;
        }
    }
}

impl<S: tokio_io::AsyncRead> tokio_io::AsyncRead for RealShutdown<S> {
    delegate! {
        target self.0 {
            unsafe fn prepare_uninitialized_buffer(&self, buf: &mut [u8]) -> bool;
            fn read_buf<B: bytes::BufMut>(&mut self, buf: &mut B) -> futures::Poll<usize, io::Error>;
            fn poll_read(&mut self, buf: &mut [u8]) -> futures::Poll<usize, io::Error>;
        }
    }
}

pub trait HasRealShutdown {
    fn real_shutdown(&self, how: net::Shutdown) -> io::Result<()>;
}

impl<S> tokio_io::AsyncWrite for RealShutdown<S>
    where S: tokio_io::AsyncWrite,
          Self: HasRealShutdown,
{
    fn shutdown(&mut self) -> futures::Poll<(), io::Error> {
        self.real_shutdown(net::Shutdown::Write).map(futures::Async::Ready)
    }

    delegate! {
        target self.0 {
            fn write_buf<B: bytes::Buf>(&mut self, buf: &mut B) -> futures::Poll<usize, io::Error>;
        }
    }
}

impl HasRealShutdown for RealShutdown<TcpStream> {
    delegate! {
        target self.0 {
            #[target_method(shutdown)]
            fn real_shutdown(&self, how: net::Shutdown) -> io::Result<()>;
        }
    }
}

impl HasRealShutdown for RealShutdown<UnixStream> {
    delegate! {
        target self.0 {
            #[target_method(shutdown)]
            fn real_shutdown(&self, how: net::Shutdown) -> io::Result<()>;
        }
    }
}
