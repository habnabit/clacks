use bytes;
use std::{io, net, ops};
use futures;
use tokio_io;
use tokio_tcp::TcpStream;


pub struct RealShutdownTcpStream(TcpStream);

impl From<TcpStream> for RealShutdownTcpStream {
    fn from(s: TcpStream) -> Self { RealShutdownTcpStream(s) }
}

impl ops::Deref for RealShutdownTcpStream {
    type Target = TcpStream;
    fn deref(&self) -> &TcpStream { &self.0 }
}

impl io::Read for RealShutdownTcpStream {
    delegate! {
        target self.0 {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize>;
        }
    }
}

impl io::Write for RealShutdownTcpStream {
    delegate! {
        target self.0 {
            fn write(&mut self, buf: &[u8]) -> io::Result<usize>;
            fn flush(&mut self) -> io::Result<()>;
        }
    }
}

impl tokio_io::AsyncRead for RealShutdownTcpStream {
    delegate! {
        target self.0 {
            unsafe fn prepare_uninitialized_buffer(&self, buf: &mut [u8]) -> bool;
            fn read_buf<B: bytes::BufMut>(&mut self, buf: &mut B) -> futures::Poll<usize, io::Error>;
            fn poll_read(&mut self, buf: &mut [u8]) -> futures::Poll<usize, io::Error>;
        }
    }
}

impl tokio_io::AsyncWrite for RealShutdownTcpStream {

    fn shutdown(&mut self) -> futures::Poll<(), io::Error> {
        self.0.shutdown(net::Shutdown::Write).map(futures::Async::Ready)
    }

    delegate! {
        target self.0 {
            fn write_buf<B: bytes::Buf>(&mut self, buf: &mut B) -> futures::Poll<usize, io::Error>;
        }
    }
}
