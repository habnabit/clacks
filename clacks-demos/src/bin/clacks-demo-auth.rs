extern crate clacks_mtproto;
extern crate clacks_transport;
extern crate futures;
extern crate rand;
extern crate tokio;
extern crate tokio_io;

use clacks_mtproto::mtproto;
use futures::{Future, Sink, Stream, future};
use rand::Rng;


fn into_error<T>(x: T) -> clacks_transport::error::Error
    where T: Into<clacks_transport::error::Error>
{
    x.into()
}

fn main_future() -> Box<Future<Item = (), Error = ()> + Send> {
    let mut rng = rand::OsRng::new().unwrap();
    let mut session = clacks_transport::session::Session::new(0, clacks_transport::session::AppId {
        api_id: 0, api_hash: "".into(),
    });
    let fut = tokio::net::TcpStream::connect(&"149.154.167.50:443".parse().unwrap())
        .map_err(into_error)
        .and_then(move |stream| {
            use tokio_io::AsyncRead;
            let (tx, rx) = stream.framed(clacks_transport::TelegramCodec::new()).split();
            rx
                .for_each(|v| {
                    println!("got: {:?}", v);
                    Ok(())
                })
                .map_err(into_error)
                .join({
                    future::result(session.plain_payload(mtproto::rpc::req_pq {
                        nonce: rng.gen(),
                    }))
                        .and_then(|msg| tx.send(msg.message).map_err(into_error))
                        .map(|_| ())
                })
        })
        .map(|((), ())| ())
        .map_err(|e| panic!("error: {:?}", e));
    Box::new(fut)
}

fn main() {
    tokio::run(main_future())
}
