extern crate jsonrpc_core;

use std::time::Instant;
use std::sync::atomic::{self, AtomicUsize};
use jsonrpc_core::*;
use jsonrpc_core::futures::Future;

#[derive(Clone, Debug)]
struct Meta(usize);
impl Metadata for Meta {}

#[derive(Default)]
struct MyMiddleware(AtomicUsize);
impl Middleware<Meta> for MyMiddleware {
	type Future = FutureResponse;

	fn on_request<F, X>(&self, request: Request, meta: Meta, next: F) -> FutureResponse where
		F: FnOnce(Request, Meta) -> X + Send,
		X: Future<Item=Option<Response>, Error=()> + Send + 'static,
	{
		let start = Instant::now();
		let request_number = self.0.fetch_add(1, atomic::Ordering::SeqCst);
		println!("Processing request {}: {:?}, {:?}", request_number, request, meta);

		Box::new(next(request, meta).map(move |res| {
			println!("Processing took: {:?}", start.elapsed());
			res
		}))
	}
}

pub fn main() {
	let mut io = MetaIoHandler::with_middleware(MyMiddleware::default());

	io.add_method_with_meta("say_hello", |_params: Params, meta: Meta| {
		Ok(Value::String(format!("Hello World: {}", meta.0)))
	});

	let request = r#"{"jsonrpc": "2.0", "method": "say_hello", "params": [42, 23], "id": 1}"#;
	let response = r#"{"jsonrpc":"2.0","result":"Hello World: 5","id":1}"#;

	let headers = 5;
	assert_eq!(
		io.handle_request(request, Meta(headers)).wait().unwrap(),
		Some(response.to_owned())
	);
}
