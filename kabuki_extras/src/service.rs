use futures::Future;
use tokio_service::Service;


pub type BoxService<Req, Res, E> = Box<
    Service<Request = Req, Response = Res, Error = E, Future = Box<Future<Item = Res, Error = E>>>>;

pub struct ErasedService<Se>(Se);

impl<Se> ErasedService<Se>
    where Se: Service + 'static,
          Se::Future: 'static,
{
    pub fn new_erased(service: Se) -> BoxService<Se::Request, Se::Response, Se::Error> {
        Box::new(ErasedService(service))
    }
}

impl<Se> Service for ErasedService<Se>
    where Se: Service,
          Se::Future: 'static,
{
    type Request = Se::Request;
    type Response = Se::Response;
    type Error = Se::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error>>;

    fn call(&self, req: Self::Request) -> Self::Future {
        Box::new(self.0.call(req))
    }
}
