#![cfg(feature = "hyper")]

use std::convert::Infallible;
use std::task::Poll;
use std::task;
use std::future::Future;
use std::pin::Pin;
use std::net::SocketAddr;

use hyper::{Body, Server};
use hyper::server::conn::AddrStream;
use hyper::service::{Service, service_fn, make_service_fn};

use plumb::{Pipe,PipeExt};

pub type Request = http::Request<Body>;
pub type Response = http::Response<Body>;
// pub type Mux = crate::mux::Mux<Body, Response>;

pub struct PipeService<P>(pub P);

impl<P, E> Service<Request> for &PipeService<P>
where
    P : Pipe<Input = (Request,), Output = Result<Response, E>>,
    E : std::error::Error + Send + Sync,
{
    type Response = Response;
    type Error = E;
    type Future = Pin<Box<dyn Future<Output = Result<Response, E>> + Send>>;

    fn poll_ready(
        &mut self,
        _cx : &mut task::Context<'_>
    ) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req : Request) -> Self::Future {
        self.0.run((req,))
    }
}

pub async fn serve<P>(pipe : P, addr : &std::net::SocketAddr) -> hyper::Result<()>
where
    P : Pipe<Input = (Request,), Output = Response> + Send + Sync + 'static,
{
    let pipe = pipe.tuple().seq(|x| Ok::<Response, Infallible>(x));

    let pipe_service : &'static _ = Box::leak(Box::new(PipeService(pipe)));

    let make_service = make_service_fn(|_conn| async move {
        Ok::<_, Infallible>(pipe_service)
    });

    Server::bind(addr).serve(make_service).await
}

pub async fn serve_addr<P>(pipe : P, addr : &SocketAddr) -> hyper::Result<()>
where
    P : Pipe<Input = (SocketAddr, Request), Output = Response> + Send + Sync + 'static,
{

    // let pipe_service : &'static _ = Box::leak(Box::new(PipeService(pipe)));

    let pipe : &'static _ = Box::leak(Box::new(pipe
        .tuple().seq(|res| Ok::<Response, Infallible>(res))));


    let make_service = make_service_fn(move |conn : &AddrStream| {
        let addr = conn.remote_addr();
        async move {
            Ok::<_, Infallible>(service_fn(move |req| {
                pipe.run(
                    (addr, req)
                )
            }))
        }
    });

    Server::bind(addr).serve(make_service).await
}
