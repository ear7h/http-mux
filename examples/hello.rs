use std::net::SocketAddr;

use hyper::Body;

use plumb::{Pipe, PipeExt};

use http_mux::route;
use http_mux::mux;

type Request = http::Request<Body>;
type Response = http::Response<Body>;

#[tokio::main]
async fn main() {
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    if let Err(e) = http_mux::hyper::serve(build_mux(), &addr).await {
        eprintln!("server error: {}", e);
    }
}

trait Render {
    fn render(&self) -> Response;
}

impl Render for String {
    fn render(&self) -> Response {
        Response::new(Body::from(self.clone()))
    }
}

impl Render for &'static str {
    fn render(&self) -> Response {
        Response::new(Body::from(*self))
    }
}

struct Sum {
    left : u32,
    right : u32,
}

impl Render for Sum {
    fn render(&self) -> Response {
        Response::new(Body::from(format!(
            "{} + {} = {}",
            self.left,
            self.right,
            self.left + self.right,
        )))
    }
}


fn build_mux() -> impl Pipe<Input=(Request,), Output=Response> {
    // let mut mux = Mux::<Body, Box<dyn Render>>::new();
    // let mut mux = new_mux::new_mux(); //Mux::<Body, Box<dyn Render>>::new();

    mux::new_mux::<http_mux::mux::MuxError, _, _>().handle_fn(
        route!(GET / "hello"),
        |_req | async {
            Ok(Box::new("Hello, World!") as Box<dyn Render>)
        }
    ).handle(
        route!(GET / "hello" / String),
        mux::new_handler()
        .map(|_req, name| {
            Box::new(format!("Hello, {}!", name)) as Box<dyn Render>
        })
    ).handle(
        route!(GET / "add" / u32 / u32),
        mux::new_handler()
        .map(|_req, a, b| {
            Box::new(Sum{
                left : a,
                right : b
            }) as Box<dyn Render>
        })
    // handle the errors
    ).map_tuple()
    .map(|x : Box<dyn Render>| x.render())
    .tuple()
    .seq(|res| {
        match res {
            Err(e) => {
                let s = format!("{}", e);
                Response::new(Body::from(s))
            },
            Ok(res) => res,
        }
    })
}

