use std::fmt;
use std::pin::Pin;
use std::future::Future;
use std::marker::PhantomData;
use std::sync::Arc;
use std::convert::TryFrom;

use http::{Uri, Request};
use plumb::{Pipe,PipeExt};
use plumb::tuple_utils::{Prepend, Pluck, Merge, Call};

use crate::route::{
    PathParser,
    Route,
    RouteUntyped,
    PathParseError,
};

type PinBoxFut<T> = Pin<Box<dyn Future<Output = T> + Send>>;

#[derive(Debug)]
pub enum MuxError {
    NotFound(String),
    MethodNotAllowed(http::Method, String),
    Parse(String, PathParseError),
}

impl std::error::Error for MuxError {}

impl fmt::Display for MuxError {
    fn fmt(&self, f : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use MuxError::*;
        match self {
            NotFound(s) => write!(f, "no handler found for path {}", s),
            MethodNotAllowed(m, s) => write!(f, "method {} not allowe for path {}", m, s),
            Parse(s, e) => write!(f, "failed to parse path {}, {}", s, e),
        }
    }
}

/// Convenience function for putting the input into a `Result`, allowing
/// subsequent calls to functions like `and_then` to Just Work.
pub fn new_handler<E, I>() -> impl Pipe<Input = I, Output = Result<I, E>>
where
    I : Send + 'static,
    E : 'static,
{
    plumb::id().tuple().seq(Ok)
}

trait HandlerT<E, I, ReqB, Res>
where
    E : From<MuxError>,
    I : Prepend<Request<ReqB>>,
{
    fn serve(
        &self,
        input : I::Output,
    ) -> PinBoxFut<Result<Res, E>>;
}

struct HandlerFn<F, T, Fut> {
    f : F,
    _marker : PhantomData<fn() -> Fut>,
    route : Route<T>,
}

impl<E, ReqB, Res, I, F, Fut, T> HandlerT<E, I, ReqB, Res> for HandlerFn<F, T, Fut>
where
    E : From<MuxError> + 'static,
    Res : 'static,
    I : Prepend<Request<ReqB>>,
    <I as Prepend<Request<ReqB>>>::Output : Merge<T>,
    F : Call<
        <I::Output as Merge<T>>::Output,
        Fut,
    > + Clone,
    Fut : Future<Output = Result<Res, E>> + Send + 'static,
    T : for<'a, 'b> TryFrom<PathParser<'a, 'b>, Error = PathParseError>,
{
    fn serve(
        &self,
        po : I::Output,
    ) -> PinBoxFut<Result<Res, E>> {
        let (req, po_tail) = po.pluck();

        let res = T::try_from(self.route.parser(req.uri().path()))
            .map_err(|e| {
                MuxError::Parse(
                    req.uri().path().to_string(),
                    e
                ).into()
            })
            .map(|t| {
                Call::call(
                    self.f.clone(),
                    po_tail.prepend(req).merge(t),
                )
            });

        Box::pin(async move {
            res?.await
        })
    }
}

struct HandlerPipe<P, T> {
    pipe : P,
    route : Route<T>,
}

impl<E, ReqB, Res, I, P, T> HandlerT<E, I, ReqB, Res> for HandlerPipe<P, T>
where
    E : From<MuxError> + 'static,
    Res : 'static,
    I : Prepend<Request<ReqB>>,
    <I as Prepend<Request<ReqB>>>::Output : Merge<T>,
    P : Pipe<
        Input = <I::Output as Merge<T>>::Output,
        Output = Result<Res, E>,
    >,
    T : for<'a, 'b> TryFrom<PathParser<'a, 'b>, Error = PathParseError>,
{
    fn serve(
        &self,
        po : I::Output,
    ) -> PinBoxFut<Result<Res, E>> {
        let (req, po_tail) = po.pluck();

        let res = T::try_from(self.route.parser(req.uri().path()))
            .map_err(|e| {
                MuxError::Parse(
                    req.uri().path().to_string(),
                    e,
                ).into()
            })
            .map(|t| {
                self.pipe.run(po_tail.prepend(req).merge(t))
            });

        Box::pin(async move {
            res?.await
        })
    }
}

struct Handler<E, I, ReqB, Res> {
    route : RouteUntyped,
    handler : Arc<dyn HandlerT<E, I, ReqB, Res> + Send + Sync>
}

pub struct Mux<E, I, ReqB, Res>
where
    E : From<MuxError>,
    I : Prepend<Request<ReqB>>,
{
    _error : PhantomData<fn () -> E>,
    _input : PhantomData<fn (I)>,
    handlers : Vec<Handler<E, I, ReqB, Res>>,
}

pub fn new_mux<E, ReqB, Res>() -> Mux<
    E,
    (),
    ReqB,
    Res
>
where
    E : From<MuxError> + 'static,
    ReqB : Send + 'static,
{
    Mux{
        _error : Default::default(),
        _input : Default::default(),
        handlers : Vec::new(),
    }
}



impl<E, I, ReqB, Res> Mux<E, I, ReqB, Res>
where
    E : From<MuxError>,
    I : Prepend<Request<ReqB>>,
{
    pub fn handle_fn<T, Hf, Fut>(
        mut self,
        route : Route<T>,
        f : Hf,
    ) -> Self
    where
        E : 'static,
        Res : 'static,
        Hf : Call<
            <I::Output as Merge<T>>::Output,
            Fut,
        > + Clone + Send + Sync + 'static,
        Fut : Future<Output = Result<Res, E>> + Send + 'static,
        I::Output : Merge<T>,
        T : for<'a, 'b> TryFrom<PathParser<'a, 'b>, Error = PathParseError> + 'static,
    {
        self.handlers.push(Handler{
            route : route.clone().into(),
            handler : Arc::new(HandlerFn{
                _marker : Default::default(),
                f,
                route
            }),
        });

        self
    }
    pub fn handle<T, Hp>(
        mut self,
        route : Route<T>,
        pipe : Hp,
    ) -> Self
    where
        E : 'static,
        I::Output : Merge<T>,
        Res : 'static,
        Hp : Pipe<
            Input = <I::Output as Merge<T>>::Output,
            Output = Result<Res, E>,
        > + Send + Sync + 'static,
        T : for<'a, 'b> TryFrom<PathParser<'a, 'b>, Error = PathParseError> + 'static,
    {
        self.handlers.push(Handler{
            route : route.clone().into(),
            handler : Arc::new(HandlerPipe{
                pipe, route
            }),
        });

        self
    }

    pub fn serve(
        &self,
        input : I::Output,
    ) -> impl Future<Output = Result<Res, E>>
    {
        let (req, input_tail) = input.pluck();

        let uri_path = req.uri().path();
        // let mut vars = Vec::new();

        let mut handler_match = None;
        let mut path_matched = false;

        // TODO: use an automata
        // * the loop is innefficient
        // * the `path_matches` call uses the same underlying function as
        //      `parse`, which results in allocations
        for Handler{route, handler} in self.handlers.iter() {
            eprintln!("trying: {:?}", route);
            if route.path_matches(&req) {
                path_matched |= true;
                if route.method_matches(&req) {
                    handler_match = Some(handler);
                    break
                }
            }
        }

        let handler_result = match handler_match {
            Some(handler) => {
                Ok(handler.serve(input_tail.prepend(req)))
            },
            None => {
                Err(if path_matched {
                    MuxError::MethodNotAllowed(req.method().clone(), uri_path.to_string())
                } else {
                    MuxError::NotFound(uri_path.to_string())
                }.into())
            }
        };

        async move {
            match handler_result {
                Err(e) => Err(e),
                Ok(pre_fut) => {
                    pre_fut.await
                }
            }
        }
    }
}

impl<E, I, ReqB, Res> Pipe for Mux<E, I, ReqB, Res>
where
    E : From<MuxError> + Send + 'static,
    I : Prepend<Request<ReqB>> + 'static,
    ReqB : 'static,
    Res : 'static,
{
    type Input = I::Output;
    type Output = Result<Res, E>;

    fn run(
        &self,
        input : Self::Input
    ) -> PinBoxFut<Self::Output> {
        Box::pin(self.serve(input))
    }
}


pub fn replace_path<Req>(req : &mut Request<Req>, path : &str) {
    let u = req.uri();
    let mut tmp = Uri::builder();

    if let Some(scheme) = u.scheme() {
        tmp = tmp.scheme(scheme.as_str());
    }

    if let Some(a) = u.authority() {
        tmp = tmp.authority(a.as_str());
    }

    let mut pq = String::new();
    pq.push_str(path);

    if let Some(q) = u.query() {
        pq.push_str(q);
    }

    tmp = tmp.path_and_query(pq);

    *req.uri_mut() = tmp.build().unwrap();
}
