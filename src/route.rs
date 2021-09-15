use std::fmt;
use std::marker::PhantomData;
use std::str::FromStr;
use std::error::Error;
use std::convert::TryFrom;

use http::Method;
use plumb::tuple_utils::Append;

use crate::tuple_macro;

#[macro_export]
macro_rules! route {
    (* / $($pieces:tt)*) => {
        $crate::route_impl!(
            $crate::route::RouteBuilder::new(None);
            $($pieces)*
        )
    };
    ($method:ident / $($pieces:tt)*) => {
        $crate::route_impl!(
            $crate::route::RouteBuilder::new(Some(::http::Method::$method));
            $($pieces)*
        )
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! route_impl {
    ($acc:expr;) => {
        $acc.end()
    };
    ($acc:expr; [ .. ] []) => {
        $acc.rest()
    };
    ($acc:expr; [ $cur:literal ] []) => {
        $crate::route_impl!(
            $acc.exact($cur.to_string());
        )
    };
    ($acc:expr; [ $cur:literal ] [ / $head:tt $(/ $tail:tt)* ]) => {
        $crate::route_impl!(
            $acc.exact($cur.to_string());
            [ $head ]
            [ $(/ $tail)* ]
        )
    };
    ($acc:expr; [ $cur:ty ] []) => {
        $crate::route_impl!(
            $acc.var::<$cur>();
        )
    };
    ($acc:expr; [ $cur:ty ] [ / $head:tt $(/ $tail:tt)* ]) => {
        $crate::route_impl!(
            $acc.var::<$cur>();
            [ $head ]
            [ $(/ $tail)* ]
        )
    };
    ($acc:expr; $head:tt $(/ $tail:tt)*) => {
        $crate::route_impl!(
            $acc;
            [$head]
            [$(/ $tail)*]
        )
    };
}

fn _test() {
    let _p : Route<()> = route!(* /);
    let _p : Route<()> = route!(* / "hello");
    let _p : Route<()> = route!(GET / "hello" / "world");
    let _p : Route<(u32,)> = route!(* / u32);
    let _p : Route<(u32,String)> = route!(POST / u32 / ..);
    let _p : Route<(u32,String)> = route!(* / "hello" / u32 / ..);
}

#[derive(Debug,Clone,PartialEq,Eq)]
enum PathToken {
    // exact matches should have highest priority
    Exact(String),
    Var,
    Rest,
    End,
}



fn parse_path_tokens<'a>(
    tokens : &[PathToken],
    path : &'a str,
    vars : &mut Vec<&'a str>,
) -> Result<Option<&'a str>, ()> {

    vars.clear();

    fn next_segment<'a>(p : &'a str) -> (&'a str, &'a str) {
        assert!(p.len() > 0 && &p[0..1] == "/");
        let p = p.split_at(1).1;
        match p.find('/') {
            Some(n) => p.split_at(n),
            None => (p, ""),
        }
    }

    fn scan<'a>(
        tok : &PathToken,
        s : &'a str,
        vars : &mut Vec<&'a str>
        ) -> Result<Option<&'a str>, ()> {
        assert!(&s[0..1] == "/");

        use PathToken::*;

        match tok {
            Var => {
                dbg!(s);
                let (seg, rest) = dbg!(next_segment(s));
                vars.push(seg);
                if rest.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(rest))
                }
            },
            Exact(exact) => {
                dbg!(s);
                let (seg, rest) = dbg!(next_segment(&s));
                if &seg != &exact {
                    return Err(())
                } else if rest.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(rest))
                }
            },
            Rest => {
                vars.push(s);
                Ok(None)
            },
            End => {
                if s.is_empty() || s == "/" {
                    Ok(None)
                } else {
                    Err(())
                }
            }
        }
    }

    let mut cursor = path;
    let mut it = tokens.iter();

    while let Some(token) = it.next() {
        match scan(token, cursor, vars) {
            Ok(Some(next)) => cursor = next,
            Ok(None) => {
                return match it.next() {
                    Some(PathToken::End) | None => Ok(None),
                    _ => Err(()),
                }
            }
            Err(_) => return Err(()),
        }
    }

    return Ok(Some(cursor))
}


pub struct RouteBuilder<T> {
    method : Option<Method>,
    tokens : Vec<PathToken>,
    _marker : PhantomData<fn () -> T>
}

impl RouteBuilder<()> {
    pub fn new(method : Option<Method>) -> Self {
        RouteBuilder {
            method,
            tokens : Vec::new(),
            _marker : Default::default(),
        }
    }
}

impl<T> RouteBuilder<T> {
    pub fn var<U : FromStr>(mut self) -> RouteBuilder<T::Output>
    where
        T : Append<U>
    {
        self.tokens.push(PathToken::Var);

        RouteBuilder::<T::Output> {
            method : self.method,
            tokens : self.tokens,
            _marker : Default::default(),
        }
    }

    pub fn exact(mut self, s : String) -> Self {
        self.tokens.push(PathToken::Exact(s));
        self
    }

    pub fn end(mut self) -> Route<T> {
        self.tokens.push(PathToken::End);
        self.build()
    }

    pub fn rest(mut self) -> Route<T::Output>
    where
        T : Append<String>
    {
        self.tokens.push(PathToken::Rest);
        RouteBuilder::<T::Output> {
            method : self.method,
            tokens : self.tokens,
            _marker : Default::default(),
        }.build()
    }

    fn build(self) -> Route<T> {
        Route{
            method : self.method,
            tokens : self.tokens,
            _marker : self._marker,
        }
    }
}


#[derive(Debug)]
pub(crate) struct RouteUntyped {
    method : Option<Method>,
    tokens : Vec<PathToken>,
}

impl<T> From<Route<T>> for RouteUntyped {
    fn from(r : Route<T>) -> Self {
        Self{
            method: r.method,
            tokens: r.tokens,
        }
    }
}

impl RouteUntyped {
    pub(crate) fn path_matches<B>(&self, req : &http::Request<B>) -> bool {
        parse_path_tokens(&self.tokens, req.uri().path(), &mut Vec::new()).is_ok()
    }

    pub(crate) fn method_matches<B>(&self, req : &http::Request<B>) -> bool {
        self.method.is_none() || self.method.as_ref() == Some(req.method())
    }
}

pub struct Route<T> {
    method : Option<Method>,
    tokens : Vec<PathToken>,
    _marker : PhantomData<fn () -> T>
}

impl<T> Clone for Route<T> {
    fn clone(&self) -> Self {
        Self{
            method : self.method.clone(),
            tokens : self.tokens.clone(),
            _marker : Default::default(),
        }
    }
}

#[derive(Debug)]
pub enum PathParseError {
    NoMatch,
    VarCountMismatch{
        tuple_size : usize,
        var_count : usize,
    },
    FromStr(Box<dyn Error + Send + Sync>),
}

impl fmt::Display for PathParseError {
    fn fmt(&self, f : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use PathParseError::*;
        match self {
            NoMatch => write!(f, "not a match"),
            VarCountMismatch{tuple_size, var_count}  => write!(
                f,
                "tuple has size {} but path has {} variables",
               tuple_size,
               var_count,
            ),
            FromStr(e) => e.fmt(f)
        }
    }
}

impl<T> Route<T> {

    pub fn parser<'a, 'b>(&'a self, s : &'b str) -> PathParser<'a, 'b> {
        PathParser{
            tokens : &self.tokens,
            path : s,
        }
    }
}



macro_rules! toss {
    ($x:tt) => { () }
}

macro_rules! impl_from_path_parser {
    ($($x:ident,)*) => {

        impl<$($x,)*> TryFrom<PathParser<'_, '_>> for ($($x,)*)
        where
        $(
            $x : FromStr,
            $x::Err : Error + Send + Sync + 'static,
        )*
        {
            type Error = PathParseError; // Box<dyn Error + Send + Sync>;

            fn try_from(mut it : PathParser) -> Result<Self, Self::Error> {
                #![allow(non_snake_case,unused_variables,unused_mut)]

                let units : &[()] = &[
                $(
                    toss!($x),
                )*
                ];

                let tuple_size = units.len();
                let mut n = 0;

                $(
                    let v = it.next().ok_or_else(|| {
                        PathParseError::VarCountMismatch{
                            tuple_size,
                            var_count : n,
                        }
                    })?.map_err(|()| {
                        PathParseError::NoMatch
                    })?;

                    let $x = $x::from_str(v)
                        .map_err(|x| PathParseError::FromStr(Box::new(x)))?;

                    n += 1;
                )*

                if it.next().is_some() {
                    return Err(PathParseError::VarCountMismatch{
                        tuple_size,
                        var_count : n + 1,
                    })
                }

                Ok((
                $(
                    $x,
                )*
                ))
            }
        }
    }
}

tuple_macro!{impl_from_path_parser}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test1() {
        use PathToken::*;

        let res = RouteBuilder::new(None)
            .exact("a".to_string())
            .build()
            .parse("/a");

        assert!(matches!(
                res,
                Ok(((), None))
        ));

        let res = RouteBuilder::new(None)
            .exact("a".to_string())
            .var::<u8>()
            .build()
            .parse("/a/10");

        assert!(matches!(
                res,
                Ok(((10,), None))
        ));

        let res = RouteBuilder::new(None)
            .exact("a".to_string())
            .var::<u8>()
            .build()
            .parse("/a/256");

        assert!(matches!(
                res,
                Err(_),
        ));


        let res = RouteBuilder::new(None)
            .exact("b".to_string())
            .build()
            .parse("/a");

        assert!(matches!(res, Err(PathParseError::NoMatch)));

        let res = RouteBuilder::new(None)
            .exact("a".to_string())
            .rest()
            .parse("/a/");

        assert!(matches!(
                res,
                Ok(((s,), None)) if s == "/"
        ));

        let res = RouteBuilder::new(None)
            .exact("a".to_string())
            .rest()
            .parse("/a");

        assert!(matches!(
            res,
            Err(PathParseError::NoMatch),
        ));


        let res = RouteBuilder::new(None)
            .var::<String>()
            .build()
            .parse("/a");

        assert!(matches!(
            res,
            Ok(((a,), None)) if a == "a"
        ));


        let res = RouteBuilder::new(None)
            .rest()
            .parse("/");

        assert!(matches!(
            res,
            Ok(((a,), None)) if a == "/"
        ));


        let res = RouteBuilder::new(None)
            .var::<String>()
            .exact("b".to_string())
            .rest()
            .parse("/a/b/c/d");

        assert!(matches!(
            res,
            Ok(((a,cd), None)) if a == "a" && cd == "/c/d"
        ));

        let res = RouteBuilder::new(None)
            .var::<String>()
            .exact("b".to_string())
            .var::<String>()
            .end()
            .parse("/a/b/c");

        assert!(matches!(
            res,
            Ok(((a,c), None)) if a == "a" && c == "c"
        ));
    }
}

pub struct PathParser<'a, 'b> {
    tokens : &'a [PathToken],
    path : &'b str
}

impl<'a, 'b> PathParser<'a, 'b> {
    pub fn rest(&self) -> &'b str {
        self.path
    }
}

impl<'a, 'b> Iterator for PathParser<'a, 'b> {
    type Item = Result<&'b str, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.len() == 0 {
            return None
        }

        if self.path.len() == 0 {
            return match self.tokens.get(0) {
                Some(PathToken::End) | None => None,
                _ => Some(Err(())),
            }
        }

        let mut var = None;

        loop {
            let cur = &self.tokens[0];
            self.tokens = &self.tokens[1..];

            match scan(cur, self.path, &mut var) {
                Ok(Some(next)) => {
                    self.path = next;

                    if var.is_some() {
                        return var.map(Ok)
                    }
                }
                Ok(None) => {
                    let (_, end) = self.path.split_at(self.path.len());
                    self.path = end;

                    if var.is_some() {
                        return var.map(Ok)
                    }

                    return match self.tokens.get(0) {
                        Some(PathToken::End) | None => None,
                        _ => Some(Err(())),
                    }
                },
                Err(_) => return Some(Err(()))
            }

        }
    }
}

fn next_segment<'a>(p : &'a str) -> (&'a str, &'a str) {
    assert!(p.len() > 0 && &p[0..1] == "/");
    let p = p.split_at(1).1;
    match p.find('/') {
        Some(n) => p.split_at(n),
        None => (p, ""),
    }
}

fn scan<'a>(
    tok : &PathToken,
    s : &'a str,
    var : &mut Option<&'a str>
) -> Result<Option<&'a str>, ()> {
    assert!(&s[0..1] == "/");

    use PathToken::*;

    match tok {
        Var => {
            dbg!(s);
            let (seg, rest) = dbg!(next_segment(s));
            *var = Some(seg);
            if rest.is_empty() {
                Ok(None)
            } else {
                Ok(Some(rest))
            }
        },
        Exact(exact) => {
            dbg!(s);
            let (seg, rest) = dbg!(next_segment(&s));
            if &seg != &exact {
                return Err(())
            } else if rest.is_empty() {
                Ok(None)
            } else {
                Ok(Some(rest))
            }
        },
        Rest => {
            *var = Some(s);
            Ok(None)
        },
        End => {
            if s.is_empty() || s == "/" {
                Ok(None)
            } else {
                Err(())
            }
        }
    }
}

#[cfg(test)]
#[test]
fn test() {
    let r = route!(* / "hello").into();
    let mut it = PathParser::new(
        &r,
        "/hello"
    );

    assert!(it.next() == None);
    assert_eq!(it.rest(), "");

    let r = route!(* / "hello" / u32).into();
    let mut it = PathParser::new(
        &r,
        "/hello/10"
    );

    assert_eq!(it.next(), Some(Ok("10")));
    assert_eq!(it.next(), None);
    assert_eq!(it.path, "");

    let r = route!(* / "hello" / u32 / "goodbye").into();
    let mut it = PathParser::new(
        &r,
        "/hello/10/goodbye"
    );

    assert_eq!(it.next(), Some(Ok("10")));
    assert_eq!(it.next(), None);
    assert_eq!(it.path, "");

    let r = route!(* / "hello" / u32 / .. ).into();
    let mut it = PathParser::new(
        &r,
        "/hello/10/goodbye/true"
    );

    assert_eq!(it.next(), Some(Ok("10")));
    assert_eq!(it.next(), Some(Ok("/goodbye/true")));
    assert_eq!(it.path, "");
}

