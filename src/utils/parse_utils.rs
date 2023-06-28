use nom::{
    bytes::complete::tag, combinator::map, error::ErrorKind, multi::separated_list1, IResult,
    Parser,
};

use super::slice::Slice;

#[macro_export]
macro_rules! make_node {
    ($kind:ident, $src:expr, $( $prop:ident ),* $(,)?) => {
        {
            let src = $src;
            let this = $kind {
                $($prop: $prop.clone(),)*
            }
            .as_ast(src);

            $($prop.set_parent(&this);)*

            this
        }
    };
}

#[macro_export]
macro_rules! make_node_tuple {
    ($kind:ident, $src:expr, $( $prop:ident ),* $(,)?) => {
        {
            let src = $src;
            let this = $kind(
                $($prop.clone()),*
            )
            .as_ast(src);

            $($prop.set_parent(&this);)*

            this
        }
    };
}

pub type ParseResult<T> = IResult<Slice, T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub src: Slice,
    pub details: ParseErrorDetails,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorDetails {
    Expected(String),
    Kind(nom::error::ErrorKind),
    Char(char),
    Incomplete,
}

impl nom::error::ParseError<Slice> for ParseError {
    fn from_error_kind(input: Slice, kind: nom::error::ErrorKind) -> Self {
        Self {
            src: input,
            details: ParseErrorDetails::Kind(kind),
        }
    }

    fn append(_input: Slice, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: Slice, ch: char) -> Self {
        Self {
            src: input,
            details: ParseErrorDetails::Char(ch),
        }
    }
}

impl nom::error::ContextError<Slice> for ParseError {
    fn add_context(_input: Slice, _ctx: &'static str, other: Self) -> Self {
        other
    }
}

impl<E> nom::error::FromExternalError<Slice, E> for ParseError {
    fn from_external_error(input: Slice, kind: nom::error::ErrorKind, _e: E) -> Self {
        Self {
            src: input,
            details: ParseErrorDetails::Kind(kind),
        }
    }
}

pub fn separated_list2<T, O2, F, G>(
    sep: G,
    f: F,
) -> impl FnMut(Slice) -> IResult<Slice, Vec<T>, ParseError>
where
    F: Parser<Slice, T, ParseError>,
    G: Parser<Slice, O2, ParseError>,
{
    let mut parser = separated_list1(sep, f);

    move |i: Slice| -> IResult<Slice, Vec<T>, ParseError> {
        let res = parser(i)?;

        if res.1.len() < 2 {
            Err(nom::Err::Error(ParseError {
                src: res.0,
                details: ParseErrorDetails::Kind(ErrorKind::SeparatedList),
            }))
        } else {
            Ok(res)
        }
    }
}

pub fn expect<TResult, F: FnMut(Slice) -> ParseResult<TResult>>(
    f: F,
    description: &'static str,
) -> impl FnMut(Slice) -> ParseResult<TResult> {
    expect_inner(f, description, false)
}

pub fn expect_tag(t: &'static str) -> impl FnMut(Slice) -> ParseResult<Slice> {
    expect_inner(tag(t), t, true)
}

fn expect_inner<TResult, F: FnMut(Slice) -> ParseResult<TResult>>(
    mut f: F,
    description: &'static str,
    quoted: bool,
) -> impl FnMut(Slice) -> ParseResult<TResult> {
    move |i: Slice| {
        let res = f(i.clone());

        if matches!(res, Err(nom::Err::Error(_))) {
            let details = if quoted {
                format!("'{}'", description)
            } else {
                description.to_owned()
            };

            Err(nom::Err::Failure(ParseError {
                src: i,
                details: ParseErrorDetails::Expected(details),
            }))
        } else {
            res
        }
    }
}

pub fn precedence<T>(
    levels: &'static [&'static [fn(Slice) -> ParseResult<T>]],
    after: Option<fn(Slice) -> ParseResult<T>>,
    i: Slice,
) -> ParseResult<T> {
    let start_index = match after {
        Some(after) => {
            levels
                .iter()
                .take_while(|level| !level.contains(&after))
                .count()
                + 1
        }
        None => 0,
    };

    for level in &levels[start_index..] {
        for f in *level {
            let res = f(i.clone());

            if res.is_ok() {
                return res;
            }
        }
    }

    Err(nom::Err::Error(ParseError {
        src: i,
        details: ParseErrorDetails::Kind(ErrorKind::Fail),
    }))
}

pub fn log<T: std::fmt::Debug, F: FnMut(Slice) -> ParseResult<T>>(
    f: F,
) -> impl FnMut(Slice) -> ParseResult<T> {
    map(f, |r| {
        println!("{:?}", r);
        r
    })
}
