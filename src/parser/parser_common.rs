use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, char, digit1, multispace0},
    combinator::{map, map_res, not, opt, peek, recognize, value, verify},
    multi::{fold_many0, many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use crate::parser::keywords::KEYWORDS;

/// Accepts any character except '"' and control characters (like \n, \t)
pub fn is_string_char(c: char) -> bool {
    c != '"' && !c.is_control()
}

pub fn separator<'a>(sep: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(multispace0, tag(sep), multispace0)
}

/// Parses a reserved keyword (e.g., "if") surrounded by optional spaces
/// Fails if followed by an identifier character
pub fn keyword<'a>(kw: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    terminated(
        delimited(multispace0, tag(kw), multispace0),
        not(peek(identifier_start_or_continue)),
    )
}

/// Parsers for identifiers.
pub fn identifier(input: &str) -> IResult<&str, &str> {
    let (input, _) = multispace0(input)?;

    let (input, first_char) = identifier_start(input)?;
    let (input, rest) = identifier_continue(input)?;

    let ident = format!("{}{}", first_char, rest);

    if KEYWORDS.contains(&ident.as_str()) {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    } else {
        Ok((input, Box::leak(ident.into_boxed_str())))
    }
}

/// First character of an identifier: [a-zA-Z_]
fn identifier_start(input: &str) -> IResult<&str, &str> {
    alt((alpha1, tag("_")))(input)
}

/// Remaining characters: [a-zA-Z0-9_]*
fn identifier_continue(input: &str) -> IResult<&str, &str> {
    recognize(many0(identifier_start_or_continue))(input)
}

/// A single identifier character: alphanumeric or underscore
fn identifier_start_or_continue(input: &str) -> IResult<&str, &str> {
    recognize(alt((alpha1, tag("_"), nom::character::complete::digit1)))(input)
}
