use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{digit1, multispace0, alpha1, char},
    combinator::{map_res, recognize, value, map, opt, not, peek, verify},
    sequence::{pair, delimited, terminated, preceded},
    multi::{many0, fold_many0, separated_list0}
};
use std::str::FromStr;

use crate::ir::ast::Function;
use crate::ir::ast::Type;
use crate::ir::ast::{Expression, Name, Statement, ValueConstructor};

use crate::parser::keywords::KEYWORDS;

pub fn parse_expression(input: &str) -> IResult<&str, Expression> {
    parse_or(input)
}

fn parse_or(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_and(input)?;
    fold_many0(
        preceded(keyword("or"), parse_and),
        move || init.clone(),
        |acc, val| Expression::Or(Box::new(acc), Box::new(val)),
    )(input)
}

fn parse_and(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_not(input)?;
    fold_many0(
        preceded(keyword("and"), parse_not),
        move || init.clone(),
        |acc, val| Expression::And(Box::new(acc), Box::new(val)),
    )(input)
}

fn parse_not(input: &str) -> IResult<&str, Expression> {
    alt((
        map(preceded(keyword("not"), parse_not), |e| Expression::Not(Box::new(e))),
        parse_relational,
    ))(input)
}

fn parse_relational(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_add_sub(input)?;
    fold_many0(
        pair(
            alt((operator("<="), operator("<"), operator(">="), operator(">"), operator("=="), operator("!="))),
            parse_add_sub,
        ),
        move || init.clone(),
        |acc, (op, val)| match op {
            "<" => Expression::LT(Box::new(acc), Box::new(val)),
            "<=" => Expression::LTE(Box::new(acc), Box::new(val)),
            ">" => Expression::GT(Box::new(acc), Box::new(val)),
            ">=" => Expression::GTE(Box::new(acc), Box::new(val)),
            "==" => Expression::EQ(Box::new(acc), Box::new(val)),
            "!=" => Expression::NEQ(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        },
    )(input)
}

fn parse_add_sub(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_term(input)?;
    fold_many0(
        pair(
            alt((operator("+"), operator("-"))),
            parse_term,
        ),
        move || init.clone(),
        |acc, (op, val)| match op {
            "+" => Expression::Add(Box::new(acc), Box::new(val)),
            "-" => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        },
    )(input)
}

fn parse_term(input: &str) -> IResult<&str, Expression> {
    let (input, init) = parse_factor(input)?;
    fold_many0(
        pair(
            alt((operator("*"), operator("/"))),
            parse_factor,
        ),
        move || init.clone(),
        |acc, (op, val)| match op {
            "*" => Expression::Mul(Box::new(acc), Box::new(val)),
            "/" => Expression::Div(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        },
    )(input)
}

fn parse_factor(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_bool,
        parse_number,
	parse_string,
	parse_var,
	parse_function_call,
        delimited(tag("("), parse_expression, tag(")")),
    ))(input)
}

fn parse_bool(input: &str) -> IResult<&str, Expression> {
    alt((value(Expression::CTrue, keyword("True")), value(Expression::CFalse, keyword("False"))))(input)
}
				      
fn parse_number(input: &str) -> IResult<&str, Expression> {
    let float_parser = map_res(
        recognize(pair(
            digit1,
            opt(pair(tag("."), digit1))
        )),
        |s: &str| f64::from_str(s)
    );

    let int_parser = map_res(digit1, |s: &str| i32::from_str(s));

    alt((
        map(float_parser, Expression::CReal),
        map(int_parser, Expression::CInt),
    ))(input)
}

fn parse_string(input: &str) -> IResult<&str, Expression> {
    map(delimited(
        multispace0,
        delimited(
            tag("\""),
            map(take_while(is_string_char), |s: &str| s.to_string()),
            tag("\""),
        ),
        multispace0,
    ), |s| Expression::CString(s))(input)
}

fn parse_var(input: &str) -> IResult<&str, Expression> {
    map(parse_identifier, |v| Expression::Var(v.into()))(input)
}
fn parse_function_call(input: &str) -> IResult<&str, Expression> {
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('(')(input)?;
    let (input, args) = separated_list0(separator(","), parse_expression)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('(')(input)?;

    Ok((input, Expression::FuncCall(name.to_string(), args)))
}


fn separator<'a>(sep: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(multispace0, tag(sep), multispace0)
}
    


/// Parses a reserved keyword (e.g., "if") surrounded by optional spaces
/// Fails if followed by an identifier character
fn keyword<'a>(kw: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    terminated(
        delimited(multispace0, tag(kw), multispace0),
        not(peek(identifier_start_or_continue)),
    )
}

/// Parsers for identifiers.
fn parse_identifier(input: &str) -> IResult<&str, &str> {
    let (input, _) = multispace0(input)?;

    let (input, first_char) = identifier_start(input)?;
    let (input, rest) = identifier_continue(input)?;

    let ident = format!("{}{}", first_char, rest);

    if KEYWORDS.contains(&ident.as_str()) {
        Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
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


/// Parses an operator.
fn operator<'a>(op: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(multispace0, tag(op), multispace0)
}


/// Accepts any character except '"' and control characters (like \n, \t)
fn is_string_char(c: char) -> bool {
    c != '"' && !c.is_control()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expression_integer() {
        assert_eq!(
            parse_expression("123abc"),
            Ok(("abc", Expression::CInt(123)))
        );

        assert_eq!(
            parse_expression("-456 rest"),
            Ok((" rest", Expression::CInt(-456)))
        );
    }

    #[test]
    #[ignore]
    fn test_parse_expression_real() {
        assert_eq!(
            parse_expression("3.14xyz"),
            Ok(("xyz", Expression::CReal(3.14)))
        );

        assert_eq!(
            parse_expression("-0.001rest"),
            Ok(("rest", Expression::CReal(-0.001)))
        );

        assert_eq!(
            parse_expression("2e3more"),
            Ok(("more", Expression::CReal(2000.0)))
        );
    }

    #[test]
    #[ignore]
    fn test_parse_expression_errors() {
        // Doesn't start with a number
        assert!(parse_expression("hello").is_err());

        // Not a valid number
        assert!(parse_expression("12.34.56").is_err());
    }

    #[test]
    fn test_keywords() {
        let cases = [
            ("if", "if"),
            (" else ", "else"),
            ("while rest", "while"),
            ("  and   ", "and"),
            ("or)", "or"),
            ("not x", "not"),
            (" for (", "for"),
            ("def  ", "def"),
        ];

        for (input, expected) in cases {
            let mut parser = keyword(expected);
            let result = parser(input);
            assert_eq!(
                result,
                Ok((input[expected.len()..].trim_start(), expected)),
                "Failed to parse keyword '{}'", expected
            );
        }
    }

    #[test]
    fn test_keyword_should_not_match_prefix_of_identifiers() {
        let mut parser = keyword("if");
        assert!(parser("iffy").is_err());

        let mut parser = keyword("def");
        assert!(parser("default").is_err());

        let mut parser = keyword("or");
        assert!(parser("origin").is_err());
    }
}

