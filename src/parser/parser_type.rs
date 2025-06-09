use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, char, digit1, line_ending, multispace0, space0},
    combinator::{map, map_res, not, opt, peek, recognize, value, verify},
    multi::{fold_many0, many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::str::FromStr;

use crate::ir::ast::{Type, ValueConstructor};

use crate::parser::parser_common::{keyword, separator, identifier};

pub fn parse_type(input: &str) -> IResult<&str, Type> {
    alt(
	(parse_basic_types,
	 parse_list_type,
	 parse_tuple_type,
	 parse_maybe_type,
	 parse_result_type,
	 parse_function_type,
	 parse_adt_type)
    )(input)
}

fn parse_basic_types(input: &str) -> IResult<&str, Type> {
    map(
	alt((keyword("Int"),
	     keyword("Real"),
	     keyword("Boolean"),
	     keyword("String"),
	     keyword("Unit"),
	     keyword("Any")
	)),
	    |t| match t {
		"Int" => Type::TInteger,
		"Real" => Type::TReal,
		"Boolean" => Type::TBool,
		"String" => Type::TString,
		"Unit" => Type::TVoid,
		"Any" => Type::TAny,
		_ => unreachable!()
	    }
    )(input)
}

fn parse_list_type(input: &str) -> IResult<&str, Type> {
    map(tuple(
	(preceded(multispace0, char('[')),
	 preceded(multispace0, parse_type),
	 preceded(multispace0, char(']')),
	)),
	|(_, t, _)| Type::TList(Box::new(t))
    )(input)
}

fn parse_tuple_type(input: &str) -> IResult<&str, Type> {
   map(tuple(
	(preceded(multispace0, char('(')),
	 preceded(multispace0, separated_list1(separator(","), parse_type)),
	 preceded(multispace0, char(')')),
	)),
	|(_, ts, _)| Type::TTuple(ts)
    )(input)
}

fn parse_maybe_type(input: &str) -> IResult<&str, Type> {
   map(tuple(
         (preceded(multispace0, keyword("Maybe")),
	  preceded(multispace0, char('[')), 
	  preceded(multispace0, parse_type),
	  preceded(multispace0, char(']')),
	 )),
	|(_, _, t, _)| Type::TMaybe(Box::new(t))
    )(input)
}

fn parse_result_type(input: &str) -> IResult<&str, Type> {
   map(tuple(
         (preceded(multispace0, keyword("Result")),
	  preceded(multispace0, char('[')), 
	  preceded(multispace0, parse_type),
	  preceded(multispace0, char(',')),
	  preceded(multispace0, parse_type),
	  preceded(multispace0, char(']')),
	 )),
	|(_, _, t_ok, _, t_err, _)| Type::TResult(Box::new(t_ok), Box::new(t_err))
    )(input)
}

fn parse_function_type(input: &str) -> IResult<&str, Type> {
       map(tuple(
         (preceded(multispace0, char('(')), 
	  preceded(multispace0, separated_list0(separator(","), parse_type)),
	  preceded(multispace0, char(')')),
	  preceded(multispace0, tag("->")),
	  preceded(multispace0, parse_type),
	 )),
	|(_, t_args, _, _, t_ret)| Type::TFunction(Box::new(Some(t_ret)), t_args)
    )(input)
}

fn parse_adt_type(input: &str) -> IResult<&str, Type> {
    map(
        tuple((
            keyword("data"),
            preceded(multispace0, identifier),
            preceded(multispace0, char(':')),
            many1(parse_adt_cons),
            preceded(multispace0, keyword("end")),
        )),
        |(_, name, _, cons, _)| Type::Tadt(name.to_string(), cons),
    )(input)
}

fn parse_adt_cons(input: &str) -> IResult<&str, ValueConstructor> {
    map(
        tuple((
            preceded(multispace0, char('|')),
            preceded(multispace0, identifier),
            separated_list0(multispace0, parse_type),
        )),
        |(_, name, types)| ValueConstructor::new(name.to_string(), types),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_types() {
        assert_eq!(parse_basic_types("Int"), Ok(("", Type::TInteger)));
        assert_eq!(parse_basic_types("Boolean"), Ok(("", Type::TBool)));
    }

    #[test]
    fn test_parse_list_type() {
        assert_eq!(
            parse_list_type("[Int]"),
            Ok(("", Type::TList(Box::new(Type::TInteger))))
        );
    }

    #[test]
    fn test_parse_tuple_type() {
        assert_eq!(
            parse_tuple_type("(Int, Real)"),
            Ok(("", Type::TTuple(vec![Type::TInteger, Type::TReal])))
        );
    }

    #[test]
    fn test_parse_maybe_type() {
        assert_eq!(
            parse_maybe_type("Maybe [Boolean]"),
            Ok(("", Type::TMaybe(Box::new(Type::TBool))))
        );
    }

    #[test]
    fn test_parse_result_type() {
        assert_eq!(
            parse_result_type("Result [Int, String]"),
            Ok((
                "",
                Type::TResult(Box::new(Type::TInteger), Box::new(Type::TString))
            ))
        );
    }

    #[test]
    fn test_parse_function_type() {
        assert_eq!(
            parse_function_type("(Int, Boolean) -> String"),
            Ok((
                "",
                Type::TFunction(
                    Box::new(Some(Type::TString)),
                    vec![Type::TInteger, Type::TBool]
                )
            ))
        );
    }

    #[test]
    #[ignore]
    fn test_parse_adt_type() {
        let input = "data Maybe:\n  | Just Int\n  | Nothing\nend";
        let expected = Type::Tadt(
            "Maybe".to_string(),
            vec![
                ValueConstructor::new("Just".to_string(), vec![Type::TInteger]),
                ValueConstructor::new("Nothing".to_string(), vec![]),
            ],
        );
        assert_eq!(parse_adt_type(input), Ok(("", expected)));
    }
}
