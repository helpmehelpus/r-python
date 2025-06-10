use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1},
    combinator::{map, map_res, opt, value, verify},
    multi::{fold_many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
    error::Error,
};

use crate::ir::ast::{Expression, Statement, Function, Type, FormalArgument};
use crate::parser::parser_common::{identifier, keyword};
use crate::parser::parser_expr::{parse_expression, parse_actual_arguments};
use crate::parser::parser_type::parse_type;

pub fn parse_statement(input: &str) -> IResult<&str, Statement> {
    alt((
        parse_assignment_statement,
        parse_if_else_statement,
        parse_while_statement,
        parse_for_statement,
        parse_assert_statement,
        parse_function_definition_statement,
    ))(input)
}

fn parse_assignment_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            delimited(multispace0, identifier, multispace0),
            char::<&str, Error<&str>>('='),
            delimited(multispace0, parse_expression, multispace0)
        )),
        |(var, _, expr)| Statement::Assignment(var.to_string(), Box::new(expr)),
    )(input)
}

fn parse_if_else_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword("if"),
            preceded(multispace1, parse_expression),
            parse_block,
            opt(preceded(
                tuple((multispace0, keyword("else"))),
                parse_block
            ))
        )),
        |(_, cond, then_block, else_block)| {
            Statement::IfThenElse(
                Box::new(cond),
                Box::new(then_block),
                else_block.map(Box::new)
            )
        }
    )(input)
}

fn parse_while_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword("while"),
            preceded(multispace1, parse_expression),
            parse_block
        )),
        |(_, cond, block)| {
            Statement::While(
                Box::new(cond),
                Box::new(block)
            )
        }
    )(input)
}

fn parse_for_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword("for"),
            preceded(multispace1, identifier),
            preceded(multispace0, keyword("in")),
            preceded(multispace1, parse_expression),
            parse_block
        )),
        |(_, var, _, expr, block)| {
            Statement::For(
                var.to_string(),
                Box::new(expr),
                Box::new(block)
            )
        }
    )(input)
}

fn parse_assert_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword("assert"),
            delimited(
                char::<&str, Error<&str>>('('),
                separated_list0(
                    tuple((multispace0, char::<&str, Error<&str>>(','), multispace0)),
                    parse_expression
                ),
                char::<&str, Error<&str>>(')')
            )
        )),
        |(_, args)| {
            if args.len() != 2 {
                panic!("Assert statement requires exactly 2 arguments");
            }
            Statement::Assert(Box::new(args[0].clone()), Box::new(args[1].clone()))
        }
    )(input)
}

fn parse_function_definition_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            keyword("def"),
            preceded(multispace1, identifier),
            delimited(
                char::<&str, Error<&str>>('('),
                separated_list0(
                    tuple((multispace0, char::<&str, Error<&str>>(','), multispace0)),
                    parse_formal_argument
                ),
                char::<&str, Error<&str>>(')')
            ),
            preceded(multispace0, tag("->")),
            preceded(multispace0, parse_type),
            parse_block
        )),
        |(_, name, args, _, t, block)| {
            let params = args
                .into_iter()
                .map(|f| (f.argumentName, f.argumentType))
                .collect::<Vec<_>>();

            Statement::FuncDef(Function {
                name: name.to_string(),
                kind: t,
                params: Some(params),
                body: Some(Box::new(block))
            })
        }
    )(input)
}

fn parse_block(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            char::<&str, Error<&str>>(':'),
            multispace0,
            separated_list0(
                delimited(
                    multispace0,
                    char::<&str, Error<&str>>(';'),
                    multispace0
                ),
                parse_statement
            ),
            opt(preceded(
                multispace0,
                char::<&str, Error<&str>>(';')
            )),
            delimited(multispace0, keyword("end"), multispace0)
        )),
        |(_, _, stmts, _, _)| Statement::Block(stmts)
    )(input)
}

fn parse_formal_argument(input: &str) -> IResult<&str, FormalArgument> {
    map(
        tuple((
            preceded(multispace0, identifier),
            preceded(multispace0, char::<&str, Error<&str>>(':') ),
            preceded(multispace0, parse_type)
        )),
        |(name, _, t)| FormalArgument::new(name.to_string(), t)
    )(input)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{Expression, Statement, Function, Type, FormalArgument};

    #[test]
    fn test_parse_assignment_statement() {
        let input = "x = 42";
        let expected = Statement::Assignment("x".to_string(), Box::new(Expression::CInt(42)));
        let parsed = parse_assignment_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    #[ignore]
    fn test_parse_if_else_statement() {
        let input = "if True: x = 1; end";
        let expected = Statement::IfThenElse(
            Box::new(Expression::CTrue),
            Box::new(Statement::Block(vec![
                Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1)))
            ])),
            None,
        );
        let parsed = parse_if_else_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    #[ignore]
    fn test_parse_while_statement() {
        let input = "while True: x = 1; end";
        let expected = Statement::While(
            Box::new(Expression::CTrue),
            Box::new(Statement::Block(vec![
                Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1)))
            ])),
        );
        let parsed = parse_while_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    #[ignore]
    fn test_parse_for_statement() {
        let input = "for x in y: x = 1; end";
        let expected = Statement::For(
            "x".to_string(),
            Box::new(Expression::Var("y".to_string())),
            Box::new(Statement::Block(vec![
                Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1)))
            ])),
        );
        let parsed = parse_for_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_assert_statement() {
        let input = "assert(1 == 2, \"expecting an error\")";
        let expected = Statement::Assert(
            Box::new(Expression::EQ(
                Box::new(Expression::CInt(1)),
                Box::new(Expression::CInt(2))
            )),
            Box::new(Expression::CString("expecting an error".to_string()))
        );
        let parsed = parse_assert_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    #[ignore]
    fn test_parse_function_definition_statement() {
        let input = "def f(x: Int) -> Int: x = 1; end";
        let expected = Statement::FuncDef(Function {
            name: "f".to_string(),
            kind: Type::TInteger,
            params: Some(vec![("x".to_string(), Type::TInteger)]),
            body: Some(Box::new(Statement::Block(vec![
                Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1)))
            ]))),
        });
        let parsed = parse_function_definition_statement(input).unwrap().1;
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_block() {
        let input = ": x = 1; end";
        let expected = Statement::Block(vec![
            Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1)))
        ]);
        let (rest, parsed) = parse_block(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(parsed, expected);

        let input = ": x = 1; y = x + 1; end";
        let expected = Statement::Block(vec![
            Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1))),
            Statement::Assignment(
                "y".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::CInt(1))
                ))
            )
        ]);
        let (rest, parsed) = parse_block(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_formal_argument() {
        let input = "x: Int";
        let expected = FormalArgument {
            argumentName: "x".to_string(),
            argumentType: Type::TInteger,
        };
        let parsed = parse_formal_argument(input).unwrap().1;
        assert_eq!(parsed, expected);
    }
}
