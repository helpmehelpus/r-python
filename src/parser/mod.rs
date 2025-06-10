pub mod keywords;
pub mod parser_common;
pub mod parser_expr;
pub mod parser_stmt;
pub mod parser_type;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0},
    combinator::{map, opt},
    error::Error,
    multi::separated_list0,
    sequence::tuple,
    IResult,
};

use crate::ir::ast::Statement;

pub use parser_expr::parse_expression;
pub use parser_stmt::parse_statement;
pub use parser_type::parse_type;

pub fn parse(input: &str) -> IResult<&str, Vec<Statement>> {
    map(
        tuple((
            multispace0,
            separated_list0(
                tuple((multispace0, char(';'), multispace0)),
                parse_statement,
            ),
            opt(tuple((multispace0, char(';')))), // optional trailing semicolon
            multispace0,
        )),
        |(_, statements, _, _)| statements,
    )(input)
}
