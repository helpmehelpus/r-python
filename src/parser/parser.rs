use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, line_ending, multispace0, space0, space1},
    combinator::{map, map_res, opt, recognize},
    error::Error,
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use crate::ir::ast::Function;
use crate::ir::ast::Type;
use crate::ir::ast::{Expression, Name, Statement, ValueConstructor};

type ParseResult<'a, T> = IResult<&'a str, T, Error<&'a str>>;

pub fn parse(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, statements) = parse_statements(input)?;
    let (input, _) = many0(line_ending)(input)?;
    let (input, _) = space0(input)?;

    Ok((input, statements))
}

pub fn parse_statements(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, _) = space0(input)?;
    let (input, statements) =
        separated_list1(many1(tuple((space0, line_ending, space0))), parse_statement)(input)?;
    let (input, _) = space0(input)?;

    Ok((input, statements))
}

pub fn parse_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = space0(input)?;
    alt((
        parse_assignment_statement,
        parse_if_statement,
        parse_for_statement,
	parse_while_statement,
        parse_function_def_statement,
        parse_return_statement,
        parse_var_declaration_statement,
        parse_adt_declaration_statement,
        match_expression,
    ))(input)
}

fn parse_assignment_statement(input: &str) -> IResult<&str, Statement> {
    let (input, name) = identifier(input)?;
    let (input, _) = delimited(space0, char('='), space0)(input)?;
    let (input, expr) = expression(input)?;

    Ok((input, Statement::Assignment(name, Box::new(expr))))
}

fn parse_if_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = space1(input)?;
    let (input, condition) = alt((
        comparison_expression,
        boolean_expression,
        map(identifier, Expression::Var),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, then_block) = indented_block(input)?;

    let (input, else_block) = opt(preceded(
        tuple((line_ending, space0, tag("else"), char(':'))),
        indented_block,
    ))(input)?;

    Ok((
        input,
        Statement::IfThenElse(
            Box::new(condition),
            Box::new(Statement::Block(then_block)),
            else_block.map(|stmts| Box::new(Statement::Block(stmts))),
        ),
    ))
}

fn parse_for_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("for")(input)?;
    let (input, _) = space1(input)?;
    let (input, var) = identifier(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag("in")(input)?;
    let (input, _) = space1(input)?;
    let (input, exp) = expression(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, block) = indented_block(input)?;
    Ok((
        input,
        Statement::For(var, Box::new(exp), Box::new(Statement::Block(block))),
    ))
}

fn parse_while_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("while")(input)?;
    let (input, _) = space1(input)?;
    let (input, exp) = expression(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, block) = indented_block(input)?;
    Ok((
        input,
        Statement::While(Box::new(exp), Box::new(Statement::Block(block)))
    ))
}

fn parse_function_def_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("def")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = char('(')(input)?;
    let (input, params) = separated_list0(
        delimited(space0, char(','), space0),
        tuple((
            identifier,
            preceded(tuple((space0, char(':'), space0)), identifier),
        )),
    )(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = tag("->")(input)?;
    let (input, _) = space0(input)?;
    let (input, return_type) = identifier(input)?;
    let (input, _) = char(':')(input)?;
    let (input, body) = indented_block(input)?;

    Ok((
        input,
        Statement::FuncDef(Function {
            name: name.clone(),
            kind: Some(parse_type(&return_type)), 
            params: Some(
                params
                    .into_iter()
                    .map(|(name, type_name)| (name, parse_type(&type_name)))
                    .collect(),
            ),
            body: Some(Box::new(Statement::Block(body))),
        }),
    ))
}

fn parse_return_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("return")(input)?;
    let (input, _) = space1(input)?;
    let (input, expr) = expression(input)?;

    Ok((input, Statement::Return(Box::new(expr))))
}

fn parse_var_declaration_statement(input: &str) -> IResult<&str, Statement> {
    let (input, keyword) = alt((tag("var"), tag("val")))(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = identifier(input)?;

    Ok((
        input,
        match keyword {
            "var" => Statement::VarDeclaration(name), 
            "val" => Statement::ValDeclaration(name), 
            _ => unreachable!(),
        },
    ))
}

pub fn parse_adt_declaration_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("data")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space0(input)?;
    let (input, constructors) = separated_list1(
        preceded(space0, char('|')),         // Match `|`, allowing leading spaces
        preceded(space0, value_constructor), // Consume extra spaces before each constructor
    )(input)?;

    Ok((input, Statement::ADTDeclaration(name, constructors)))
}

pub fn value_constructor(input: &str) -> IResult<&str, ValueConstructor> {
    let (input, name) = identifier(input)?;
    let (input, types) = many0(preceded(space1, type_annotation))(input)?;

    Ok((input, ValueConstructor { name, types }))
}

pub fn identifier(input: &str) -> IResult<&str, Name> {
    let (input, id) = take_while1(|c: char| c.is_alphanumeric() || c == '_')(input)?;

    if KEYWORDS.contains(&id) {
        return Err(nom::Err::Error(Error {
            input,
            code: nom::error::ErrorKind::Tag,
        }));
    }

    Ok((input, id.to_string()))
}

// Parse integer literals
pub fn integer(input: &str) -> IResult<&str, Expression> {
    map_res(
        pair(opt(preceded(space0, char('-'))), preceded(space0, digit1)),
        |(sign, digits): (Option<char>, &str)| {
            digits.parse::<i32>().map(|num| {
                if sign.is_some() {
                    Expression::CInt(-num)
                } else {
                    Expression::CInt(num)
                }
            })
        },
    )(input)
}

//term parser for arithmetic
pub fn term(input: &str) -> ParseResult<Expression> {
    let (mut input, mut expr) = factor(input)?;

    loop {
        let op_result = delimited::<_, _, _, _, Error<&str>, _, _, _>(
            space0::<&str, Error<&str>>,
            alt((tag("*"), tag("/"))),
            space0::<&str, Error<&str>>,
        )(input);

        match op_result {
            Ok((new_input, op)) => {
                let (newer_input, factor2) = factor(new_input)?;
                expr = match op {
                    "*" => Expression::Mul(Box::new(expr), Box::new(factor2)),
                    "/" => Expression::Div(Box::new(expr), Box::new(factor2)),
                    _ => unreachable!(),
                };
                input = newer_input;
            }
            Err(_) => break,
        }
    }

    Ok((input, expr))
}

//expression parser to include if statements

// Parse basic expressions
pub fn expression(input: &str) -> IResult<&str, Expression> {
    alt((
        boolean_expression,
        comparison_expression,
        arithmetic_expression,
        real,
        integer,
        ok_expression,
        err_expression,
        just_expression,
        nothing_expression,
        unwrap_expression,
        tryunwrap_expression,
        iserror_expression,
        isnothing_expression,
        string,
        map(identifier, Expression::Var),
    ))(input)
}

// Parse arithmetic operators (unused)
//pub fn operator(input: &str) -> IResult<&str, &str> {
//alt((tag("+"), tag("-"), tag("*"), tag("/")))(input)
//}

// Add comparison operator parsing
pub fn comparison_operator(input: &str) -> IResult<&str, &str> {
    alt((
        tag("=="),
        tag("!="),
        tag(">="),
        tag("<="),
        tag(">"),
        tag("<"),
    ))(input)
}

// Update expression to handle comparisons
pub fn comparison_expression(input: &str) -> IResult<&str, Expression> {
    let (input, left) = term(input)?;
    let (input, _) = space0(input)?;
    let (input, op) = comparison_operator(input)?;
    let (input, _) = space0(input)?;
    let (input, right) = term(input)?;
    Ok((
        input,
        match op {
            ">" => Expression::GT(Box::new(left), Box::new(right)),
            "<" => Expression::LT(Box::new(left), Box::new(right)),
            ">=" => Expression::GTE(Box::new(left), Box::new(right)),
            "<=" => Expression::LTE(Box::new(left), Box::new(right)),
            "==" => Expression::EQ(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        },
    ))
}

// Parse expressions with operator precedence
pub fn arithmetic_expression(input: &str) -> ParseResult<Expression> {
    let (mut input, mut expr) = term(input)?;

    loop {
        let op_result = delimited::<_, _, _, _, Error<&str>, _, _, _>(
            space0::<&str, Error<&str>>,
            alt((tag("+"), tag("-"))),
            space0::<&str, Error<&str>>,
        )(input);

        match op_result {
            Ok((new_input, op)) => {
                let (newer_input, term2) = term(new_input)?;
                expr = match op {
                    "+" => Expression::Add(Box::new(expr), Box::new(term2)),
                    "-" => Expression::Sub(Box::new(expr), Box::new(term2)),
                    _ => unreachable!(),
                };
                input = newer_input;
            }
            Err(_) => break,
        }
    }

    Ok((input, expr))
}

// Add to imports
use nom::character::complete::char as char_parser;

// Parse boolean literals
pub fn boolean(input: &str) -> IResult<&str, Expression> {
    alt((
        map(tag("True"), |_| Expression::CTrue),
        map(tag("False"), |_| Expression::CFalse),
    ))(input)
}

// Parse real numbers
pub fn real(input: &str) -> IResult<&str, Expression> {
    map_res(
        recognize(tuple((opt(char('-')), digit1, char('.'), digit1))),
        |num_str: &str| num_str.parse::<f64>().map(Expression::CReal),
    )(input)
}

// Parse strings
pub fn string(input: &str) -> IResult<&str, Expression> {
    delimited(
        char_parser('"'),
        map(take_while(|c| c != '"'), |s: &str| {
            Expression::CString(s.to_string())
        }),
        char_parser('"'),
    )(input)
}

pub fn ok_expression(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("Ok")(input)?;
    let (input, _) = space0(input)?;
    let (input, expr) = delimited(
        tuple((char('('), space0)),
        expression,
        tuple((space0, char(')'))),
    )(input)?;

    Ok((input, Expression::COk(Box::new(expr))))
}

pub fn err_expression(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("Err")(input)?;
    let (input, _) = space0(input)?;
    let (input, expr) = delimited(
        tuple((char('('), space0)),
        expression,
        tuple((space0, char(')'))),
    )(input)?;

    Ok((input, Expression::CErr(Box::new(expr))))
}

pub fn just_expression(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("Just")(input)?;
    let (input, _) = space0(input)?;
    let (input, expr) = delimited(
        tuple((char('('), space0)),
        expression,
        tuple((space0, char(')'))),
    )(input)?;
    Ok((input, Expression::CJust(Box::new(expr))))
}

pub fn nothing_expression(input: &str) -> IResult<&str, Expression> {
    map(tag("Nothing"), |_| Expression::CNothing)(input)
}

pub fn isnothing_expression(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("isNothing")(input)?;
    let (input, _) = space0(input)?;

    let (input, expr) = delimited(
        tuple((char('('), space0)),
        expression,
        tuple((space0, char(')'))),
    )(input)?;

    Ok((input, Expression::IsNothing(Box::new(expr))))
}

pub fn iserror_expression(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("isError")(input)?;
    let (input, _) = space0(input)?;
    let (input, expr) = delimited(
        tuple((char('('), space0)),
        expression,
        tuple((space0, char(')'))),
    )(input)?;

    Ok((input, Expression::IsError(Box::new(expr))))
}

pub fn unwrap_expression(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("unwrap")(input)?;
    let (input, _) = space0(input)?;
    let (input, expr) = delimited(
        tuple((char('('), space0)),
        expression,
        tuple((space0, char(')'))),
    )(input)?;

    Ok((input, Expression::Unwrap(Box::new(expr))))
}

pub fn tryunwrap_expression(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("tryUnwrap")(input)?;
    let (input, _) = space0(input)?;
    let (input, expr) = delimited(
        tuple((char('('), space0)),
        expression,
        tuple((space0, char(')'))),
    )(input)?;

    Ok((input, Expression::Propagate(Box::new(expr))))
}

// Parse boolean operations
pub fn boolean_expression(input: &str) -> IResult<&str, Expression> {
    let (input, first) = boolean_term(input)?;
    let (input, rest) = many0(tuple((
        delimited(space0, alt((tag("and"), tag("or"))), space0),
        boolean_term,
    )))(input)?;

    Ok((
        input,
        rest.into_iter().fold(first, |acc, (op, val)| match op {
            "and" => Expression::And(Box::new(acc), Box::new(val)),
            "or" => Expression::Or(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        }),
    ))
}

pub fn boolean_term(input: &str) -> IResult<&str, Expression> {
    alt((
        map(preceded(tag("not "), boolean_factor), |expr| {
            Expression::Not(Box::new(expr))
        }),
        boolean_factor,
    ))(input)
}

pub fn boolean_factor(input: &str) -> IResult<&str, Expression> {
    alt((
        boolean,
        comparison_expression,
        unwrap_expression,
        tryunwrap_expression,
        iserror_expression,
        isnothing_expression,
        delimited(
            tuple((char('('), space0)),
            boolean_expression,
            tuple((space0, char(')'))),
        ),
    ))(input)
}

pub fn factor(input: &str) -> IResult<&str, Expression> {
    alt((
        delimited(
            tuple((char('('), space0)),
            arithmetic_expression,
            tuple((space0, char(')'))),
        ),
        function_call,
        ok_expression,
        err_expression,
        just_expression,
        nothing_expression,
        unwrap_expression,
        tryunwrap_expression,
        iserror_expression,
        isnothing_expression,
        real,
        integer,
        map(tuple((char('-'), space0, factor)), |(_, _, expr)| {
            Expression::Mul(Box::new(Expression::CInt(-1)), Box::new(expr))
        }),
        map(identifier, Expression::Var),
    ))(input)
}

//indented block parser
pub fn indented_block(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, _) = line_ending(input)?;
    let (input, statements) = separated_list1(
        line_ending,
        preceded(
            space1, // Require at least one space for indentation
            parse_statement,
        ),
    )(input)?;
    Ok((input, statements))
}

pub fn parse_type(type_name: &str) -> Type {
    match type_name {
        "TInteger" => Type::TInteger,
        "TBool" => Type::TBool,
        "TReal" => Type::TReal,
        _ => Type::TInteger, // Default case
    }
}

// function definition parsing

//return statement parsing

// Parse multiple statements

// function call parsing
pub fn function_call(input: &str) -> IResult<&str, Expression> {
    let (input, name) = identifier(input)?;
    let (input, _) = char('(')(input)?;
    let (input, args) = separated_list0(delimited(space0, char(','), space0), expression)(input)?;
    let (input, _) = char(')')(input)?;

    Ok((input, Expression::FuncCall(name, args)))
}

// Main parse function



pub fn type_annotation(input: &str) -> IResult<&str, Type> {
    alt((
        map(tag("Int"), |_| Type::TInteger),
        map(tag("Bool"), |_| Type::TBool),
        map(tag("Real"), |_| Type::TReal),
        map(tag("String"), |_| Type::TString),
        map(tag("Any"), |_| Type::TAny),
    ))(input)
}

pub fn match_expression(input: &str) -> IResult<&str, Statement> {
    let (input, _) = multispace0(input)?; // Skip leading spaces & newlines
    let (input, _) = tag("match")(input)?; // Parse the "match" keyword
    let (input, _) = space1(input)?; // Require at least one space after "match"
    let (input, exp) = expression(input)?; // Parse the expression to match
    let (input, _) = multispace0(input)?; // Skip spaces & newlines
    let (input, _) = char('{')(input)?; // Parse the opening brace
    let (input, _) = multispace0(input)?; // Skip spaces & newlines

    // Parse the match cases
    let (input, cases) = separated_list0(
        tuple((multispace0, char(','), multispace0)), // Allow spaces/newlines before and after `,`
        match_case,                                   // Parse each match case
    )(input)?;

    let (input, _) = multispace0(input)?; // Skip spaces & newlines
    let (input, _) = char('}')(input)?; // Parse the closing brace

    Ok((input, Statement::Match(Box::new(exp), cases)))
}

pub fn match_case(input: &str) -> IResult<&str, (Expression, Box<Statement>)> {
    //println!("Parsing match case: {}", input); // Debug print
    let (input, _) = multispace0(input)?; // Skip spaces & newlines
                                          //println!("After skipping spaces: {}", input); // Debug print
    let (input, pattern) = pattern(input)?;
    //println!("Parsed pattern: {:?}", pattern); // Debug print
    let (input, _) = space0(input)?; // Skip optional spaces
                                     //println!("After skipping spaces before =>: {}", input); // Debug print
    let (input, _) = tag("=>")(input)?; // Parse the "=>" operator
                                        //println!("After parsing =>: {}", input); // Debug print
    let (input, _) = space0(input)?; // Skip optional spaces
                                     //println!("After skipping spaces after =>: {}", input); // Debug print
    let (input, stmt) = parse_statement(input)?;
    //println!("Parsed statement: {:?}", stmt); // Debug print

    Ok((input, (pattern, Box::new(stmt))))
}
pub fn pattern(input: &str) -> IResult<&str, Expression> {
    alt((
        adt_pattern,                      // Handle ADT patterns first (e.g., "Circle r")
        map(identifier, Expression::Var), // Fallback to variables
    ))(input)
}

pub fn arg_pattern(input: &str) -> IResult<&str, Expression> {
    map(identifier, Expression::Var)(input) // Only parse variables
}

pub fn adt_pattern(input: &str) -> IResult<&str, Expression> {
    let (input, adt_name) = identifier(input)?; // Parse the ADT name
    let (input, _) = space0(input)?; // Skip optional spaces
    let (input, constructor_name) = identifier(input)?; // Parse the constructor name
    let (input, args) = many1(preceded(space1, arg_pattern))(input)?; // Parse the arguments

    Ok((
        input,
        Expression::ADTConstructor(
            adt_name,
            constructor_name,
            args.into_iter().map(Box::new).collect(),
        ),
    ))
}

const KEYWORDS: &[&str] = &[
    "if",
    "in",
    "else",
    "def",
    "while",
    "for",
    "val",
    "var",
    "return",
    "Ok",
    "Err",
    "Just",
    "Nothing",
    "unwrap",
    "tryUnwrap",
    "isNothing",
    "isError",
    "and",
    "or",
    "not",
    "True",
    "False",
];
