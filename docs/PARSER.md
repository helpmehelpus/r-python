# Parser Component Documentation

## Overview

The parser component is responsible for transforming source code text into an Abstract Syntax Tree (AST). It is implemented using the `nom` parser combinator library and follows a modular design pattern, breaking down the parsing logic into several specialized modules.

## Architecture

The parser is organized into the following modules:

- `parser.rs`: The main entry point that coordinates the parsing process
- `parser_common.rs`: Common parsing utilities and shared functions
- `parser_expr.rs`: Expression parsing functionality
- `parser_type.rs`: Type system parsing
- `parser_stmt.rs`: Statement and control flow parsing

### Module Responsibilities and Public Interface

#### 1. parser.rs
The main parser module that provides the entry point for parsing complete programs:
```rust
pub fn parse(input: &str) -> IResult<&str, Vec<Statement>>
```

#### 2. parser_common.rs
Common parsing utilities used across other modules:
```rust
pub fn is_string_char(c: char) -> bool
pub fn separator<'a>(sep: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str>
pub fn keyword<'a>(kw: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str>
pub fn identifier(input: &str) -> IResult<&str, &str>
```

#### 3. parser_expr.rs
Expression parsing functionality:
```rust
pub fn parse_expression(input: &str) -> IResult<&str, Expression>
pub fn parse_actual_arguments(input: &str) -> IResult<&str, Vec<Expression>>
```

#### 4. parser_type.rs
Type system parsing:
```rust
pub fn parse_type(input: &str) -> IResult<&str, Type>
```

#### 5. parser_stmt.rs
Statement and control flow parsing:
```rust
pub fn parse_statement(input: &str) -> IResult<&str, Statement>
```

## Parser Features

### Statement Parsing
The parser supports various types of statements:
- Variable declarations and assignments
- Control flow (if-else, while, for)
- Function definitions
- Assert statements
- ADT (Algebraic Data Type) declarations

### Expression Parsing
Handles different types of expressions:
- Arithmetic expressions
- Boolean expressions
- Function calls
- Variables
- Literals (numbers, strings, booleans)
- ADT constructors and pattern matching

### Type System
Supports a rich type system including:
- Basic types (Int, Real, Boolean, String, Unit, Any)
- Complex types (List, Tuple, Maybe)
- ADT declarations
- Function types

## nom Parser Combinators

The parser extensively uses the `nom` parser combinator library. Here are the key combinators used:

### Basic Combinators
- `tag`: Matches exact string patterns
- `char`: Matches single characters
- `digit1`: Matches one or more digits
- `alpha1`: Matches one or more alphabetic characters
- `space0/space1`: Matches zero or more/one or more whitespace characters

### Sequence Combinators
- `tuple`: Combines multiple parsers in sequence
- `preceded`: Matches a prefix followed by a value
- `terminated`: Matches a value followed by a suffix
- `delimited`: Matches a value between two delimiters

### Branch Combinators
- `alt`: Tries multiple parsers in order
- `map`: Transforms the output of a parser
- `opt`: Makes a parser optional

### Multi Combinators
- `many0/many1`: Matches zero or more/one or more occurrences
- `separated_list0`: Matches items separated by a delimiter

## Example Usage

Here's an example of how the parser handles a simple assignment statement:

```python
x = 42
```

This is parsed using the following combinators:
```rust
fn parse_assignment_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            preceded(multispace0, identifier),
            preceded(multispace0, tag("=")),
            preceded(multispace0, parse_expression),
        )),
        |(var, _, expr)| Statement::Assignment(var.to_string(), Box::new(expr)),
    )(input)
}
```

## AST Structure

The parser produces an Abstract Syntax Tree (AST) with the following main types:

### Statements
```rust
pub enum Statement {
    VarDeclaration(Name),
    ValDeclaration(Name),
    Assignment(Name, Box<Expression>),
    IfThenElse(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    For(Name, Box<Expression>, Box<Statement>),
    Block(Vec<Statement>),
    Assert(Box<Expression>, Box<Expression>),
    FuncDef(Function),
    Return(Box<Expression>),
    ADTDeclaration(Name, Vec<ValueConstructor>),
    // ... other variants
}
```

### Types
```rust
pub enum Type {
    TInteger,
    TReal,
    TBool,
    TString,
    TList(Box<Type>),
    TTuple(Vec<Type>),
    TMaybe(Box<Type>),
    TResult(Box<Type>, Box<Type>),
    TFunction(Box<Option<Type>>, Vec<Type>),
    // ... other variants
}
```

## Error Handling

The parser implements error handling through the `nom` error system:
```rust
pub enum ParseError {
    IndentationError(usize),
    UnexpectedToken(String),
    InvalidExpression(String),
}
```

## Testing

The parser includes a comprehensive test suite in `tests/parser_tests.rs` that verifies:
- Simple assignments
- Complex expressions
- Control flow structures
- Type annotations
- Complete programs
- Error handling
- Whitespace handling 


> **Documentation Generation Note**  
> This documentation was automatically generated by Claude (Anthropic), an AI assistant, through analysis of the codebase. While the content accurately reflects the implementation, it should be reviewed and maintained by the development team. Last generated: June 2025.
