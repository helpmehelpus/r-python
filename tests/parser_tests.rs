use r_python::ir::ast::*;
use r_python::parser::parser::*;

#[test]
fn test_simple_assignment() {
    let input = "x = 42";
    let (rest, stmt) = parse_statement(input).unwrap();
    assert_eq!(rest, "");
    match stmt {
        Statement::Assignment(name, expr) => {
            // Added _type
            assert_eq!(name, "x");
            match *expr {
                Expression::CInt(val) => assert_eq!(val, 42),
                _ => panic!("Expected CInt"),
            }
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_complete_program() {
    let input = "x = 10\nif x > 5:\n    y = 1\nelse:\n    y = 2";
    let (rest, stmts) = parse(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2); // Assignment and IfThenElse
}

#[test]
fn test_complex_expression() {
    let input = "x = (2 * 3) + (10 - 4)";
    let (rest, stmts) = parse(input).unwrap();
    assert_eq!(rest, "");

    match &stmts[0] {
        Statement::Assignment(name, expr) => {
            // Added _type
            assert_eq!(name, "x");
            match **expr {
                Expression::Add(_, _) => (),
                _ => panic!("Expected Add expression"),
            }
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_multiline_with_if() {
    let input = "x = 10\nif x > 5:\n    y = 1\nelse:\n    y = 2";
    let (rest, stmts) = parse(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2); // Should have assignment and if-statement

    // Verify first statement is assignment
    match &stmts[0] {
        Statement::Assignment(name, expr) => {
            // Added _type
            assert_eq!(name, "x");
            assert!(matches!(**expr, Expression::CInt(10)));
        }
        _ => panic!("Expected Assignment"),
    }

    // Verify second statement is if-else
    match &stmts[1] {
        Statement::IfThenElse(condition, then_block, else_block) => {
            // Check condition - using GT instead of Comparison
            assert!(matches!(**condition, Expression::GT(_, _)));

            // Check then block
            match **then_block {
                Statement::Block(ref stmts) => {
                    assert_eq!(stmts.len(), 1);
                    match &stmts[0] {
                        Statement::Assignment(name, expr) => {
                            assert_eq!(name, "y");
                            assert!(matches!(**expr, Expression::CInt(1)));
                        }
                        _ => panic!("Expected Assignment in then block"),
                    }
                }
                _ => panic!("Expected Block"),
            }

            // Check else block
            match else_block {
                Some(else_stmt) => match **else_stmt {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::Assignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(2)));
                            }
                            _ => panic!("Expected Assignment in else block"),
                        }
                    }
                    _ => panic!("Expected Block"),
                },
                None => panic!("Expected Some else block"),
            }
        }
        _ => panic!("Expected IfThenElse"),
    }
}

#[test]
fn test_if_else_block() {
    let input = "if x > 0:\n    y = 1\nelse:\n    y = 2";
    let (rest, stmt) = parse_statement(input).unwrap();
    assert_eq!(rest, "");

    match stmt {
        Statement::IfThenElse(condition, then_block, else_block) => {
            // Check condition
            assert!(matches!(*condition, Expression::GT(_, _)));

            // Check then block
            match *then_block {
                Statement::Block(ref stmts) => {
                    assert_eq!(stmts.len(), 1);
                    match &stmts[0] {
                        Statement::Assignment(name, expr) => {
                            assert_eq!(name, "y");
                            assert!(matches!(**expr, Expression::CInt(1)));
                        }
                        _ => panic!("Expected Assignment in then block"),
                    }
                }
                _ => panic!("Expected Block"),
            }

            // Check else block
            match else_block {
                Some(else_stmt) => match *else_stmt {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::Assignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(2)));
                            }
                            _ => panic!("Expected Assignment in else block"),
                        }
                    }
                    _ => panic!("Expected Block"),
                },
                None => panic!("Expected Some else block"),
            }
        }
        _ => panic!("Expected IfThenElse"),
    }
}

#[test]
fn test_if_else_statement() {
    let input = "if x > 0:\n    y = 1\nelse:\n    y = 2";
    let (rest, stmt) = parse_statement(input).unwrap();
    assert_eq!(rest, "");

    match stmt {
        Statement::IfThenElse(condition, then_block, else_block) => {
            // Check condition
            assert!(matches!(
                *condition,
                Expression::GT(_box_ref @ _, _box_ref2 @ _)
            ));

            // Check then block
            match *then_block {
                Statement::Block(ref stmts) => {
                    assert_eq!(stmts.len(), 1);
                    match &stmts[0] {
                        Statement::Assignment(name, expr) => {
                            assert_eq!(name, "y");
                            assert!(matches!(**expr, Expression::CInt(1)));
                        }
                        _ => panic!("Expected Assignment"),
                    }
                }
                _ => panic!("Expected Block"),
            }

            // Check else block
            match else_block {
                Some(else_stmt) => match *else_stmt {
                    Statement::Block(ref stmts) => {
                        assert_eq!(stmts.len(), 1);
                        match &stmts[0] {
                            Statement::Assignment(name, expr) => {
                                assert_eq!(name, "y");
                                assert!(matches!(**expr, Expression::CInt(2)));
                            }
                            _ => panic!("Expected Assignment"),
                        }
                    }
                    _ => panic!("Expected Block"),
                },
                None => panic!("Expected Some else block"),
            }
        }
        _ => panic!("Expected IfThenElse"),
    }
}

#[test]
fn test_for_statement() {
    let input = "for x in range:\n   x = x+1";
    let (rest, stmt) = parse_statement(input).unwrap();
    let expected = Statement::For(
        "x".to_string(),
        Box::new(Expression::Var("range".to_string())),
        Box::new(Statement::Block(
            [Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::Add(
                    Box::new(Expression::Var("x".to_string())),
                    Box::new(Expression::CInt(1)),
                )),
            )]
            .to_vec(),
        )),
    );
    assert_eq!(rest, "");
    assert_eq!(stmt, expected)
}

#[test]
fn test_multiline_parse() {
    let input = "x = 42\ny = 10";
    let (rest, stmts) = parse(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);

    match &stmts[0] {
        Statement::Assignment(name, expr) => {
            assert_eq!(&**name, "x");
            match **expr {
                Expression::CInt(42) => (),
                _ => panic!("Expected CInt(42)"),
            }
        }
        _ => panic!("Expected Assignment"),
    }

    match &stmts[1] {
        Statement::Assignment(name, expr) => {
            assert_eq!(&**name, "y");
            match **expr {
                Expression::CInt(10) => (),
                _ => panic!("Expected CInt(10)"),
            }
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_whitespace_handling() {
    let input = "   x    =    42   \n   y   =   10   ";
    let (rest, stmts) = parse(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
}

#[test]
fn test_function_definition() {
    let input = r#"def add(x: TInteger, y: TInteger) -> TInteger:
        return x + y"#;
    let (rest, stmt) = parse_statement(input).unwrap();
    assert_eq!(rest, "");
    match stmt {
        Statement::FuncDef(func) => {
            assert_eq!(func.name, "add");
            assert_eq!(func.kind, Some(Type::TInteger));
            match func.params {
                Some(params) => {
                    assert_eq!(params.len(), 2);
                    assert_eq!(params[0].0, "x");
                    assert_eq!(params[1].0, "y");
                }
                None => panic!("Expected Some params"),
            }
            assert_eq!(
                func.body,
                Some(Box::new(Statement::Block(vec![Statement::Return(
                    Box::new(Expression::Add(
                        Box::new(Expression::Var("x".to_string())),
                        Box::new(Expression::Var("y".to_string()))
                    ))
                )])))
            );
        }
        _ => panic!("Expected FuncDef"),
    }
}

#[test]
fn test_function_call() {
    let input = "result = add(5, 3)";
    let (rest, stmt) = parse_statement(input).unwrap();
    assert_eq!(rest, "");
    match stmt {
        Statement::Assignment(name, expr) => {
            assert_eq!(name, "result");
            match *expr {
                Expression::FuncCall(func_name, args) => {
                    assert_eq!(func_name, "add");
                    assert_eq!(args.len(), 2);
                }
                _ => panic!("Expected FuncCall"),
            }
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn test_basic_arithmetic_left_recursion() {
    let cases = vec![
        (
            "1 + 2",
            Expression::Add(Box::new(Expression::CInt(1)), Box::new(Expression::CInt(2))),
        ),
        (
            "3 * 4",
            Expression::Mul(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(4))),
        ),
    ];

    for (input, expected) in cases {
        let (rest, result) = parse_expression(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
fn test_operator_precedence() {
    let input = "2 + 3 * 4";
    let expected = Expression::Add(
        Box::new(Expression::CInt(2)),
        Box::new(Expression::Mul(
            Box::new(Expression::CInt(3)),
            Box::new(Expression::CInt(4)),
        )),
    );

    let (rest, result) = parse_expression(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_left_associativity() {
    let input = "1 - 2 - 3"; // Should parse as (1-2)-3, not 1-(2-3)
    let expected = Expression::Sub(
        Box::new(Expression::Sub(
            Box::new(Expression::CInt(1)),
            Box::new(Expression::CInt(2)),
        )),
        Box::new(Expression::CInt(3)),
    );

    let (rest, result) = parse_expression(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_nested_expressions() {
    let input = "(1 + 2) * (3 + 4)";
    let expected = Expression::Mul(
        Box::new(Expression::Add(
            Box::new(Expression::CInt(1)),
            Box::new(Expression::CInt(2)),
        )),
        Box::new(Expression::Add(
            Box::new(Expression::CInt(3)),
            Box::new(Expression::CInt(4)),
        )),
    );

    let (rest, result) = parse_expression(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_complex_expression_2() {
    let input = "1 + 2 * 3 + 4 * 5";
    let expected = Expression::Add(
        Box::new(Expression::Add(
            Box::new(Expression::CInt(1)),
            Box::new(Expression::Mul(
                Box::new(Expression::CInt(2)),
                Box::new(Expression::CInt(3)),
            )),
        )),
        Box::new(Expression::Mul(
            Box::new(Expression::CInt(4)),
            Box::new(Expression::CInt(5)),
        )),
    );

    let (rest, result) = parse_expression(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_negative_numbers_with_operations() {
    let cases = vec![
        (
            "-1 + 2",
            Expression::Add(
                Box::new(Expression::CInt(-1)),
                Box::new(Expression::CInt(2)),
            ),
        ),
        (
            "3 * -4",
            Expression::Mul(
                Box::new(Expression::CInt(3)),
                Box::new(Expression::CInt(-4)),
            ),
        ),
    ];

    for (input, expected) in cases {
        let (rest, result) = parse_expression(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
fn test_boolean_literals() {
    let cases = vec![("True", Expression::CTrue), ("False", Expression::CFalse)];

    for (input, expected) in cases {
        let (rest, result) = boolean(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
fn test_real_numbers() {
    let cases = vec![
        ("3.14", Expression::CReal(3.14)),
        ("-2.5", Expression::CReal(-2.5)),
        ("0.0", Expression::CReal(0.0)),
    ];

    for (input, expected) in cases {
        let (rest, result) = parse_expression(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
fn test_string_literals() {
    let cases = vec![
        ("\"hello\"", Expression::CString("hello".to_string())),
        ("\"123\"", Expression::CString("123".to_string())),
        ("\"\"", Expression::CString("".to_string())),
    ];

    for (input, expected) in cases {
        let (rest, result) = string(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
fn test_boolean_operations() {
    let cases = vec![
        (
            "True and False",
            Expression::And(Box::new(Expression::CTrue), Box::new(Expression::CFalse)),
        ),
        (
            "True or False",
            Expression::Or(Box::new(Expression::CTrue), Box::new(Expression::CFalse)),
        ),
        ("not True", Expression::Not(Box::new(Expression::CTrue))),
    ];

    for (input, expected) in cases {
        let (rest, result) = boolean_expression(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
fn test_complex_boolean_expressions() {
    let input = "not (True and False) or True";
    let expected = Expression::Or(
        Box::new(Expression::Not(Box::new(Expression::And(
            Box::new(Expression::CTrue),
            Box::new(Expression::CFalse),
        )))),
        Box::new(Expression::CTrue),
    );

    let (rest, result) = boolean_expression(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_eval_iserror_err_expression() {
    let input = "isError (Err (1))";
    let (rest, result) = iserror_expression(input).unwrap();
    let expected = Expression::IsError(Box::new(Expression::CErr(Box::new(Expression::CInt(1)))));
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_eval_iserror_ok_expression() {
    let input = "isError (Ok (2))";
    let (rest, result) = iserror_expression(input).unwrap();
    let expected = Expression::IsError(Box::new(Expression::COk(Box::new(Expression::CInt(2)))));
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_eval_iserror_real() {
    let input = "isError (3.14)";
    let (rest, result) = iserror_expression(input).unwrap();
    let expected = Expression::IsError(Box::new(Expression::CReal(3.14)));
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_eval_isnothing_nothing_expression() {
    let input = "isNothing(Nothing)";
    let (rest, result) = isnothing_expression(input).unwrap();
    let expected = Expression::IsNothing(Box::new(Expression::CNothing));
    assert_eq!(rest, "");
    assert_eq!(result, expected);
    //Necessita da implementação de definição de Nothing.
}

#[test]
fn test_eval_isnothing_just_expression() {
    let input = "isNothing (Just (2))";
    let (rest, result) = isnothing_expression(input).unwrap();
    let expected =
        Expression::IsNothing(Box::new(Expression::CJust(Box::new(Expression::CInt(2)))));
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_eval_isnothing_real() {
    let input = "isNothing (4.20)";
    let (rest, result) = isnothing_expression(input).unwrap();
    let expected = Expression::IsNothing(Box::new(Expression::CReal(4.20)));
    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_ok_creation() {
    let cases = vec![
        ("Ok(1)", Expression::COk(Box::new(Expression::CInt(1)))),
        ("Ok(0.5)", Expression::COk(Box::new(Expression::CReal(0.5)))),
        ("Err(False)", Expression::CErr(Box::new(Expression::CFalse))),
    ];

    for (input, expected) in cases {
        let (rest, result) = parse_expression(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
fn test_try_unwrap_expression() {
    let input = "tryUnwrap(Ok(42))";
    let expected = Expression::Propagate(Box::new(Expression::COk(Box::new(Expression::CInt(42)))));

    let (remaining, parsed) = tryunwrap_expression(input).expect("Parsing failed");

    assert_eq!(parsed, expected);
    assert!(
        remaining.is_empty(),
        "Remaining input should be empty but got: {}",
        remaining
    );
}

#[test]
fn test_unwrap_parsing() {
    let cases = vec![
        (
            "unwrap(Ok(2))",
            Expression::Unwrap(Box::new(Expression::COk(Box::new(Expression::CInt(2))))),
        ),
        (
            "unwrap(Ok(2.5))",
            Expression::Unwrap(Box::new(Expression::COk(Box::new(Expression::CReal(2.5))))),
        ),
        (
            "unwrap(3)",
            Expression::Unwrap(Box::new(Expression::CInt(3))),
        ),
        (
            "unwrap(3.5)",
            Expression::Unwrap(Box::new(Expression::CReal(3.5))),
        ),
    ];

    for (input, expected) in cases {
        let (rest, result) = unwrap_expression(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
#[ignore]
fn test_propagation_parsing() {
    let cases = vec![
        (
            "tryUnwrap(Ok(2))",
            Expression::Propagate(Box::new(Expression::COk(Box::new(Expression::CInt(2))))),
        ),
        (
            "tryUnwrap(tryUnwrap(x))",
            Expression::Propagate(Box::new(Expression::Propagate(Box::new(Expression::Var(
                String::from("x"),
            ))))),
        ),
        (
            "tryUnwrap(Ok(10.1 + 1.2))",
            Expression::Propagate(Box::new(Expression::COk(Box::new(Expression::Add(
                Box::new(Expression::CReal(10.1)),
                Box::new(Expression::CReal(1.2)),
            ))))),
        ),
        /*(
            "tryUnwrap(Ok(1)) / tryUnwrap(Just(2))",
            Expression::Div(
                Box::new(Expression::Propagate(Box::new(Expression::COk(Box::new(
                    Expression::CInt(1),
                ))))),
                Box::new(Expression::Propagate(Box::new(Expression::CJust(
                    Box::new(Expression::CInt(2)),
                )))),
            ),
        ),*/
        (
            "tryUnwrap(Ok(True)) and tryUnwrap(Ok(False))",
            Expression::And(
                Box::new(Expression::Propagate(Box::new(Expression::COk(Box::new(
                    Expression::CTrue,
                ))))),
                Box::new(Expression::Propagate(Box::new(Expression::COk(Box::new(
                    Expression::CFalse,
                ))))),
            ),
        ),
        (
            "tryUnwrap(tryUnwrap(Ok(True or False)))",
            Expression::Propagate(Box::new(Expression::Propagate(Box::new(Expression::COk(
                Box::new(Expression::Or(
                    Box::new(Expression::CTrue),
                    Box::new(Expression::CFalse),
                )),
            ))))),
        ),
        (
            "tryUnwrap(Just(not False))",
            Expression::Propagate(Box::new(Expression::CJust(Box::new(Expression::Not(
                Box::new(Expression::CFalse),
            ))))),
        ),
    ];

    for (input, expected) in cases {
        let (rest, result) = parse_expression(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, expected);
    }
}

#[test]
fn test_propagation_parsing_statements() {
    let input = "x = Ok(True)\nif unwrap(x):\n  y = 1\nif tryUnwrap(x):\n  y = 1\n";

    let (rest, result) = parse(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(
        result,
        [
            Statement::Assignment(
                String::from("x"),
                Box::new(Expression::COk(Box::new(Expression::CTrue)))
            ),
            Statement::IfThenElse(
                Box::new(Expression::Unwrap(Box::new(Expression::Var(String::from(
                    "x"
                ))))),
                Box::new(Statement::Block(vec![Statement::Assignment(
                    String::from("y"),
                    Box::new(Expression::CInt(1))
                )])),
                None
            ),
            Statement::IfThenElse(
                Box::new(Expression::Propagate(Box::new(Expression::Var(
                    String::from("x")
                )))),
                Box::new(Statement::Block(vec![Statement::Assignment(
                    String::from("y"),
                    Box::new(Expression::CInt(1))
                )])),
                None
            )
        ]
    );
}

#[test]
fn test_eval_just_integer() {
    let input = "Just (42)";
    let (rest, result) = just_expression(input).unwrap();
    let expected = Expression::CJust(Box::new(Expression::CInt(42)));

    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_eval_just_real() {
    let input = "Just (3.14)";
    let (rest, result) = just_expression(input).unwrap();
    let expected = Expression::CJust(Box::new(Expression::CReal(3.14)));

    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
#[ignore]
fn test_eval_just_expression() {
    let input = "Just (1 + 2)";
    let (rest, result) = just_expression(input).unwrap();
    let expected = Expression::CJust(Box::new(Expression::Add(
        Box::new(Expression::CInt(1)),
        Box::new(Expression::CInt(2)),
    )));

    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_eval_nothing() {
    let input = "Nothing";
    let (rest, result) = nothing_expression(input).unwrap();
    let expected = Expression::CNothing;

    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_eval_isnothing_nothing() {
    let input = "isNothing (Nothing)";
    let (rest, result) = isnothing_expression(input).unwrap();
    let expected = Expression::IsNothing(Box::new(Expression::CNothing));

    assert_eq!(rest, "");
    assert_eq!(result, expected);
}

#[test]
fn test_create_function_with_keyword_if() {
    let input = "def if(x: TInteger) -> TInteger:\n    return x";
    let result = parse_statement(input);

    assert!(result.is_err());
}

#[test]
fn test_create_function_with_keyword_while() {
    let input = "def while(x: TInteger) -> TInteger:\n    return x";
    let result = parse_statement(input);

    assert!(result.is_err());
}

#[test]
fn test_create_function_with_keyword_ok() {
    let input = "def Ok(x: TInteger) -> TInteger:\n    return x";
    let result = parse_statement(input);

    assert!(result.is_err());
}

#[test]
fn test_var_declaration_with_keyword_if() {
    let input = "if = 10";
    let result = parse_statement(input);

    assert!(result.is_err());
}

#[test]
fn test_var_declaration_with_keyword_while() {
    let input = "while = 10";
    let result = parse_statement(input);

    assert!(result.is_err());
}

#[test]
fn test_var_declaration_with_keyword_ok() {
    let input = "Ok = 10";
    let result = parse_statement(input);

    assert!(result.is_err());
}


#[test]
fn parser_test_adt_declaration() {
    // Define the ADT for geometric shapes
    let adt_input = "data FG = Circle Bool | Rectangle Bool Bool | Triangle Bool Bool Bool";
    let adt_result = parse_statement(adt_input);
    
    assert!(adt_result.is_ok())
}
