use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Name};

type ErrorMessage = (String, Option<Expression>);

pub fn eval(exp: Expression, env: &Environment<Expression>) -> Result<Expression, ErrorMessage> {
    match exp {
        Expression::Add(lhs, rhs) => add(*lhs, *rhs, env),
        Expression::Sub(lhs, rhs) => sub(*lhs, *rhs, env),
        Expression::Mul(lhs, rhs) => mul(*lhs, *rhs, env),
        Expression::Div(lhs, rhs) => div(*lhs, *rhs, env),
        Expression::And(lhs, rhs) => and(*lhs, *rhs, env),
        Expression::Or(lhs, rhs) => or(*lhs, *rhs, env),
        Expression::Not(lhs) => not(*lhs, env),
        Expression::EQ(lhs, rhs) => eq(*lhs, *rhs, env),
        Expression::GT(lhs, rhs) => gt(*lhs, *rhs, env),
        Expression::LT(lhs, rhs) => lt(*lhs, *rhs, env),
        Expression::GTE(lhs, rhs) => gte(*lhs, *rhs, env),
        Expression::LTE(lhs, rhs) => lte(*lhs, *rhs, env),
        Expression::Var(name) => lookup(name, env),
        Expression::COk(e) => eval_ok(*e, env),
        Expression::CErr(e) => eval_err(*e, env),
        Expression::CJust(e) => eval_just(*e, env),
        Expression::Unwrap(e) => eval_unwrap_expression(*e, env),
        Expression::Propagate(e) => eval_propagate_expression(*e, env),
        Expression::IsError(e) => eval_iserror_expression(*e, env),
        Expression::IsNothing(e) => eval_isnothing_expression(*e, env),
        Expression::FuncCall(name, args) => call(name, args, env),
        Expression::ListValue(values) => eval_list_value(values, env),
        _ if is_constant(exp.clone()) => Ok(exp),
        _ => Err((String::from("Not implemented yet."), None)),
    }
}

pub fn lookup(name: String, env: &Environment<Expression>) -> Result<Expression, ErrorMessage> {
    match env.lookup(&name) {
        Some((_, value)) => Ok(value.clone()),
        None => Err((format!("Variable '{}' not found", name), None)),
    }
}

pub fn call(
    name: Name,
    args: Vec<Expression>,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    match env.lookup_function(&name) {
        Some(func) => {
            let mut new_env = Environment::new();

            // Copy global functions
            for (name, (_, value)) in env.get_all_variables() {
                if let Expression::FuncCall(_, _) = value {
                    new_env.map_variable(name.clone(), false, value.clone());
                }
            }

            // Bind arguments
            for (i, formal_arg) in func.params.iter().enumerate() {
                if i >= args.len() {
                    return Err((
                        format!(
                            "[Runtime Error on '{}()'] missing argument '{}'.",
                            name, formal_arg.argument_name
                        ),
                        None,
                    ));
                }
                let arg_value = eval(args[i].clone(), env)?;
                new_env.map_variable(formal_arg.argument_name.clone(), false, arg_value);
            }

            if args.len() > func.params.len() {
                return Err((
                    format!("[Runtime Error on '{}()'] too many arguments.", name),
                    None,
                ));
            }

            // Execute function
            match super::statement_execute::execute(*func.body.as_ref().unwrap().clone(), &new_env)
            {
                Ok(_) => Err(("Function did not return a value".to_string(), None)),
                Err((_, Some(value))) => Ok(value),
                Err(e) => Err(e),
            }
        }
        _ => Err((format!("Function {} not found", name), None)),
    }
}

// Arithmetic Operations
fn eval_binary_arith_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
    op: F,
    error_msg: &str,
) -> Result<Expression, ErrorMessage>
where
    F: Fn(f64, f64) -> f64,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;

    match (v1, v2) {
        (Expression::CInt(v1), Expression::CInt(v2)) => {
            Ok(Expression::CInt(op(v1 as f64, v2 as f64) as i32))
        }
        (Expression::CInt(v1), Expression::CReal(v2)) => Ok(Expression::CReal(op(v1 as f64, v2))),
        (Expression::CReal(v1), Expression::CInt(v2)) => Ok(Expression::CReal(op(v1, v2 as f64))),
        (Expression::CReal(v1), Expression::CReal(v2)) => Ok(Expression::CReal(op(v1, v2))),
        _ => Err((error_msg.to_string(), None)),
    }
}

fn add(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a + b,
        "addition '(+)' is only defined for numbers (integers and real).",
    )
}
fn sub(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a - b,
        "subtraction '(-)' is only defined for numbers (integers and real).",
    )
}
fn mul(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a * b,
        "multiplication '(*)' is only defined for numbers (integers and real).",
    )
}
fn div(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_arith_op(
        lhs,
        rhs,
        env,
        |a, b| a / b,
        "division '(/)' is only defined for numbers (integers and real).",
    )
}

// Boolean Operations
fn eval_binary_boolean_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
    op: F,
    error_msg: &str,
) -> Result<Expression, ErrorMessage>
where
    F: Fn(bool, bool) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;

    match (v1, v2) {
        (Expression::CTrue, Expression::CTrue) => Ok(op(true, true)),
        (Expression::CTrue, Expression::CFalse) => Ok(op(true, false)),
        (Expression::CFalse, Expression::CTrue) => Ok(op(false, true)),
        (Expression::CFalse, Expression::CFalse) => Ok(op(false, false)),
        _ => Err((error_msg.to_string(), None)),
    }
}
fn and(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a && b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "'and' is only defined for booleans.",
    )
}
fn or(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_boolean_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a || b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "'or' is only defined for booleans.",
    )
}
fn not(lhs: Expression, env: &Environment<Expression>) -> Result<Expression, ErrorMessage> {
    let v = eval(lhs, env)?;
    match v {
        Expression::CTrue => Ok(Expression::CFalse),
        Expression::CFalse => Ok(Expression::CTrue),
        _ => Err((String::from("'not' is only defined for booleans."), None)),
    }
}

// Relational Operations
fn eval_binary_rel_op<F>(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
    op: F,
    error_msg: &str,
) -> Result<Expression, ErrorMessage>
where
    F: Fn(f64, f64) -> Expression,
{
    let v1 = eval(lhs, env)?;
    let v2 = eval(rhs, env)?;

    match (v1, v2) {
        (Expression::CInt(v1), Expression::CInt(v2)) => Ok(op(v1 as f64, v2 as f64)),
        (Expression::CInt(v1), Expression::CReal(v2)) => Ok(op(v1 as f64, v2)),
        (Expression::CReal(v1), Expression::CInt(v2)) => Ok(op(v1, v2 as f64)),
        (Expression::CReal(v1), Expression::CReal(v2)) => Ok(op(v1, v2)),
        _ => Err((error_msg.to_string(), None)),
    }
}
fn eq(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a == b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(==) is only defined for numbers (integers and real).",
    )
}
fn gt(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a > b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(>) is only defined for numbers (integers and real).",
    )
}
fn lt(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a < b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(<) is only defined for numbers (integers and real).",
    )
}
fn gte(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a >= b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(>=) is only defined for numbers (integers and real).",
    )
}
fn lte(
    lhs: Expression,
    rhs: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    eval_binary_rel_op(
        lhs,
        rhs,
        env,
        |a, b| {
            if a <= b {
                Expression::CTrue
            } else {
                Expression::CFalse
            }
        },
        "(<=) is only defined for numbers (integers and real).",
    )
}

// Other helpers
fn eval_unwrap_expression(
    exp: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        Expression::CJust(e) => Ok(*e),
        Expression::COk(e) => Ok(*e),
        _ => Err((String::from("Program panicked trying to unwrap."), None)),
    }
}
fn eval_propagate_expression(
    exp: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        Expression::CJust(e) => Ok(*e),
        Expression::COk(e) => Ok(*e),
        Expression::CErr(e) => Err(("Propagate".to_string(), Some(*e))),
        Expression::CNothing => Err((
            "Propagate".to_string(),
            Some(Expression::CString("Couldn't unwrap Nothing".to_string())),
        )),
        _ => Err((String::from("'propagate' is expects a Just or Ok."), None)),
    }
}
fn eval_isnothing_expression(
    exp: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        Expression::CNothing => Ok(Expression::CTrue),
        _ => Ok(Expression::CFalse),
    }
}
fn eval_iserror_expression(
    exp: Expression,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    let v = eval(exp, env)?;
    match v {
        Expression::CErr(_) => Ok(Expression::CTrue),
        _ => Ok(Expression::CFalse),
    }
}
fn eval_just(exp: Expression, env: &Environment<Expression>) -> Result<Expression, ErrorMessage> {
    let v = eval(exp, env)?;
    Ok(Expression::CJust(Box::new(v)))
}
fn eval_ok(exp: Expression, env: &Environment<Expression>) -> Result<Expression, ErrorMessage> {
    let v = eval(exp, env)?;
    Ok(Expression::COk(Box::new(v)))
}
fn eval_err(exp: Expression, env: &Environment<Expression>) -> Result<Expression, ErrorMessage> {
    let v = eval(exp, env)?;
    Ok(Expression::CErr(Box::new(v)))
}

fn is_constant(exp: Expression) -> bool {
    match exp {
        Expression::CTrue => true,
        Expression::CFalse => true,
        Expression::CVoid => true,
        Expression::CInt(_) => true,
        Expression::CReal(_) => true,
        Expression::CString(_) => true,
        Expression::CNothing => true,
        _ => false,
    }
}

fn eval_list_value(
    sub_expressions: Vec<Expression>,
    env: &Environment<Expression>,
) -> Result<Expression, ErrorMessage> {
    let mut values = Vec::new();
    for exp in sub_expressions {
        values.push(eval(exp, env)?);
    }
    Ok(Expression::ListValue(values))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::environment::Environment;

    fn create_test_env() -> Environment<Expression> {
        let mut env = Environment::new();

        // Add some variables for testing
        env.map_variable("x".to_string(), false, Expression::CInt(10));
        env.map_variable("y".to_string(), false, Expression::CReal(3.14));
        env.map_variable("flag".to_string(), false, Expression::CTrue);
        env.map_variable(
            "name".to_string(),
            false,
            Expression::CString("test".to_string()),
        );

        env
    }

    #[test]
    fn test_empty_list() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Expression::ListValue(vec![]));
    }

    #[test]
    fn test_list_with_constants() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(3),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(3),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_reals() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CReal(3.14),
            Expression::CReal(2.71),
            Expression::CReal(1.0),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CReal(3.14),
            Expression::CReal(2.71),
            Expression::CReal(1.0),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_strings() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CString("hello".to_string()),
            Expression::CString("world".to_string()),
            Expression::CString("test".to_string()),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CString("hello".to_string()),
            Expression::CString("world".to_string()),
            Expression::CString("test".to_string()),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_booleans() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CTrue,
            Expression::CFalse,
            Expression::CTrue,
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CTrue,
            Expression::CFalse,
            Expression::CTrue,
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_mixed_numbers() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CInt(42),
            Expression::CReal(3.14),
            Expression::CInt(10),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CInt(42),
            Expression::CReal(3.14),
            Expression::CInt(10),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_integer_variables() {
        let mut env = create_test_env();
        env.map_variable("a".to_string(), false, Expression::CInt(5));
        env.map_variable("b".to_string(), false, Expression::CInt(15));

        let list_expr = Expression::ListValue(vec![
            Expression::Var("x".to_string()), // 10
            Expression::Var("a".to_string()), // 5
            Expression::Var("b".to_string()), // 15
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CInt(10),
            Expression::CInt(5),
            Expression::CInt(15),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_string_variables() {
        let mut env = create_test_env();
        env.map_variable(
            "greeting".to_string(),
            false,
            Expression::CString("hello".to_string()),
        );
        env.map_variable(
            "world".to_string(),
            false,
            Expression::CString("world".to_string()),
        );

        let list_expr = Expression::ListValue(vec![
            Expression::Var("name".to_string()),     // "test"
            Expression::Var("greeting".to_string()), // "hello"
            Expression::Var("world".to_string()),    // "world"
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CString("test".to_string()),
            Expression::CString("hello".to_string()),
            Expression::CString("world".to_string()),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_arithmetic_expressions() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::Add(Box::new(Expression::CInt(1)), Box::new(Expression::CInt(2))),
            Expression::Mul(Box::new(Expression::CInt(3)), Box::new(Expression::CInt(4))),
            Expression::Sub(
                Box::new(Expression::Var("x".to_string())),
                Box::new(Expression::CInt(5)),
            ),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CInt(3),  // 1 + 2
            Expression::CInt(12), // 3 * 4
            Expression::CInt(5),  // 10 - 5
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_boolean_expressions() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::And(Box::new(Expression::CTrue), Box::new(Expression::CFalse)),
            Expression::Or(Box::new(Expression::CTrue), Box::new(Expression::CFalse)),
            Expression::Not(Box::new(Expression::CFalse)),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CFalse, // True and False
            Expression::CTrue,  // True or False
            Expression::CTrue,  // not False
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_relational_expressions() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::GT(Box::new(Expression::CInt(5)), Box::new(Expression::CInt(3))),
            Expression::LT(Box::new(Expression::CInt(2)), Box::new(Expression::CInt(8))),
            Expression::EQ(Box::new(Expression::CInt(4)), Box::new(Expression::CInt(4))),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CTrue, // 5 > 3
            Expression::CTrue, // 2 < 8
            Expression::CTrue, // 4 == 4
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_maybe_integers() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CJust(Box::new(Expression::CInt(42))),
            Expression::CNothing,
            Expression::CJust(Box::new(Expression::CInt(10))),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CJust(Box::new(Expression::CInt(42))),
            Expression::CNothing,
            Expression::CJust(Box::new(Expression::CInt(10))),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_maybe_strings() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CJust(Box::new(Expression::CString("hello".to_string()))),
            Expression::CJust(Box::new(Expression::CString("world".to_string()))),
            Expression::CNothing,
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CJust(Box::new(Expression::CString("hello".to_string()))),
            Expression::CJust(Box::new(Expression::CString("world".to_string()))),
            Expression::CNothing,
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_result_integers() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::COk(Box::new(Expression::CInt(100))),
            Expression::CErr(Box::new(Expression::CString("error".to_string()))),
            Expression::COk(Box::new(Expression::CInt(42))),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::COk(Box::new(Expression::CInt(100))),
            Expression::CErr(Box::new(Expression::CString("error".to_string()))),
            Expression::COk(Box::new(Expression::CInt(42))),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_result_strings() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::COk(Box::new(Expression::CString("success".to_string()))),
            Expression::COk(Box::new(Expression::CString("another".to_string()))),
            Expression::CErr(Box::new(Expression::CString("failure".to_string()))),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::COk(Box::new(Expression::CString("success".to_string()))),
            Expression::COk(Box::new(Expression::CString("another".to_string()))),
            Expression::CErr(Box::new(Expression::CString("failure".to_string()))),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_unwrap_expressions() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::Unwrap(Box::new(Expression::CJust(Box::new(Expression::CInt(5))))),
            Expression::Unwrap(Box::new(Expression::COk(Box::new(Expression::CString(
                "ok".to_string(),
            ))))),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CInt(5),
            Expression::CString("ok".to_string()),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_nested_integer_lists() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::ListValue(vec![Expression::CInt(1), Expression::CInt(2)]),
            Expression::ListValue(vec![Expression::CInt(3), Expression::CInt(4)]),
            Expression::ListValue(vec![Expression::CInt(5)]),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::ListValue(vec![Expression::CInt(1), Expression::CInt(2)]),
            Expression::ListValue(vec![Expression::CInt(3), Expression::CInt(4)]),
            Expression::ListValue(vec![Expression::CInt(5)]),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_nested_string_lists() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::ListValue(vec![
                Expression::CString("a".to_string()),
                Expression::CString("b".to_string()),
            ]),
            Expression::ListValue(vec![
                Expression::CString("c".to_string()),
                Expression::CString("d".to_string()),
            ]),
            Expression::ListValue(vec![]),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::ListValue(vec![
                Expression::CString("a".to_string()),
                Expression::CString("b".to_string()),
            ]),
            Expression::ListValue(vec![
                Expression::CString("c".to_string()),
                Expression::CString("d".to_string()),
            ]),
            Expression::ListValue(vec![]),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_invalid_variable() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CInt(1),
            Expression::Var("nonexistent".to_string()),
            Expression::CInt(3),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.0, "Variable 'nonexistent' not found");
    }

    #[test]
    fn test_list_with_invalid_arithmetic() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CInt(1),
            Expression::Add(
                Box::new(Expression::CString("hello".to_string())),
                Box::new(Expression::CInt(2)),
            ),
            Expression::CInt(3),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(
            error.0,
            "addition '(+)' is only defined for numbers (integers and real)."
        );
    }

    #[test]
    fn test_list_with_unwrap_failure() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CInt(1),
            Expression::Unwrap(Box::new(Expression::CNothing)),
            Expression::CInt(3),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.0, "Program panicked trying to unwrap.");
    }

    #[test]
    fn test_list_with_propagate_expressions() {
        let env = create_test_env();

        // Test successful propagation
        let list_expr = Expression::ListValue(vec![
            Expression::Propagate(Box::new(Expression::CJust(Box::new(Expression::CInt(42))))),
            Expression::Propagate(Box::new(Expression::COk(Box::new(Expression::CString(
                "success".to_string(),
            ))))),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CInt(42),
            Expression::CString("success".to_string()),
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_propagate_error() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::CInt(1),
            Expression::Propagate(Box::new(Expression::CErr(Box::new(Expression::CString(
                "error".to_string(),
            ))))),
            Expression::CInt(3),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.0, "Propagate");
        assert_eq!(error.1, Some(Expression::CString("error".to_string())));
    }

    #[test]
    fn test_list_with_isnothing_expressions() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::IsNothing(Box::new(Expression::CNothing)),
            Expression::IsNothing(Box::new(Expression::CJust(Box::new(Expression::CInt(5))))),
            Expression::IsNothing(Box::new(Expression::CInt(10))),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CTrue,  // CNothing is nothing
            Expression::CFalse, // CJust(5) is not nothing
            Expression::CFalse, // CInt(10) is not nothing
        ]);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_list_with_iserror_expressions() {
        let env = create_test_env();
        let list_expr = Expression::ListValue(vec![
            Expression::IsError(Box::new(Expression::CErr(Box::new(Expression::CString(
                "error".to_string(),
            ))))),
            Expression::IsError(Box::new(Expression::COk(Box::new(Expression::CInt(5))))),
            Expression::IsError(Box::new(Expression::CInt(10))),
        ]);

        let result = eval(list_expr, &env);

        assert!(result.is_ok());
        let expected = Expression::ListValue(vec![
            Expression::CTrue,  // CErr is an error
            Expression::CFalse, // COk is not an error
            Expression::CFalse, // CInt is not an error
        ]);
        assert_eq!(result.unwrap(), expected);
    }
}
