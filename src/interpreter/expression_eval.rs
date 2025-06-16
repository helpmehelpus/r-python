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
