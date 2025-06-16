use super::expression_eval::eval;
use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Statement};

type ErrorMessage = (String, Option<Expression>);

pub fn _execute_with_env_(
    stmt: Statement,
    env: &mut Environment<Expression>,
) -> Result<Environment<Expression>, ErrorMessage> {
    execute(stmt, &env.clone())
}

pub fn run(
    stmt: Statement,
    env: &Environment<Expression>,
) -> Result<Environment<Expression>, String> {
    match execute(stmt, env) {
        Ok(e) => Ok(e),
        Err((s, _)) => Err(s),
    }
}

pub fn execute(
    stmt: Statement,
    env: &Environment<Expression>,
) -> Result<Environment<Expression>, ErrorMessage> {
    let mut new_env = env.clone();

    match stmt {
        Statement::Assignment(name, exp) => {
            let value = eval(*exp, &new_env)?;
            new_env.map_variable(name, true, value);
            Ok(new_env)
        }

        Statement::IfThenElse(cond, stmt_then, stmt_else) => {
            let value = eval(*cond, &new_env)?;

            match value {
                Expression::CTrue => match *stmt_then {
                    Statement::Block(stmts) => execute_block(stmts, &new_env),
                    _ => execute(*stmt_then, &new_env),
                },
                Expression::CFalse => match stmt_else {
                    Some(else_stmt) => match *else_stmt {
                        Statement::Block(stmts) => execute_block(stmts, &new_env),
                        _ => execute(*else_stmt, &new_env),
                    },
                    None => Ok(new_env),
                },
                _ => Err(("Condition must evaluate to a boolean".to_string(), None)),
            }
        }

        Statement::Block(stmts) => {
            new_env.push();
            let result = execute_block(stmts, &new_env);
            new_env.pop();
            result
        }

        Statement::While(cond, stmt) => {
            let mut value = eval(*cond.clone(), &new_env)?;

            loop {
                match value {
                    Expression::CTrue => {
                        new_env = execute(*stmt.clone(), &new_env)?;
                        value = eval(*cond.clone(), &new_env)?;
                    }
                    Expression::CFalse => return Ok(new_env),
                    _ => unreachable!(),
                }
            }
        }

        Statement::Sequence(s1, s2) => {
            new_env = execute(*s1, &new_env)?;
            execute(*s2, &new_env)
        }

        Statement::FuncDef(func) => {
            new_env.map_function(func.clone());
            Ok(new_env)
        }

        Statement::Return(exp) => {
            let exp_value = eval(*exp, &new_env)?;
            Err(("Return".to_string(), Some(exp_value)))
        }

        Statement::TypeDeclaration(name, constructors) => {
            new_env.map_adt(name, constructors);
            Ok(new_env)
        }

        _ => Err((String::from("not implemented yet"), None)),
    }
}

pub fn execute_block(
    stmts: Vec<Statement>,
    env: &Environment<Expression>,
) -> Result<Environment<Expression>, ErrorMessage> {
    let mut current_env = env.clone();

    for stmt in stmts {
        match execute(stmt, &current_env) {
            Ok(new_env) => current_env = new_env,
            Err(e) => return Err(e),
        }
    }
    Ok(current_env)
}
