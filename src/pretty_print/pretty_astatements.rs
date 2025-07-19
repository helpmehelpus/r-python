// Trait e implementação de pretty printing para Statement

use crate::ir::ast::{Statement, Expression};

pub trait PrettyPrintStatement {
    fn pretty_print_stmt(&self, indent: usize) -> String;
}

impl PrettyPrintStatement for Statement {
    fn pretty_print_stmt(&self, indent: usize) -> String {
        let pad = "  ".repeat(indent);
        match self {
            Statement::Let { name, expr } => format!(
                "{}Let({},\n{}\n{})",
                pad,
                name,
                expr.pretty_print_expr(indent + 1),
                pad
            ),
            Statement::Assign { name, expr } => format!(
                "{}Assign({},\n{}\n{})",
                pad,
                name,
                expr.pretty_print_expr(indent + 1),
                pad
            ),
            Statement::Expr(expr) => format!(
                "{}Expr(\n{}\n{})",
                pad,
                expr.pretty_print_expr(indent + 1),
                pad
            ),
            Statement::Return(expr) => format!(
                "{}Return(\n{}\n{})",
                pad,
                expr.pretty_print_expr(indent + 1),
                pad
            ),
            Statement::If { cond, then_branch, else_branch } => {
                let then_str = then_branch.pretty_print_stmt(indent + 1);
                let else_str = else_branch
                    .as_ref()
                    .map(|e| e.pretty_print_stmt(indent + 1))
                    .unwrap_or_else(|| format!("{}None", "  ".repeat(indent + 1)));
                format!(
                    "{}If(\n{}Cond:\n{}\n{}Then:\n{}\n{}Else:\n{}\n{})",
                    pad,
                    "  ".repeat(indent + 1),
                    cond.pretty_print_expr(indent + 2),
                    "  ".repeat(indent + 1),
                    then_str,
                    "  ".repeat(indent + 1),
                    else_str,
                    pad
                )
            }
            Statement::While { cond, body } => format!(
                "{}While(\n{}Cond:\n{}\n{}Body:\n{}\n{})",
                pad,
                "  ".repeat(indent + 1),
                cond.pretty_print_expr(indent + 2),
                "  ".repeat(indent + 1),
                body.pretty_print_stmt(indent + 2),
                pad
            ),
            Statement::For { var, start, end, body } => format!(
                "{}For(\n{}Var: {}\n{}Start:\n{}\n{}End:\n{}\n{}Body:\n{}\n{})",
                pad,
                "  ".repeat(indent + 1),
                var,
                "  ".repeat(indent + 1),
                start.pretty_print_expr(indent + 2),
                "  ".repeat(indent + 1),
                end.pretty_print_expr(indent + 2),
                "  ".repeat(indent + 1),
                body.pretty_print_stmt(indent + 2),
                pad
            ),
            Statement::Block(stmts) => {
                let stmts_str = stmts
                    .iter()
                    .map(|s| s.pretty_print_stmt(indent + 1))
                    .collect::<Vec<_>>()
                    .join("\n");
                format!(
                    "{}Block([\n{}\n{}])",
                    pad, stmts_str, pad
                )
            }
            Statement::None => format!("{}None", pad),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{Statement, Expression};

    #[test]
    fn test_pretty_print_let() {
        let stmt = Statement::Let {
            name: "x".to_string(),
            expr: Box::new(Expression::CInt(5)),
        };
        let expected = "\
Let(x,
  5
)";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_assign() {
        let stmt = Statement::Assign {
            name: "y".to_string(),
            expr: Box::new(Expression::CInt(10)),
        };
        let expected = "\
Assign(y,
  10
)";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_expr_stmt() {
        let stmt = Statement::Expr(Box::new(Expression::CInt(7)));
        let expected = "\
Expr(
  7
)";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_return() {
        let stmt = Statement::Return(Box::new(Expression::CInt(99)));
        let expected = "\
Return(
  99
)";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_if_with_else() {
        let stmt = Statement::If {
            cond: Box::new(Expression::CTrue),
            then_branch: Box::new(Statement::Expr(Box::new(Expression::CInt(1)))),
            else_branch: Some(Box::new(Statement::Expr(Box::new(Expression::CInt(2)))),
            ),
        };
        let expected = "\
If(
  Cond:
    True
  Then:
    Expr(
      1
    )
  Else:
    Expr(
      2
    )
)";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_if_without_else() {
        let stmt = Statement::If {
            cond: Box::new(Expression::CFalse),
            then_branch: Box::new(Statement::Expr(Box::new(Expression::CInt(3)))),
            else_branch: None,
        };
        let expected = "\
If(
  Cond:
    False
  Then:
    Expr(
      3
    )
  Else:
    None
)";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_while() {
        let stmt = Statement::While {
            cond: Box::new(Expression::Var("c".to_string())),
            body: Box::new(Statement::Expr(Box::new(Expression::CInt(4)))),
        };
        let expected = "\
While(
  Cond:
    c
  Body:
    Expr(
      4
    )
)";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_for() {
        let stmt = Statement::For {
            var: "i".to_string(),
            start: Box::new(Expression::CInt(1)),
            end: Box::new(Expression::CInt(5)),
            body: Box::new(Statement::Expr(Box::new(Expression::CInt(99)))),
        };
        let expected = "\
For(
  Var: i
  Start:
    1
  End:
    5
  Body:
    Expr(
      99
    )
)";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_block() {
        let stmt = Statement::Block(vec![
            Statement::Let {
                name: "x".to_string(),
                expr: Box::new(Expression::CInt(5)),
            },
            Statement::Assign {
                name: "y".to_string(),
                expr: Box::new(Expression::CInt(10)),
            },
        ]);
        let expected = "\
Block([
  Let(x,
    5
  )
  Assign(y,
    10
  )
])";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_none() {
        let stmt = Statement::None;
        assert_eq!(stmt.pretty_print_stmt(0), "None");
    }
}