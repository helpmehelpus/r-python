// Trait e implementação de pretty printing para Expression

use crate::ir::ast::Expression;

pub trait PrettyPrintExpression {
    fn pretty_print_expr(&self, indent: usize) -> String;
}

impl PrettyPrintExpression for Expression {
    fn pretty_print_expr(&self, indent: usize) -> String {
        let pad = "  ".repeat(indent);
        match self {
            Expression::CInt(i) => format!("{}{}", pad, i),
            Expression::CReal(f) => format!("{}{:.3}", pad, f),
            Expression::CString(s) => format!("{}\"{}\"", pad, s),
            Expression::CTrue => format!("{}True", pad),
            Expression::CFalse => format!("{}False", pad),
            Expression::Var(name) => format!("{}{}", pad, name),
            Expression::Add(lhs, rhs) => format!(
                "{}Add(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::Sub(lhs, rhs) => format!(
                "{}Sub(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::Mul(lhs, rhs) => format!(
                "{}Mul(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::Div(lhs, rhs) => format!(
                "{}Div(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::Or(lhs, rhs) => format!(
                "{}Or(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::And(lhs, rhs) => format!(
                "{}And(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::Not(inner) => format!(
                "{}Not(\n{}\n{})",
                pad,
                inner.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::EQ(lhs, rhs) => format!(
                "{}EQ(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::NEQ(lhs, rhs) => format!(
                "{}NEQ(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::LT(lhs, rhs) => format!(
                "{}LT(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::LTE(lhs, rhs) => format!(
                "{}LTE(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::GT(lhs, rhs) => format!(
                "{}GT(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::GTE(lhs, rhs) => format!(
                "{}GTE(\n{}\n{}\n{})",
                pad,
                lhs.pretty_print_expr(indent + 1),
                rhs.pretty_print_expr(indent + 1),
                pad
            ),
            Expression::FuncCall(name, args) => {
                let args_str = args
                    .iter()
                    .map(|a| a.pretty_print_expr(indent + 1))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!(
                    "{}FuncCall({}, [\n{}\n{}])",
                    pad, name, args_str, pad
                )
            }
            Expression::Constructor(name, args) => {
                let args_str = args
                    .iter()
                    .map(|a| a.pretty_print_expr(indent + 1))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!(
                    "{}Constructor({}, [\n{}\n{}])",
                    pad, name, args_str, pad
                )
            }
            Expression::ListValue(values) => {
                let vals_str = values
                    .iter()
                    .map(|v| v.pretty_print_expr(indent + 1))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!(
                    "{}List([\n{}\n{}])",
                    pad, vals_str, pad
                )
            }
            Expression::None => format!("{}None", pad),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::Expression;

    #[test]
    fn test_pretty_print_int() {
        let expr = Expression::CInt(42);
        assert_eq!(expr.pretty_print_expr(0), "42");
    }

    #[test]
    fn test_pretty_print_real() {
        let expr = Expression::CReal(3.1415);
        assert_eq!(expr.pretty_print_expr(0), "3.142"); // arredondamento de 3 casas
    }

    #[test]
    fn test_pretty_print_string() {
        let expr = Expression::CString("hello".to_string());
        assert_eq!(expr.pretty_print_expr(0), "\"hello\"");
    }

    #[test]
    fn test_pretty_print_bool_true() {
        let expr = Expression::CTrue;
        assert_eq!(expr.pretty_print_expr(0), "True");
    }

    #[test]
    fn test_pretty_print_bool_false() {
        let expr = Expression::CFalse;
        assert_eq!(expr.pretty_print_expr(0), "False");
    }

    #[test]
    fn test_pretty_print_var() {
        let expr = Expression::Var("x".to_string());
        assert_eq!(expr.pretty_print_expr(0), "x");
    }

    #[test]
    fn test_pretty_print_add() {
        let expr = Expression::Add(
            Box::new(Expression::CInt(1)),
            Box::new(Expression::CInt(2)),
        );
        let expected = "\
Add(
  1
  2
)";
        assert_eq!(expr.pretty_print_expr(0), expected);
    }

    #[test]
    fn test_pretty_print_func_call() {
        let expr = Expression::FuncCall(
            "foo".to_string(),
            vec![
                Expression::CInt(10),
                Expression::CReal(2.5),
                Expression::CTrue,
            ],
        );
        let expected = "\
FuncCall(foo, [
  10,
  2.500,
  True
])";
        assert_eq!(expr.pretty_print_expr(0), expected);
    }

    #[test]
    fn test_pretty_print_constructor() {
        let expr = Expression::Constructor(
            "Some".to_string(),
            vec![
                Box::new(Expression::CInt(42)),
            ],
        );
        let expected = "\
Constructor(Some, [
  42
])";
        assert_eq!(expr.pretty_print_expr(0), expected);
    }

    #[test]
    fn test_pretty_print_list_value() {
        let expr = Expression::ListValue(vec![
            Expression::CInt(1),
            Expression::CInt(2),
            Expression::CInt(3),
        ]);
        let expected = "\
List([
  1,
  2,
  3
])";
        assert_eq!(expr.pretty_print_expr(0), expected);
    }

    #[test]
    fn test_pretty_print_none() {
        let expr = Expression::None;
        assert_eq!(expr.pretty_print_expr(0), "None");
    }
}