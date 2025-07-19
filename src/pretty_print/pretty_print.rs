// Módulo central de pretty print: reexporta traits e funções dos submódulos

pub use crate::pretty_type::PrettyPrintType;
pub use crate::pretty_expressions::PrettyPrintExpression;
pub use crate::pretty_statements::PrettyPrintStatement;
pub use crate::pretty_functions::{PrettyPrintFunction, PrettyPrintFormalArgument};

// Se quiser, pode adicionar helpers globais aqui

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::*;
    use crate::pretty_type::PrettyPrintType;
    use crate::pretty_expressions::PrettyPrintExpression;
    use crate::pretty_statements::PrettyPrintStatement;
    use crate::pretty_functions::{PrettyPrintFunction, PrettyPrintFormalArgument};

    #[test]
    fn test_pretty_print_full_function() {
        let func = Function {
            name: "main".to_string(),
            args: vec![
                FormalArgument {
                    name: "a".to_string(),
                    typ: Type::TInteger,
                },
                FormalArgument {
                    name: "b".to_string(),
                    typ: Type::TBool,
                },
            ],
            return_type: Some(Type::TInteger),
            body: Statement::Block(vec![
                Statement::Let {
                    name: "x".to_string(),
                    expr: Box::new(Expression::CInt(42)),
                },
                Statement::Return(Box::new(Expression::Var("x".to_string()))),
            ]),
        };

        let pretty = func.pretty_print_func(0);
        let expected = "\
Function(
  Name: main
  Args: [
    FormalArg(a,   Int),
    FormalArg(b,   Bool)
  ]
  Return:   Int
  Body:
    Block([
      Let(x,
        42
      )
      Return(
        x
      )
    ])
)";
        assert_eq!(pretty, expected);
    }

    #[test]
    fn test_pretty_print_complex_expression() {
        let expr = Expression::Add(
            Box::new(Expression::CInt(2)),
            Box::new(Expression::Mul(
                Box::new(Expression::CInt(3)),
                Box::new(Expression::CInt(4)),
            )),
        );
        let expected = "\
Add(
  2
  Mul(
    3
    4
  )
)";
        assert_eq!(expr.pretty_print_expr(0), expected);
    }

    #[test]
    fn test_pretty_print_block_statement() {
        let stmt = Statement::Block(vec![
            Statement::Let {
                name: "foo".to_string(),
                expr: Box::new(Expression::CInt(10)),
            },
            Statement::Assign {
                name: "foo".to_string(),
                expr: Box::new(Expression::Add(
                    Box::new(Expression::Var("foo".to_string())),
                    Box::new(Expression::CInt(2)),
                )),
            },
        ]);
        let expected = "\
Block([
  Let(foo,
    10
  )
  Assign(foo,
    Add(
      foo
      2
    )
  )
])";
        assert_eq!(stmt.pretty_print_stmt(0), expected);
    }

    #[test]
    fn test_pretty_print_formal_argument() {
        let arg = FormalArgument {
            name: "n".to_string(),
            typ: Type::TInteger,
        };
        let expected = "  FormalArg(n,   Int)";
        assert_eq!(arg.pretty_print_formal_arg(1), expected);
    }
}