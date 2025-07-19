// Trait e implementação de pretty printing para Function

use crate::ir::ast::{Function, FormalArgument, Statement};

pub trait PrettyPrintFunction {
    fn pretty_print_func(&self, indent: usize) -> String;
}

impl PrettyPrintFunction for Function {
    fn pretty_print_func(&self, indent: usize) -> String {
        let pad = "  ".repeat(indent);
        let args_str = self
            .args
            .iter()
            .map(|arg| arg.pretty_print_formal_arg(indent + 1))
            .collect::<Vec<_>>()
            .join(",\n");
        let body_str = self.body.pretty_print_stmt(indent + 1);
        let ret_str = match &self.return_type {
            Some(rt) => format!("{}", rt.pretty_print_type(indent + 1)),
            None => format!("{}None", "  ".repeat(indent + 1)),
        };
        format!(
            "{}Function(\n{}Name: {}\n{}Args: [\n{}\n{}]\n{}Return: {}\n{}Body:\n{}\n{})",
            pad,
            "  ".repeat(indent + 1),
            self.name,
            "  ".repeat(indent + 1),
            args_str,
            "  ".repeat(indent + 1),
            "  ".repeat(indent + 1),
            ret_str,
            "  ".repeat(indent + 1),
            body_str,
            pad
        )
    }
}

pub trait PrettyPrintFormalArgument {
    fn pretty_print_formal_arg(&self, indent: usize) -> String;
}

impl PrettyPrintFormalArgument for FormalArgument {
    fn pretty_print_formal_arg(&self, indent: usize) -> String {
        let pad = "  ".repeat(indent);
        format!(
            "{}FormalArg({}, {})",
            pad,
            self.name,
            self.typ.pretty_print_type(indent + 1)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{Function, FormalArgument, Type, Statement, Expression};

    #[test]
    fn test_pretty_print_function_simple() {
        let func = Function {
            name: "add".to_string(),
            args: vec![
                FormalArgument {
                    name: "x".to_string(),
                    typ: Type::TInteger,
                },
                FormalArgument {
                    name: "y".to_string(),
                    typ: Type::TInteger,
                },
            ],
            return_type: Some(Type::TInteger),
            body: Statement::Return(Box::new(Expression::Add(
                Box::new(Expression::Var("x".to_string())),
                Box::new(Expression::Var("y".to_string())),
            ))),
        };
        let expected = "\
Function(
  Name: add
  Args: [
    FormalArg(x,   Int),
    FormalArg(y,   Int)
  ]
  Return:   Int
  Body:
    Return(
      Add(
        x
        y
      )
    )
)";
        assert_eq!(func.pretty_print_func(0), expected);
    }

    #[test]
    fn test_pretty_print_function_with_none_return() {
        let func = Function {
            name: "log".to_string(),
            args: vec![
                FormalArgument {
                    name: "msg".to_string(),
                    typ: Type::TString,
                },
            ],
            return_type: None,
            body: Statement::Expr(Box::new(Expression::Var("msg".to_string()))),
        };
        let expected = "\
Function(
  Name: log
  Args: [
    FormalArg(msg,   String)
  ]
  Return:   None
  Body:
    Expr(
      msg
    )
)";
        assert_eq!(func.pretty_print_func(0), expected);
    }

    #[test]
    fn test_pretty_print_formal_argument() {
        let arg = FormalArgument {
            name: "v".to_string(),
            typ: Type::TBool,
        };
        let expected = "  FormalArg(v,   Bool)";
        assert_eq!(arg.pretty_print_formal_arg(1), expected);
    }
}