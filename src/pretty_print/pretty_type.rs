// Trait e implementação de pretty printing para Type

use crate::ir::ast::{Type, ValueConstructor};

pub trait PrettyPrintType {
    fn pretty_print_type(&self, indent: usize) -> String;
}

impl PrettyPrintType for Type {
    fn pretty_print_type(&self, indent: usize) -> String {
        let pad = "  ".repeat(indent);
        match self {
            Type::TInteger => format!("{}Int", pad),
            Type::TBool => format!("{}Bool", pad),
            Type::TString => format!("{}String", pad),
            Type::TList(t) => format!(
                "{}List(\n{}\n{})",
                pad,
                t.pretty_print_type(indent + 1),
                pad
            ),
            Type::TFunction(ret, params) => {
                let params_str = params
                    .iter()
                    .map(|p| format!("{}", p.pretty_print_type(indent + 2)))
                    .collect::<Vec<_>>()
                    .join(",\n");
                let ret_str = if let Some(rt) = ret.as_ref() {
                    rt.pretty_print_type(indent + 2)
                } else {
                    format!("{}None", pad)
                };
                format!(
                    "{}Function(\n{}Return:   {}\n{}Params: [\n{}\n{}]\n{})",
                    pad,
                    "  ".repeat(indent + 1),
                    ret_str,
                    "  ".repeat(indent + 1),
                    params_str,
                    "  ".repeat(indent + 1),
                    pad
                )
            }
            Type::TAlgebraicData(name, constructors) => {
                let ctors_str = constructors
                    .iter()
                    .map(|c| format!("{}", c.pretty_print_value_constructor(indent + 1)))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!(
                    "{}AlgebraicData({}, [\n{}\n{}])",
                    pad, name, ctors_str, pad
                )
            }
            Type::TCustom(name) => format!("{}Custom({})", pad, name),
            Type::TNone => format!("{}None", pad),
        }
    }
}

pub trait PrettyPrintValueConstructor {
    fn pretty_print_value_constructor(&self, indent: usize) -> String;
}

impl PrettyPrintValueConstructor for ValueConstructor {
    fn pretty_print_value_constructor(&self, indent: usize) -> String {
        let pad = "  ".repeat(indent);
        let types_str = self
            .types
            .iter()
            .map(|t| t.pretty_print_type(indent + 2))
            .collect::<Vec<_>>()
            .join(", ");
        format!(
            "{}Constructor({}, [{}])",
            pad, self.name, types_str
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::Type;
    use crate::ir::ast::ValueConstructor;

    #[test]
    fn test_pretty_print_integer() {
        let typ = Type::TInteger;
        assert_eq!(typ.pretty_print_type(0), "Int");
    }

    #[test]
    fn test_pretty_print_function_type() {
        let typ = Type::TFunction(
            Box::new(Some(Type::TInteger)),
            vec![Type::TBool, Type::TList(Box::new(Type::TString))],
        );
        let expected = "\
Function(
  Return:   Int
  Params: [
    Bool,
    List(
      String
    )
  ]
)";
        assert_eq!(typ.pretty_print_type(0), expected);
    }

    #[test]
    fn test_pretty_print_adt_type() {
        let typ = Type::TAlgebraicData(
            "Option".to_string(),
            vec![
                ValueConstructor { name: "Some".to_string(), types: vec![Type::TInteger] },
                ValueConstructor { name: "None".to_string(), types: vec![] },
            ]
        );
        let expected = "\
AlgebraicData(Option, [
  Constructor(Some, [    Int]),
  Constructor(None, [])
])";
        assert_eq!(typ.pretty_print_type(0), expected);
    }

    #[test]
    fn test_pretty_print_custom_type() {
        let typ = Type::TCustom("MyType".to_string());
        assert_eq!(typ.pretty_print_type(0), "Custom(MyType)");
    }

    #[test]
    fn test_pretty_print_none_type() {
        let typ = Type::TNone;
        assert_eq!(typ.pretty_print_type(0), "None");
    }

    #[test]
    fn test_pretty_print_value_constructor() {
        let vc = ValueConstructor {
            name: "Node".to_string(),
            types: vec![Type::TInteger, Type::TList(Box::new(Type::TInteger))],
        };
        let expected = "  Constructor(Node, [    Int, List(\n      Int\n    )])";
        assert_eq!(vc.pretty_print_value_constructor(1), expected);
    }
}