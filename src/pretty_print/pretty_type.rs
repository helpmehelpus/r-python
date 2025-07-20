// src/pretty_print/pretty_type.rs

use std::rc::Rc;
use crate::ir::ast::{Type, ValueConstructor};

use super::pretty_print::{
    group, line, nest, text, ToDoc, Doc, nil, concat, hardline
};

fn join(sep: Rc<Doc>, docs: Vec<Rc<Doc>>) -> Rc<Doc> {
    docs.into_iter().reduce(|acc, doc| concat(acc, concat(sep.clone(), doc))).unwrap_or_else(nil)
}

impl ToDoc for Type {
    fn to_doc(&self) -> Rc<Doc> {
        match self {
            Type::TInteger => text("Int"),
            Type::TBool => text("Boolean"),
            Type::TReal => text("Real"),
            Type::TString => text("String"),
            Type::TVoid => text("Unit"),
            Type::TAny => text("Any"),

            Type::TList(t) => concat(text("["), concat(t.to_doc(), text("]"))),

            Type::TTuple(types) => {
                let inner_docs = types.iter().map(|t| t.to_doc()).collect();
                concat(text("("), concat(join(text(", "), inner_docs), text(")")))
            },

            Type::TMaybe(t) => concat(text("Maybe["), concat(t.to_doc(), text("]"))),

            Type::TResult(ok, err) => {
                concat(text("Result["), concat(ok.to_doc(), concat(text(", "), concat(err.to_doc(), text("]")))))
            }

            Type::TFunction(ret, params) => {
                let params_docs = params.iter().map(|p| p.to_doc()).collect();
                let separator = concat(text(","), line()); // Separador: vírgula e quebra de linha suave

                let params_doc = group(concat(
                    text("("),
                    concat(
                        nest(4, concat(line(), join(separator, params_docs))),
                        concat(line(), text(")"))
                    )
                ));
                
                let ret_doc = match ret.as_ref() {
                    Some(rt) => rt.to_doc(),
                    None => text("Unit"),
                };

                concat(params_doc, concat(text(" -> "), ret_doc))
            }

            Type::TAlgebraicData(name, constructors) => {
                let ctors_docs = constructors.iter().map(|c| c.to_doc()).collect();
                concat(
                    text("data "),
                    concat(
                        text(name.clone()),
                        concat(
                            text(":"),
                            concat(
                                nest(4, concat(hardline(), join(hardline(), ctors_docs))),
                                concat(hardline(), text("end"))
                            )
                        )
                    )
                )
            }
        }
    }
}

impl ToDoc for ValueConstructor {
    fn to_doc(&self) -> Rc<Doc> {
        let name_doc = concat(text("| "), text(self.name.clone()));
        if self.types.is_empty() {
            return name_doc;
        }

        let types_docs: Vec<Rc<Doc>> = self.types.iter().map(|t| t.to_doc()).collect();
        concat(name_doc, concat(text(" "), join(text(" "), types_docs)))
    }
}


#[cfg(test)]
mod tests {
    use crate::ir::ast::{Type, ValueConstructor};
    use super::*;
    use crate::pretty_print::pretty;

    #[test]
    fn test_basic_type_doc() {
        assert_eq!(pretty(80, &Type::TInteger.to_doc()), "Int");
        assert_eq!(pretty(80, &Type::TBool.to_doc()), "Boolean");
    }

    #[test]
    fn test_list_and_tuple_doc() {
        let list_type = Type::TList(Box::new(Type::TList(Box::new(Type::TString))));
        assert_eq!(pretty(80, &list_type.to_doc()), "[[String]]");

        let tuple_type = Type::TTuple(vec![Type::TInteger, Type::TReal]);
        assert_eq!(pretty(80, &tuple_type.to_doc()), "(Int, Real)");
    }
    
    #[test]
    fn test_function_type_layout() {
        let func_type = Type::TFunction(
            Box::new(Some(Type::TString)),
            vec![Type::TInteger, Type::TList(Box::new(Type::TBool)), Type::TReal],
        );
        let doc = func_type.to_doc();

        let expected_wide = "( Int, [Boolean], Real ) -> String";
        assert_eq!(pretty(80, &doc), expected_wide);
        
        // CORREÇÃO: O `expected` agora bate com a nova lógica de `group`
        let expected_narrow = "(\n    Int,\n    [Boolean],\n    Real\n) -> String";
        assert_eq!(pretty(20, &doc), expected_narrow);
    }

    #[test]
    fn test_adt_layout() {
        let adt = Type::TAlgebraicData(
            "MyList".to_string(),
            vec![
                ValueConstructor::new("Cons".to_string(), vec![Type::TInteger, Type::TList(Box::new(Type::TInteger))]),
                ValueConstructor::new("Nil".to_string(), vec![]),
            ],
        );
        let doc = adt.to_doc();

        // Corrigido para a indentação correta
        let expected = "data MyList:\n    | Cons Int [Int]\n    | Nil\nend";
        assert_eq!(pretty(80, &doc), expected);
    }
}