// src/pretty_print/pretty_expressions.rs

use std::rc::Rc;
use crate::ir::ast::Expression;

// Importamos o novo trait e os construtores de Doc do nosso motor principal.
use super::pretty_print::{
    group, nest, nil, text, ToDoc, Doc, concat, line
};

// --- Níveis de Precedência dos Operadores ---
const PREC_NONE: usize = 0;
const PREC_OR: usize = 1;
const PREC_AND: usize = 2;
const PREC_RELATIONAL: usize = 3;
const PREC_ADD_SUB: usize = 4;
const PREC_MUL_DIV: usize = 5;
const PREC_UNARY: usize = 6;
const PREC_CALL: usize = 7;

// --- Implementação do Trait ---

impl ToDoc for Expression {
    fn to_doc(&self) -> Rc<Doc> {
        self.to_doc_inner(PREC_NONE)
    }
}

impl Expression {
    fn to_doc_inner(&self, parent_precedence: usize) -> Rc<Doc> {
        let maybe_paren = |current_precedence, doc| {
            if current_precedence < parent_precedence {
                concat(text("("), concat(doc, text(")")))
            } else {
                doc
            }
        };
        
        let binary_op = |op, current_prec, lhs: &Expression, rhs: &Expression| {
            let doc = concat(
                lhs.to_doc_inner(current_prec),
                concat(text(op), rhs.to_doc_inner(current_prec)),
            );
            maybe_paren(current_prec, doc)
        };

        match self {
            // Literais e variáveis
            Expression::CTrue => text("True"),
            Expression::CFalse => text("False"),
            Expression::CInt(i) => text(i.to_string()),
            Expression::CReal(f) => text(f.to_string()),
            Expression::CString(s) => text(format!("\"{}\"", s)),
            Expression::CVoid => text("()"),
            Expression::Var(name) => text(name.clone()),

            // Operadores binários
            Expression::Or(l, r) => binary_op(" or ", PREC_OR, l, r),
            Expression::And(l, r) => binary_op(" and ", PREC_AND, l, r),
            Expression::EQ(l, r) => binary_op(" == ", PREC_RELATIONAL, l, r),
            Expression::NEQ(l, r) => binary_op(" != ", PREC_RELATIONAL, l, r),
            Expression::GT(l, r) => binary_op(" > ", PREC_RELATIONAL, l, r),
            Expression::LT(l, r) => binary_op(" < ", PREC_RELATIONAL, l, r),
            Expression::GTE(l, r) => binary_op(" >= ", PREC_RELATIONAL, l, r),
            Expression::LTE(l, r) => binary_op(" <= ", PREC_RELATIONAL, l, r),
            Expression::Add(l, r) => binary_op(" + ", PREC_ADD_SUB, l, r),
            Expression::Sub(l, r) => binary_op(" - ", PREC_ADD_SUB, l, r),
            Expression::Mul(l, r) => binary_op(" * ", PREC_MUL_DIV, l, r),
            Expression::Div(l, r) => binary_op(" / ", PREC_MUL_DIV, l, r),

            // Operadores unários
            Expression::Not(expr) => {
                let doc = concat(text("not "), expr.to_doc_inner(PREC_UNARY));
                maybe_paren(PREC_UNARY, doc)
            }
            Expression::Unwrap(expr) => {
                 concat(expr.to_doc_inner(PREC_CALL), text("!"))
            }

            // Estruturas complexas com listas
            Expression::FuncCall(name, args) => {
                let separator = concat(text(","), line()); // "," + possible newline
                let args_docs = args.iter().map(|arg| arg.to_doc_inner(PREC_NONE)).collect();
                
                concat(
                    text(name.clone()),
                    group(concat(
                        text("("),
                        concat(nest(2, join(separator, args_docs)), text(")")),
                    )),
                )
            }

            Expression::ListValue(elements) => {
                let separator = concat(text(","), line()); // "," + possible newline
                let elems_docs = elements.iter().map(|el| el.to_doc_inner(PREC_NONE)).collect();
                group(concat(
                    text("["),
                    concat(nest(2, join(separator, elems_docs)), text("]")),
                ))
            }

            Expression::Constructor(name, args) => {
                let args_docs = args.iter().map(|arg| arg.to_doc_inner(PREC_NONE)).collect();
                let doc = concat(text(name.clone()), concat(text(" "), join(text(" "), args_docs)));
                maybe_paren(PREC_CALL, doc.clone())
            }

            // Tratamento de Erro
            Expression::COk(expr) => concat(text("Ok("), concat(expr.to_doc(), text(")"))),
            Expression::CErr(expr) => concat(text("Err("), concat(expr.to_doc(), text(")"))),
            Expression::CJust(expr) => concat(text("Just("), concat(expr.to_doc(), text(")"))),
            Expression::CNothing => text("Nothing"),
            
            _ => text("/* expr not implemented */"),
        }
    }
}

// --- Funções Auxiliares ---
fn join(sep: Rc<Doc>, docs: Vec<Rc<Doc>>) -> Rc<Doc> {
    docs.into_iter().reduce(|acc, doc| concat(acc, concat(sep.clone(), doc))).unwrap_or_else(nil)
}

// --- Testes Robustos ---

#[cfg(test)]
mod tests {
    use crate::ir::ast::Expression;
    // CORREÇÃO: Importa tudo o que o `mod.rs` (super) exporta.
    use super::*;
    use crate::pretty_print::pretty;

    #[test]
    fn test_precedence_add_mul() {
        let expr = Expression::Mul(
            Box::new(Expression::Add(
                Box::new(Expression::Var("a".to_string())),
                Box::new(Expression::Var("b".to_string())),
            )),
            Box::new(Expression::Var("c".to_string())),
        );
        assert_eq!(pretty(80, &expr.to_doc()), "(a + b) * c");
    }
    
    #[test]
    fn test_precedence_relational_and() {
        let expr = Expression::And(
            Box::new(Expression::GT(
                Box::new(Expression::Var("a".to_string())),
                Box::new(Expression::Var("b".to_string())),
            )),
            Box::new(Expression::LT(
                Box::new(Expression::Var("c".to_string())),
                Box::new(Expression::Var("d".to_string())),
            )),
        );
        assert_eq!(pretty(80, &expr.to_doc()), "a > b and c < d");
    }

    #[test]
    fn test_list_layout() {
        let list = Expression::ListValue(vec![
            Expression::CString("um item longo".to_string()),
            Expression::CString("outro item bem longo".to_string()),
            Expression::CString("o ultimo item super longo".to_string()),
        ]);
        let doc = list.to_doc();

        // Com espaço, fica em uma linha
        let wide_expected = "[\"um item longo\", \"outro item bem longo\", \"o ultimo item super longo\"]";
        assert_eq!(pretty(100, &doc), wide_expected);

        // Com pouco espaço, quebra a linha
        let narrow_expected = "\
[
  \"um item longo\",
  \"outro item bem longo\",
  \"o ultimo item super longo\"
]";
        assert_eq!(pretty(40, &doc), narrow_expected);
    }
}