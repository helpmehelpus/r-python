// src/pretty_print/pretty_statements.rs

use std::rc::Rc;
use crate::ir::ast::{FormalArgument, Function, Statement};

// CORREÇÃO: Removido o lifetime de todas as importações e usos.
use super::pretty_print::{
    group, hardline, line, nest, nil, text, ToDoc, Doc, concat
};

// CORREÇÃO: A função `join` agora opera com `Rc<Doc>` sem lifetimes.
fn join(sep: Rc<Doc>, docs: Vec<Rc<Doc>>) -> Rc<Doc> {
    // CORREÇÃO: Usa `concat` em vez de `+`.
    docs.into_iter().reduce(|acc, doc| concat(acc, concat(sep.clone(), doc))).unwrap_or_else(nil)
}

// CORREÇÃO: `impl ToDoc` sem lifetime.
impl ToDoc for Statement {
    // CORREÇÃO: Retorna `Rc<Doc>`.
    fn to_doc(&self) -> Rc<Doc> {
        match self {
            // CORREÇÃO: Todas as concatenações usam a função `concat()`.
            Statement::VarDeclaration(name, expr) => {
                concat(text("var "), concat(text(name.clone()), concat(text(" = "), concat(expr.to_doc(), text(";")))))
            }
            Statement::ValDeclaration(name, expr) => {
                concat(text("val "), concat(text(name.clone()), concat(text(" = "), concat(expr.to_doc(), text(";")))))
            }
            Statement::Assignment(name, expr) => {
                concat(text(name.clone()), concat(text(" = "), concat(expr.to_doc(), text(";"))))
            }
            Statement::Return(expr) => {
                concat(text("return "), concat(expr.to_doc(), text(";")))
            }
            Statement::Assert(check, msg) => {
                concat(text("assert("), concat(check.to_doc(), concat(text(", "), concat(msg.to_doc(), text(");")))))
            }

            Statement::Block(stmts) => {
                let stmts_doc = stmts.iter().map(|s| s.to_doc()).collect();
                concat(
                    text(":"),
                    concat(
                        nest(2, concat(hardline(), join(hardline(), stmts_doc))),
                        concat(hardline(), text("end"))
                    )
                )
            }
            Statement::IfThenElse(cond, then_branch, else_branch) => {
                let mut doc = concat(text("if "), concat(cond.to_doc(), concat(text(" "), then_branch.to_doc())));
                if let Some(else_b) = else_branch {
                    doc = concat(doc, concat(text(" else "), else_b.to_doc()));
                }
                doc
            }
            Statement::While(cond, body) => {
                concat(text("while "), concat(cond.to_doc(), concat(text(" "), body.to_doc())))
            }
            Statement::For(var, iterable, body) => {
                concat(text("for "), concat(text(var.clone()), concat(text(" in "), concat(iterable.to_doc(), concat(text(" "), body.to_doc())))))
            }
            Statement::FuncDef(func) => func.to_doc(),
            
            _ => text("// Statement não implementado"),
        }
    }
}

// CORREÇÃO: `impl ToDoc` sem lifetime.
impl ToDoc for Function {
    fn to_doc(&self) -> Rc<Doc> {
        let params_docs: Vec<Rc<Doc>> = self.params.iter().map(|p| p.to_doc()).collect();
        let params_doc = group(
            concat(text("("), concat(nest(2, concat(line(), join(concat(text(","), line()), params_docs))), concat(line(), text(")"))))
        );

        let body_doc = self.body.as_ref().map_or_else(
            || concat(text(":"), text(" end")),
            |b| b.to_doc(),
        );

        concat(
            text("def "),
            concat(
                text(self.name.clone()),
                concat(
                    params_doc,
                    concat(
                        text(" -> "),
                        concat(
                            self.kind.to_doc(),
                            concat(text(" "), body_doc)
                        )
                    )
                )
            )
        )
    }
}

// CORREÇÃO: `impl ToDoc` sem lifetime.
impl ToDoc for FormalArgument {
    fn to_doc(&self) -> Rc<Doc> {
        concat(text(self.argument_name.clone()), concat(text(": "), self.argument_type.to_doc()))
    }
}

// O bloco de testes permanece conceitualmente o mesmo, mas agora compila com a nova arquitetura.

#[cfg(test)]
mod tests {
    // CORREÇÃO: Removida a importação de `Type` que não era usada aqui.
    use crate::ir::ast::{Expression, Statement};
    // CORREÇÃO: Importa tudo o que o `mod.rs` (super) exporta.
    use super::*;
    use crate::pretty_print::pretty;

    // CORREÇÃO: Removidos os stubs `impl ToDoc for Expression` e `impl ToDoc for Type`.

    #[test]
    fn test_block_formatting() {
        let block = Statement::Block(vec![
            Statement::VarDeclaration("x".to_string(), Box::new(Expression::CInt(1))),
            Statement::Assignment("y".to_string(), Box::new(Expression::CInt(2))),
        ]);

        let expected = "\
:
  var x = 1;
  y = 2;
end";
        // Os testes agora usarão as implementações REAIS de ToDoc para Expression e Type.
        assert_eq!(pretty(80, &block.to_doc()), expected);
    }
    
    #[test]
    fn test_nested_structure_formatting() {
        let while_stmt = Statement::While(
            Box::new(Expression::Var("cond".to_string())),
            Box::new(Statement::Block(vec![
                Statement::IfThenElse(
                    Box::new(Expression::Var("a".to_string())),
                    Box::new(Statement::Block(vec![
                        Statement::Assignment("x".to_string(), Box::new(Expression::CInt(1)))
                    ])),
                    None
                )
            ]))
        );
        
        let expected_while = "\
while [expr:Var(\"cond\")] :
  if [expr:Var(\"a\")] :
    x = [expr:CInt(1)];
  end
end";
        assert_eq!(pretty(80, &while_stmt.to_doc()), expected_while);
    }
}
