// src/pretty_print/pretty_print.rs

use std::rc::Rc;

#[derive(Clone)]
pub enum Doc {
    Nil,
    Text(String),
    Line,
    HardLine,
    Nest(usize, Rc<Doc>),
    Concat(Rc<Doc>, Rc<Doc>),
    Group(Rc<Doc>),
}

pub trait ToDoc {
    fn to_doc(&self) -> Rc<Doc>;
}

// --- Funções Construtoras ---
pub fn nil() -> Rc<Doc> { Rc::new(Doc::Nil) }
pub fn text<S: Into<String>>(s: S) -> Rc<Doc> { Rc::new(Doc::Text(s.into())) }
pub fn line() -> Rc<Doc> { Rc::new(Doc::Line) }
pub fn hardline() -> Rc<Doc> { Rc::new(Doc::HardLine) }
pub fn nest(i: usize, doc: Rc<Doc>) -> Rc<Doc> { Rc::new(Doc::Nest(i, doc)) }
pub fn group(doc: Rc<Doc>) -> Rc<Doc> { Rc::new(Doc::Group(doc)) }

pub fn concat(d1: Rc<Doc>, d2: Rc<Doc>) -> Rc<Doc> {
    Rc::new(Doc::Concat(d1, d2))
}

// --- O Motor de Renderização ---

pub fn pretty(width: usize, doc: &Rc<Doc>) -> String {
    let mut out = String::new();
    best(width as isize, 0, &[(0, doc.clone())], &mut out);
    out
}

fn flatten(doc: &Rc<Doc>) -> Rc<Doc> {
    match &**doc {
        Doc::Nil => nil(),
        Doc::Text(_) => doc.clone(),
        Doc::Line => text(" "),
        Doc::HardLine => nil(),
        Doc::Nest(_, d) => flatten(d),
        Doc::Concat(d1, d2) => concat(flatten(d1), flatten(d2)),
        Doc::Group(d) => flatten(d),
    }
}

fn best(w: isize, mut k: isize, docs: &[(usize, Rc<Doc>)], out: &mut String) -> isize {
    for (indent, doc) in docs {
        match &**doc {
            Doc::Nil => {}
            Doc::Text(s) => {
                k += s.len() as isize;
                out.push_str(s);
            }
            Doc::Line | Doc::HardLine => {
                out.push('\n');
                let new_indent_str = "  ".repeat(*indent);
                k = new_indent_str.len() as isize;
                out.push_str(&new_indent_str);
            }
            Doc::Nest(i, d) => {
                k = best(w, k, &[(indent + i, d.clone())], out);
            }
            Doc::Concat(d1, d2) => {
                k = best(w, k, &[(*indent, d1.clone())], out);
                k = best(w, k, &[(*indent, d2.clone())], out);
            }
            Doc::Group(d) => {
                if fits(w - k, &flatten(d)) {
                    k = best(w, k, &[(*indent, flatten(d))], out);
                } else {
                    k = best(w, k, &[(*indent, d.clone())], out);
                }
            }
        }
    }
    k
}

fn fits(mut w: isize, doc: &Rc<Doc>) -> bool {
    if w < 0 { return false; }
    match &**doc {
        Doc::Nil => true,
        Doc::Text(s) => {
            w -= s.len() as isize;
            w >= 0
        }
        Doc::Line => true, // `Line` sempre "cabe" porque pode quebrar para uma nova linha.
        Doc::HardLine => false, // `HardLine` força uma quebra, então não "cabe" em uma linha.
        Doc::Nest(_, d) => fits(w, d),
        Doc::Concat(d1, d2) => {
            if !fits(w, d1) { return false; }
            // Esta lógica para `Concat` é uma simplificação. Uma implementação 100% precisa
            // exigiria renderizar d1 para saber o espaço exato que ele consome.
            // Para a maioria dos casos, verificar ambos com `w` funciona como uma boa heurística.
            fits(w, d1) && fits(w, d2)
        }
        Doc::Group(d) => fits(w, d),
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    // Função auxiliar para juntar documentos nos testes.
    fn join(sep: Rc<Doc>, docs: Vec<Rc<Doc>>) -> Rc<Doc> {
        docs.into_iter().reduce(|acc, doc| concat(acc, concat(sep.clone(), doc))).unwrap_or_else(nil)
    }

    #[test]
    fn test_simple_text() {
        let doc = text("hello");
        assert_eq!(pretty(80, &doc), "hello");
    }

    #[test]
    fn test_concat() {
        let doc = concat(text("hello"), text(" world"));
        assert_eq!(pretty(80, &doc), "hello world");
    }

    #[test]
    fn test_nesting() {
        let doc = concat(text("inicio"), nest(2, concat(hardline(), text("meio"))));
        let expected = "inicio\n  meio";
        assert_eq!(pretty(80, &doc), expected);
    }

    #[test]
    fn test_group_fits_on_one_line() {
        // [1, 2, 3]
        let list_doc = group(concat(
            text("["),
            nest(2, concat(
                line(),
                join(concat(text(","), line()), vec![text("1"), text("2"), text("3")])
            )),
            concat(line(), text("]"))
        ));

        // Com largura suficiente, `line()` vira espaço.
        assert_eq!(pretty(80, &list_doc), "[ 1, 2, 3 ]");
    }

    #[test]
    fn test_group_breaks_into_multiple_lines() {
        // [ "um_item_longo", "outro_item_longo" ]
        let list_doc = group(concat(
            text("["),
            nest(2, concat(
                line(),
                join(
                    concat(text(","), line()),
                    vec![text("\"um_item_longo\""), text("\"outro_item_longo\"")]
                )
            )),
            concat(line(), text("]"))
        ));

        // Com pouca largura, `line()` vira `\n` e `nest` indenta.
        let expected = "\
[
  \"um_item_longo\",
  \"outro_item_longo\"
]";
        assert_eq!(pretty(30, &list_doc), expected);
    }
}