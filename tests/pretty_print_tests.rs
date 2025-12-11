// tests/pretty_print_tests.rs (UNIFICADO)
//
// Este arquivo consolida todos os testes relacionados ao pretty printer:
// 1. Testes do núcleo algorítmico (Doc algebra, group, nest, hardline, etc.).
// 2. Testes de limites / widths mínimos.
// 3. Testes de integração com a AST (expressões, statements via parser).
// 4. Espaço reservado para futuros testes de Fase 6 (fill, comments, timing, adaptive punctuate).
//
// As seções são separadas por blocos de comentários grandes para facilitar navegação.

use r_python::ir::ast::{Expression, Statement};
use r_python::parser::parse;
use r_python::pretty_print::*;
use std::rc::Rc;

// ------------------------------------------------------------
// Helpers comuns usados por múltiplos grupos de testes
// ------------------------------------------------------------

/// Concatena uma sequência de docs intercalando o separador (forma linear simples).
fn join_linear(sep: Rc<Doc>, docs: Vec<Rc<Doc>>) -> Rc<Doc> {
    docs.into_iter()
        .reduce(|acc, d| concat(acc, concat(sep.clone(), d)))
        .unwrap_or_else(nil)
}

/// Converte um programa (lista de statements) em Doc, separando com linha em branco.
fn program_to_doc(program_ast: &[Statement]) -> Rc<Doc> {
    if program_ast.is_empty() {
        return nil();
    }
    program_ast
        .iter()
        .map(|s| s.to_doc())
        .reduce(|acc, doc| concat(acc, concat(hardline(), concat(hardline(), doc))))
        .unwrap_or_else(nil)
}

// ============================================================
// (1) TESTES DO NÚCLEO ALGORÍTMICO DO PRETTY PRINTER
// ============================================================

#[test]
fn test_nesting() {
    let doc = concat(text("inicio"), nest(4, concat(hardline(), text("meio"))));
    let expected = "inicio\n    meio";
    assert_eq!(pretty(80, &doc), expected);
}

#[test]
fn test_group_fits_on_one_line() {
    let list_doc = group(concat(
        text("["),
        concat(
            nest(
                4,
                concat(
                    line(),
                    join_linear(
                        concat(text(","), line()),
                        vec![text("1"), text("2"), text("3")],
                    ),
                ),
            ),
            concat(line(), text("]")),
        ),
    ));
    assert_eq!(pretty(80, &list_doc), "[ 1, 2, 3 ]");
}

#[test]
fn test_group_breaks_into_multiple_lines() {
    let list_doc = group(concat(
        text("["),
        concat(
            nest(
                4,
                concat(
                    line(),
                    join_linear(
                        concat(text(","), line()),
                        vec![text("\"item1\""), text("\"item2\"")],
                    ),
                ),
            ),
            concat(line(), text("]")),
        ),
    ));
    let expected = "[\n    \"item1\",\n    \"item2\"\n]";
    assert_eq!(pretty(10, &list_doc), expected);
}

#[test]
fn test_group_with_hardline_never_flattens() {
    let doc = group(concat(text("a"), concat(hardline(), text("b"))));
    assert_eq!(pretty(100, &doc), "a\nb");
}

// ============================================================
// (2) EDGE / WIDTH TESTS
// ============================================================

#[test]
fn test_list_min_widths() {
    let expr = Expression::ListValue(vec![
        Expression::CInt(1),
        Expression::CInt(2),
        Expression::CInt(3),
    ]);
    let doc = expr.to_doc();
    let w1 = pretty(1, &doc); // força quebras máximas
    assert!(w1.starts_with("[") && w1.contains("\n"));
    let w2 = pretty(2, &doc);
    assert!(w2.contains("\n"));
    let wide = pretty(80, &doc);
    assert_eq!(wide, "[ 1, 2, 3 ]");
}

#[test]
fn test_function_call_min_widths() {
    let call = Expression::FuncCall(
        "f".into(),
        vec![Expression::Var("x".into()), Expression::Var("y".into())],
    );
    let doc = call.to_doc();
    let narrow = pretty(3, &doc);
    assert!(narrow.contains("\n"));
    let wide = pretty(80, &doc);
    assert_eq!(wide, "f(x, y)");
}

#[test]
fn test_constructor_min_widths() {
    let cons = Expression::Constructor(
        "Pair".into(),
        vec![
            Box::new(Expression::CInt(10)),
            Box::new(Expression::CInt(20)),
        ],
    );
    let doc = cons.to_doc();
    let narrow = pretty(4, &doc);
    assert!(narrow.contains("\n") || narrow.contains("Pair 10 20"));
    let wide = pretty(80, &doc);
    assert_eq!(wide, "Pair 10 20");
}

// ============================================================
// (3) TESTES DE INTEGRAÇÃO (PARSER + STATEMENTS)
// ============================================================

#[test]
fn test_assignment_sequence_pretty_print() {
    let input_program = "x = 10; y = 20; z = x + y;";
    let (remaining, ast) = parse(input_program).unwrap();
    assert!(
        remaining.trim().is_empty(),
        "Parser não consumiu toda a entrada"
    );
    let expected_output = r#"x = 10;

y = 20;

z = x + y;"#;
    let program_doc = program_to_doc(&ast);
    let formatted_program = pretty(80, &program_doc);
    assert_eq!(formatted_program, expected_output);
}

// ---------------------------------------------
// Comentários
// ---------------------------------------------

#[test]
fn test_line_comment_basic() {
    use r_python::pretty_print::pretty_print::*;
    let c = line_comment("// hello");
    assert_eq!(pretty(20, &c), "// hello");
}

#[test]
fn test_line_comment_ln_trailing() {
    use r_python::pretty_print::pretty_print::*;
    let c = line_comment_ln("note");
    assert_eq!(pretty(40, &c), "// note\n");
}

#[test]
fn test_block_comment_single_line() {
    use r_python::pretty_print::pretty_print::*;
    let c = block_comment("/* one */");
    assert_eq!(pretty(40, &c), "/* one */");
}

#[test]
fn test_block_comment_multi_line() {
    use r_python::pretty_print::pretty_print::*;
    let src = "/*\n line one\n  line two  \n\n*/";
    let c = block_comment(src);
    let rendered = pretty(80, &c);
    let expected = "/*\n * line one\n * line two\n */";
    assert_eq!(rendered, expected);
}

// ---------------------------------------------
// Stress / Robustez Fase 6
// ---------------------------------------------

#[test]
fn test_punctuate_adaptive_large_list_layout_variation() {
    use r_python::pretty_print::pretty_print::*;
    // Lista com > 8 elementos para forçar caminho balanceado.
    let elems: Vec<Rc<Doc>> = (0..15).map(|i| text(format!("x{i}"))).collect();
    let doc = group(punctuate(concat(text(","), line()), &elems));
    // Largura generosa deve manter tudo em uma linha.
    let single_line = pretty(200, &doc);
    assert!(single_line.contains("x14"));
    assert!(!single_line.contains('\n'));
    // Largura pequena deve quebrar.
    let multi = pretty(10, &doc);
    assert!(multi.lines().count() > 1);
}

#[test]
fn test_fill_with_mixed_length_items() {
    use r_python::pretty_print::pretty_print::*;
    let docs = vec![
        text("short"),
        concat(text("veryverylongsegment"), line()),
        text("mid"),
        text("s"),
    ];
    let filled = fill(&docs);
    let wide = pretty(120, &filled);
    assert!(wide.contains("veryverylongsegment"));
    // Em largura estreita, pelo menos uma quebra deve ocorrer.
    let narrow = pretty(15, &filled);
    assert!(narrow.lines().count() >= 2);
}

#[test]
fn test_comments_integrated_sequence() {
    use r_python::pretty_print::pretty_print::*;
    let seq = concat(
        line_comment_ln("um comentario"),
        group(concat(text("x = 1"), concat(line(), text("y = 2")))),
    );
    let rendered = pretty(20, &seq);
    assert!(rendered.starts_with("// um comentario"));
    assert!(rendered.contains("x = 1"));
}

#[test]
#[cfg(feature = "pp-timing")]
fn test_timing_feature_compiles_and_returns_metrics() {
    use r_python::pretty_print::pretty_print::*;
    let doc = group(concat(text("alpha"), concat(line(), text("beta"))));
    let metrics = pretty_with_metrics(5, &doc);
    assert!(metrics.duration_ns > 0);
    assert_eq!(metrics.width, 5);
    assert_eq!(metrics.output, "alpha\nbeta");
}

// ============================================================
// (4) PLACEHOLDER FASE 6 (novos recursos) - manter organizado
// ============================================================
// Futuramente: testes de fill, comments, adaptive punctuate, timing (feature gate)
// Ex: test_fill_compact_layout, test_line_comment_preserves_spacing, etc.

// ============================================================
// (4.a) FASE 6 - TESTES DO COMBINADOR FILL
// ============================================================

#[test]
fn test_fill_keeps_items_on_same_line_when_possible() {
    let docs = vec![
        text("a"),
        space(),
        text("b"),
        space(),
        text("c"),
        space(),
        text("d"),
    ];
    // Sem quebras explícitas, fill deve simplesmente manter em linha única se couber.
    let f = fill(&docs);
    let rendered = pretty(10, &f);
    assert_eq!(rendered, "a b c d");
}

#[test]
fn test_fill_breaks_progressively() {
    // Intercalamos line() para permitir pontos de quebra.
    let docs = vec![
        text("alpha"),
        line(),
        text("beta"),
        line(),
        text("gamma"),
        line(),
        text("delta"),
    ];
    let f = fill(&docs);
    // Largura pequena força várias quebras; ainda assim não deve colapsar em uma quebra por item se algumas combinações cabem.
    let narrow = pretty(7, &f); // 7 permite às vezes juntar palavra curta.
                                // Aceitamos dois formatos possíveis dependendo de heurística de groups (mas garantimos prefixo e ordem):
    assert!(narrow.starts_with("alpha"));
    assert!(narrow.contains("beta"));
    assert!(narrow.contains("gamma"));
    assert!(narrow.contains("delta"));
    // Largura grande mantém tudo em uma linha, substituindo line() por espaços nas regiões flatten.
    let wide = pretty(80, &f);
    assert_eq!(wide, "alpha beta gamma delta");
}
