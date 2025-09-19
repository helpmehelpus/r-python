//! Benchmark sintético simples para o pretty printer.
//! Execute com: cargo run --release --features pp-profile --example pp_bench

use r_python::pretty_print::*;
use std::rc::Rc;

fn linear_chain(n: usize) -> Rc<Doc> {
    let mut parts = Vec::with_capacity(n);
    for i in 0..n {
        parts.push(text(format!("item{i}")));
    }
    // Implementação linear (left-deep) via fold
    let sep = concat(text(","), line());
    let doc = parts
        .into_iter()
        .reduce(|acc, d| concat(acc, concat(sep.clone(), d)))
        .unwrap();
    group(doc)
}

fn balanced_chain(n: usize) -> Rc<Doc> {
    let mut parts = Vec::with_capacity(n);
    for i in 0..n {
        parts.push(text(format!("item{i}")));
    }
    let sep = concat(text(","), line());
    group(join_balanced(sep, &parts))
}

fn main() {
    let sizes = [50, 100, 200, 400, 800];
    for &n in &sizes {
        for variant in ["linear", "balanced"] {
            let doc = match variant {
                "linear" => linear_chain(n),
                _ => balanced_chain(n),
            };
            let rendered = pretty(80, &doc);
            // Consumir output para evitar otimização
            assert!(rendered.len() > 0);
            #[cfg(feature = "pp-profile")]
            println!("n={n} variant={variant} {}", pp_profile_report());
        }
    }
}
