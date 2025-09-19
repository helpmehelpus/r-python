//! Exemplo de instrumentação com `pp-timing` (+ opcional `pp-profile`).
//! Execute com:
//!   cargo run --release --features "pp-timing" --example pp_timing
//! Ou com contadores:
//!   cargo run --release --features "pp-timing pp-profile" --example pp_timing

#[cfg(feature = "pp-timing")]
fn main() {
    use r_python::prelude::*;

    // Construímos um Doc com mistura de grupos, nest e fill para gerar alguma atividade.
    let items: Vec<_> = (0..20)
        .map(|i| {
            group(concat(
                text(format!("item{i}")),
                concat(line(), text("valor")),
            ))
        })
        .collect();
    let filled = fill(&items);
    let doc = group(nest(2, concat(text("lista:"), concat(line(), filled))));

    // Mede em diferentes larguras para comparar decisões de quebra.
    for &w in &[120usize, 40, 15] {
        let metrics = pretty_with_metrics(w, &doc);
        println!("--- width={w} ---");
        println!("{}", metrics.output);
        println!(
            "len={} duration_ns={}{}",
            metrics.rendered_len,
            metrics.duration_ns,
            match (
                metrics.flatten_calls,
                metrics.fits_calls,
                metrics.group_decisions
            ) {
                (Some(f), Some(ft), Some(g)) => format!(" flatten={f} fits={ft} groups={g}"),
                _ => String::new(),
            }
        );
        println!();
    }
}

#[cfg(not(feature = "pp-timing"))]
fn main() {
    eprintln!("Este exemplo requer a feature 'pp-timing'. Use: \n  cargo run --features pp-timing --example pp_timing");
}
