//! Prelude estável de construção de documentos. Detalhes completos em `docs/PRELUDE.md`.
//! Inclui apenas superfície considerada estável; internals ficam fora.

pub use crate::pretty_print::pretty_print::{
    block_comment, concat, fill, group, hardline, hardline_if_nonempty, join_balanced, line,
    line as softline, line_comment, line_comment_ln, nest, nil, pretty, punctuate, space, text,
    Doc, ToDoc,
};

#[cfg(feature = "pp-timing")]
pub use crate::pretty_print::pretty_print::{pretty_with_metrics, PrettyMetrics};

// Fora do prelude: flatten, best, fits, caches/profile diretos.
