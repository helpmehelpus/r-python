// src/pretty_print/mod.rs

#![allow(unused_imports)]

// 1. Declara os submódulos para que o compilador saiba que eles existem.
pub mod pretty_expressions;
pub mod pretty_print;
pub mod pretty_statements;
pub mod pretty_type;

// 2. Torna PÚBLICO tudo o que está dentro de cada submódulo.
//    Isso permite que qualquer parte do seu programa (incluindo os testes)
//    acesse os itens usando o caminho `crate::pretty_print::...`
pub use pretty_expressions::*;
pub use pretty_print::*;
pub use pretty_statements::*;
pub use pretty_type::*;