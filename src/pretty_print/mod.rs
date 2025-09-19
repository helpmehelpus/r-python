// src/pretty_print/mod.rs

//! # Módulo de Pretty-Printing
//!
//! Este módulo define a API pública para a biblioteca de pretty-printing do RPython.
//! Ele atua como uma "fachada" (facade), declarando os submódulos internos e
//! reexportando seus componentes públicos para que possam ser facilmente utilizados
//! por outras partes do compilador.
//!
//! A estrutura é a seguinte:
//! - `pretty_print`: O motor de renderização principal, contendo a definição de `Doc`.
//! - `pretty_expressions`: Lógica para formatar `Expression`s.
//! - `pretty_statements`: Lógica para formatar `Statement`s.
//! - `pretty_type`: Lógica para formatar `Type`s.

// Declara os submódulos para que o compilador os reconheça como parte do módulo `pretty_print`.
pub mod pretty_expressions;
pub mod pretty_print;
pub mod pretty_statements;
pub mod pretty_type;

// Utiliza `pub use` para reexportar todo o conteúdo público dos submódulos.
// Isso cria uma API conveniente e unificada. Em vez de um usuário do módulo precisar
// importar de `crate::pretty_print::pretty_print::ToDoc`, ele pode simplesmente
// importar de `crate::pretty_print::ToDoc`.
// Reexports: mantidos para ergonomia; suprimimos warnings se alguns símbolos
// não forem usados em determinados binários/testes.
#[allow(unused_imports)]
pub use pretty_expressions::*;
#[allow(unused_imports)]
pub use pretty_print::*;
#[allow(unused_imports)]
pub use pretty_statements::*;
#[allow(unused_imports)]
pub use pretty_type::*;
