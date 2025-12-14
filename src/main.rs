//use crate::ir::ast::Expression;
//use crate::ir::ast::Statement;
//use crate::interpreter::interpreter::eval;

/*
use crate::interpreter::interpreter::{execute, ControlFlow};
use crate::ir::ast::{Statement, Type};
use crate::parser::parser::parse;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;*/

pub mod environment;
pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod pretty_print;
pub mod stdlib;
pub mod type_checker;

use crate::environment::environment::Environment;
use crate::interpreter::statement_execute::execute_block;
use crate::parser::parse;

/// Pequeno driver para executar um programa R-Python de um arquivo.
/// Uso: `cargo run -- <caminho_para_arquivo.rpy>`
/// Se nenhum arquivo for passado, executa um programa de demonstração.
fn main() {
    let mut args = std::env::args().skip(1);
    let source = if let Some(path) = args.next() {
        std::fs::read_to_string(&path).unwrap_or_else(|e| {
            eprintln!("Erro ao ler {}: {}", path, e);
            std::process::exit(1);
        })
    } else {
        // Programa de demonstração básico
        "var x = 10; var y = 5; assert(x + y == 15, \"soma falhou\");".to_string()
    };

    let (rest, stmts) = match parse(&source) {
        Ok(res) => res,
        Err(e) => {
            eprintln!("Erro de parsing: {}", e);
            std::process::exit(1);
        }
    };

    if !rest.trim().is_empty() {
        eprintln!("Aviso: código não consumido: '{}'", rest);
    }

    let env = Environment::new();
    match execute_block(stmts, &env) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Erro em tempo de execução: {}", e);
            std::process::exit(1);
        }
    }
}
