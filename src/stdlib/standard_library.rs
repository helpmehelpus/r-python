use crate::environment::environment::Environment;
use crate::ir::ast::{Statement, Expression};
use std::collections::HashMap;
use std::sync::OnceLock;

pub type MetaBuiltinStmt = fn(&mut Environment<Expression>) -> Statement;

// Tabela estática global de metabuiltins
static METABUILTINS_TABLE: OnceLock<HashMap<String, MetaBuiltinStmt>> = OnceLock::new();

pub fn get_metabuiltins_table() -> &'static HashMap<String, MetaBuiltinStmt> {
    METABUILTINS_TABLE.get_or_init(|| {
        let mut table = HashMap::new();
        table.insert("input".to_string(), input_builtin as MetaBuiltinStmt);
        table.insert("print".to_string(), print_builtin as MetaBuiltinStmt);
        table
    })
}

pub fn input_builtin(env: &mut Environment<Expression>) -> Statement {
    let prompt = match env.lookup(&"prompt".to_string()) {
        Some((_, Expression::CString(s))) => s.clone(),
        _ => "".to_string(),
    };
    print!("{}", prompt);
    use std::io::{self, Write};
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let input = input.trim_end_matches(['\n', '\r']).to_string();
    Statement::Return(Box::new(Expression::CString(input)))
}

pub fn print_builtin(env: &mut Environment<Expression>) -> Statement {
    let value = env.lookup(&"value".to_string())
        .map(|(_, v)| v)
        .unwrap_or(Expression::CString("".to_string()));
    match value {
        Expression::CString(s) => println!("{}", s),
        Expression::CInt(i) => println!("{}", i),
        Expression::CReal(f) => println!("{}", f),
        _ => println!("{:?}", value),
    }
    Statement::Return(Box::new(Expression::CVoid))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_meta_stmt_table_contains_input_function() {
        let table = get_metabuiltins_table();
        assert!(table.contains_key("input"), "The table must contain the 'input' function");
        
        // Check if the function in the table is callable
        let input_func = table.get("input").unwrap();
        let _env: Environment<Expression> = Environment::new();
        // We do not execute the function to avoid blocking stdin
        // Just check if it exists and can be referenced
        assert!(std::ptr::addr_of!(*input_func) != std::ptr::null(), 
                "The 'input' function must exist in the table");
    }

    #[test]
    fn test_meta_stmt_table_contains_print_function() {
        let table = get_metabuiltins_table();
        assert!(table.contains_key("print"), "The table must contain the 'print' function");
        
        // Check if the function in the table is callable
        let print_func = table.get("print").unwrap();
        let mut env: Environment<Expression> = Environment::new();
        let result = print_func(&mut env);
        match result {
            Statement::Return(_) => (), // Expected
            _ => panic!("The 'print' function in the table must return Statement::Return"),
        }
    }

    #[test]
    fn test_meta_stmt_table_has_correct_size() {
        let table = get_metabuiltins_table();
        assert_eq!(table.len(), 2, "The table must contain exactly 2 functions");
    }

    #[test]
    fn test_meta_stmt_table_contains_only_expected_keys() {
        let table = get_metabuiltins_table();
        let keys: Vec<&String> = table.keys().collect();
        
        assert!(keys.contains(&&"input".to_string()), "The table must contain the key 'input'");
        assert!(keys.contains(&&"print".to_string()), "The table must contain the key 'print'");
        assert_eq!(keys.len(), 2, "The table must contain only 2 keys");
    }

    #[test]
    fn test_input_builtin_function_signature() {
        // Checks if the input_builtin function can be referenced with an Environment
        // Note: We do not execute the function because it requires stdin
        let _input_func: MetaBuiltinStmt = input_builtin;
        assert!(true, "input_builtin has the correct signature");
    }

    #[test]
    fn test_print_builtin_function_signature() {
        // Checks if the print_builtin function can be called with an Environment
        let mut env: Environment<Expression> = Environment::new();
        env.map_variable("value".to_string(), false, Expression::CString("Hello World".to_string()));
        
        let result = print_builtin(&mut env);
        match result {
            Statement::Return(_) => (), // Expected
            _ => panic!("print_builtin must return Statement::Return"),
        }
    }

    #[test]
    fn test_meta_stmt_table_functions_are_callable() {
        let table = get_metabuiltins_table();
        
        // Check if the 'input' function exists in the table (without executing it)
        assert!(table.contains_key("input"), "The 'input' function must be in the table");
        
        // Check if the 'print' function can be called
        if let Some(print_func) = table.get("print") {
            let mut env: Environment<Expression> = Environment::new();
            let result = print_func(&mut env);
            match result {
                Statement::Return(_) => (), // Expected
                _ => panic!("The 'print' function in the table must return Statement::Return"),
            }
        } else {
            panic!("The 'print' function must be in the table");
        }
    }
} 