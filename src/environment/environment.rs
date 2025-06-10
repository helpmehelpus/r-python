use crate::ir::ast::Function;
use crate::ir::ast::Name;

use std::collections::HashMap;
use std::collections::LinkedList;

#[derive(Clone)]
pub struct Scope<A> {
    pub variables: HashMap<Name, A>,
    pub functions: HashMap<Name, Function>,
}

impl<A: Clone> Scope<A> {
    fn new() -> Scope<A> {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn map_variable(&mut self, var: Name, value: A) -> () {
        self.variables.insert(var, value);
        return ();
    }

    fn map_function(&mut self, function: Function) -> () {
        self.functions.insert(function.name.clone(), function);
        return ();
    }

    fn lookup_var(&self, var: &Name) -> Option<&A> {
        self.variables.get(var)
    }

    fn lookup_function(&self, name: &Name) -> Option<&Function> {
        self.functions.get(name)
    }
}

#[derive(Clone)]
pub struct Environment<A> {
    pub globals: Scope<A>,
    pub stack: LinkedList<Scope<A>>,
}

impl<A: Clone> Environment<A> {
    pub fn new() -> Environment<A> {
        Environment {
            globals: Scope::new(),
            stack: LinkedList::new(),
        }
    }

    pub fn map_variable(&mut self, var: Name, value: A) -> () {
        match self.stack.front_mut() {
            None => self.globals.map_variable(var, value),
            Some(top) => top.map_variable(var, value),
        }
    }

    pub fn map_function(&mut self, function: Function) -> () {
        match self.stack.front_mut() {
            None => self.globals.map_function(function),
            Some(top) => top.map_function(function),
        }
    }

    pub fn lookup(&self, var: &Name) -> Option<&A> {
        // First check local scopes in order
        for scope in self.stack.iter() {
            if let Some(value) = scope.lookup_var(var) {
                return Some(value);
            }
        }
        // Then check global scope
        self.globals.lookup_var(var)
    }

    pub fn lookup_function(&self, name: &Name) -> Option<&Function> {
        // First check local scopes in order
        for scope in self.stack.iter() {
            if let Some(func) = scope.lookup_function(name) {
                return Some(func);
            }
        }
        // Then check global scope
        self.globals.lookup_function(name)
    }

    pub fn scoped_function(&self) -> bool {
        !self.stack.is_empty()
    }

    pub fn push(&mut self) -> () {
        self.stack.push_front(Scope::new());
    }

    pub fn pop(&mut self) -> () {
        self.stack.pop_front();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variable_scoping() {
        let mut env: Environment<i32> = Environment::new();

        // Test global scope
        env.map_variable("x".to_string(), 32);
        assert_eq!(Some(&32), env.lookup(&"x".to_string()));

        // Test nested scopes
        env.push();  // scope 1
        env.map_variable("y".to_string(), 23);
        env.map_variable("x".to_string(), 55);  // shadows global x

        env.push();  // scope 2
        env.map_variable("z".to_string(), 44);

        // Variables from all scopes should be accessible
        assert_eq!(Some(&55), env.lookup(&"x".to_string())); // from scope 1
        assert_eq!(Some(&23), env.lookup(&"y".to_string())); // from scope 1
        assert_eq!(Some(&44), env.lookup(&"z".to_string())); // from scope 2

        // Pop scope 2
        env.pop();
        assert_eq!(Some(&55), env.lookup(&"x".to_string())); // still in scope 1
        assert_eq!(Some(&23), env.lookup(&"y".to_string())); // still in scope 1
        assert_eq!(None, env.lookup(&"z".to_string()));      // z is gone

        // Pop scope 1
        env.pop();
        assert_eq!(Some(&32), env.lookup(&"x".to_string())); // back to global x
        assert_eq!(None, env.lookup(&"y".to_string()));      // y is gone
    }

    #[test]
    fn test_function_scoping() {
        let mut env: Environment<i32> = Environment::new();
        
        let global_func = Function {
            name: "global".to_string(),
            kind: None,
            params: None,
            body: None,
        };

        let local_func = Function {
            name: "local".to_string(),
            kind: None,
            params: None,
            body: None,
        };

        // Test function scoping
        env.map_function(global_func.clone());
        assert!(env.lookup_function(&"global".to_string()).is_some());

        env.push();
        env.map_function(local_func.clone());
        
        assert!(env.lookup_function(&"global".to_string()).is_some()); // can see global
        assert!(env.lookup_function(&"local".to_string()).is_some());  // can see local

        env.pop();
        assert!(env.lookup_function(&"global".to_string()).is_some()); // global still visible
        assert!(env.lookup_function(&"local".to_string()).is_none());  // local gone
    }
}
