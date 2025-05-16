use crate::ir::ast::Function;
use crate::ir::ast::Name;

use std::collections::HashMap;
use std::collections::LinkedList;

pub struct Scope<A> {
    pub variables: HashMap<Name, A>,
    pub functions: HashMap<Name, Function>,
}

impl<A> Scope<A> {
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

    fn lookup(&mut self, var: Name) -> Option<&A> {
        self.variables.get(&var)
    }
}

pub struct Environment<A> {
    pub globals: Scope<A>,
    pub stack: LinkedList<Scope<A>>,
}

impl<A> Environment<A> {
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
        };
        return ();
    }

    pub fn lookup(&mut self, var: Name) -> Option<&A> {
        match self.stack.front_mut() {
            None => self.globals.lookup(var),
            Some(top) => top.lookup(var),
        }
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
    use crate::environment::environment::{Environment, Scope};

    #[test]
    fn eval_map_and_lookup_var() {
        let mut s: Scope<i32> = Scope::new();

        s.map_variable("x".to_string(), 32);
        s.map_variable("y".to_string(), 23);

        assert_eq!(Some(32), s.lookup("x".to_string()).copied());
        assert_eq!(Some(23), s.lookup("y".to_string()).copied());
    }

    #[test]
    fn eval_environment() {
        let mut env: Environment<i32> = Environment::new();

        env.map_variable("x".to_string(), 32);

        env.push();

        env.map_variable("x".to_string(), 55);
        env.map_variable("y".to_string(), 23);

        assert_eq!(Some(55), env.lookup("x".to_string()).copied());
        assert_eq!(Some(23), env.lookup("y".to_string()).copied());
        assert_eq!(None, env.lookup("a".to_string()).copied());

        env.pop();

        assert_eq!(Some(32), env.lookup("x".to_string()).copied());
        assert_eq!(None, env.lookup("y".to_string()).copied());
    }
}
