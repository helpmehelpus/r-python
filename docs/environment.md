# Environment Module Documentation

The environment module provides a lexically-scoped symbol table implementation for the R-Python language. It supports both variable and function bindings, with proper scope chain resolution.

## Overview

The module implements two main structures:
- `Scope<A>`: Represents a single scope with its variable and function bindings
- `Environment<A>`: Manages a stack of scopes with a global scope at the bottom

The generic parameter `A` allows the environment to be used for different purposes:
- Type checking: `Environment<Type>`
- Interpretation: `Environment<Expression>`

## Structures

### Scope<A>

A single scope containing mappings for variables and functions.

```rust
pub struct Scope<A> {
    pub variables: HashMap<Name, A>,
    pub functions: HashMap<Name, Function>,
}
```

#### Methods

- `new() -> Scope<A>`: Creates a new empty scope
- `map_variable(var: Name, value: A)`: Binds a variable in the current scope
- `map_function(function: Function)`: Binds a function in the current scope
- `lookup_var(var: &Name) -> Option<&A>`: Looks up a variable in this scope
- `lookup_function(name: &Name) -> Option<&Function>`: Looks up a function in this scope

### Environment<A>

Manages a stack of scopes with lexical scoping rules.

```rust
pub struct Environment<A> {
    pub globals: Scope<A>,
    pub stack: LinkedList<Scope<A>>,
}
```

#### Methods

- `new() -> Environment<A>`: Creates a new environment with empty global scope
- `map_variable(var: Name, value: A)`: Maps a variable in the current scope
- `map_function(function: Function)`: Maps a function in the current scope
- `lookup(var: &Name) -> Option<&A>`: Looks up a variable through the scope chain
- `lookup_function(name: &Name) -> Option<&Function>`: Looks up a function through the scope chain
- `push()`: Creates a new scope at the top of the stack
- `pop()`: Removes the topmost scope
- `scoped_function() -> bool`: Checks if we're in a function scope

## Scoping Rules

1. **Variable Resolution**:
   - First checks the local scopes from innermost to outermost
   - Falls back to global scope if not found in any local scope
   - Returns None if the variable is not found anywhere

2. **Function Resolution**:
   - Follows the same rules as variable resolution
   - Functions can be defined in any scope
   - Inner functions can shadow outer functions

3. **Scope Management**:
   - New scopes are pushed when entering a function or block
   - Scopes are popped when exiting their block
   - Global scope always remains at the bottom

## Usage Examples

### Type Checking

```rust
let mut type_env: Environment<Type> = Environment::new();

// In global scope
type_env.map_variable("x".to_string(), Type::TInteger);

// In function scope
type_env.push();
type_env.map_variable("y".to_string(), Type::TReal);
```

### Interpretation

```rust
let mut runtime_env: Environment<Expression> = Environment::new();

// In global scope
runtime_env.map_variable("x".to_string(), Expression::CInt(42));

// In function scope
runtime_env.push();
runtime_env.map_variable("y".to_string(), Expression::CReal(3.14));
```

### Nested Scopes

```rust
let mut env: Environment<i32> = Environment::new();

// Global scope
env.map_variable("x".to_string(), 1);

// Outer function scope
env.push();
env.map_variable("y".to_string(), 2);

// Inner function scope
env.push();
env.map_variable("z".to_string(), 3);

// Variables from all scopes are accessible
assert!(env.lookup(&"x".to_string()).is_some()); // from global
assert!(env.lookup(&"y".to_string()).is_some()); // from outer
assert!(env.lookup(&"z".to_string()).is_some()); // from inner

// Pop scopes to clean up
env.pop(); // removes inner scope
env.pop(); // removes outer scope
```

## Implementation Notes

1. The environment uses a `LinkedList` for the scope stack to efficiently push/pop scopes
2. All lookups traverse the entire scope chain for proper lexical scoping
3. The generic parameter `A` must implement `Clone` for the environment to work
4. Functions are treated similarly to variables but stored in a separate map
5. The global scope is always accessible, regardless of the current scope depth 