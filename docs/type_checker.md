# Type Checker Module Documentation

> **⚠️ Work in Progress**  
> This type checker implementation is currently under active development. Some features are incomplete or may change in future versions. Please refer to the test cases for the most up-to-date behavior examples.

## Overview

The type checker module (`src/tc/type_checker.rs`) provides static type analysis for the R-Python language. It implements a type system that ensures type safety at compile time by analyzing expressions and statements according to well-defined typing rules.

## Architecture

The type checker is built around two main functions:
- `check_exp`: Type checking for expressions
- `check_stmt`: Type checking for statements

Both functions work with an `Environment<Type>` that maintains the typing context and variable bindings throughout the analysis.

## Type System

### Basic Types

The type system supports the following basic types:

- `TInteger`: 32-bit signed integers
- `TReal`: 64-bit floating-point numbers  
- `TBool`: Boolean values (true/false)
- `TString`: String literals
- `TVoid`: Unit type (no value)

### Complex Types

- `TList(Box<Type>)`: Homogeneous lists containing elements of the same type
- `TTuple(Vec<Type>)`: Heterogeneous tuples containing elements of different types
- `TMaybe(Box<Type>)`: Optional values that can be `Just(value)` or `Nothing`
- `TResult(Box<Type>, Box<Type>)`: Result types for error handling (`Ok(value)` or `Err(error)`)
- `TFunction(Box<Option<Type>>, Vec<Type>)`: Function types with return type and parameter types
- `TAny`: Universal type (used for type inference gaps)
- `Tadt(Name, Vec<ValueConstructor>)`: Algebraic Data Types (ADTs) - **Not yet implemented**

## Expression Type Rules

### Literals

```rust
// Type rules for literal expressions
CTrue, CFalse    : TBool
CInt(_)         : TInteger  
CReal(_)        : TReal
CString(_)      : TString
CVoid           : TVoid
```

### Arithmetic Expressions

Binary arithmetic operations (`+`, `-`, `*`, `/`) follow these rules:

```rust
// Both operands are integers
e1: TInteger, e2: TInteger
─────────────────────────── 
e1 op e2: TInteger

// Mixed integer and real operations  
e1: TInteger, e2: TReal     e1: TReal, e2: TInteger
─────────────────────────   ─────────────────────────
e1 op e2: TReal            e1 op e2: TReal

// Both operands are reals
e1: TReal, e2: TReal
─────────────────────────
e1 op e2: TReal
```

**Error**: Any other type combination results in a type error.

### Boolean Expressions

```rust
// Logical AND and OR
e1: TBool, e2: TBool        e1: TBool, e2: TBool
─────────────────────       ─────────────────────
e1 and e2: TBool           e1 or e2: TBool

// Logical NOT
e: TBool
─────────────
not e: TBool
```

### Relational Expressions

Comparison operations (`==`, `!=`, `<`, `>`, `<=`, `>=`) work on numeric types:

```rust
// Numeric comparisons return boolean
e1: TInteger, e2: TInteger   e1: TReal, e2: TReal
─────────────────────────    ──────────────────────
e1 == e2: TBool             e1 == e2: TBool

// Mixed numeric comparisons
e1: TInteger, e2: TReal      e1: TReal, e2: TInteger  
─────────────────────────    ─────────────────────────
e1 < e2: TBool              e1 > e2: TBool
```

### Variable References

```rust
// Variable lookup in environment
Γ ⊢ x: T    (x is bound to type T in environment Γ)
─────────────
Γ ⊢ Var(x): T
```

**Error**: Reference to undefined variable results in a name error.

### Maybe Types

```rust
// Just constructor
e: T
────────────────────
CJust(e): TMaybe(T)

// Nothing constructor  
CNothing: TMaybe(TAny)

// IsNothing check
e: TMaybe(T)
──────────────────
IsNothing(e): TBool
```

### Result Types

```rust
// Ok constructor
e: T
─────────────────────────
COk(e): TResult(T, TAny)

// Err constructor
e: T  
─────────────────────────
CErr(e): TResult(TAny, T)

// IsError check
e: TResult(T1, T2)
────────────────────
IsError(e): TBool
```

### Unwrap Operations

```rust
// Unwrap Maybe
e: TMaybe(T)
──────────────
Unwrap(e): T

// Unwrap Result
e: TResult(T, E)
──────────────────
Unwrap(e): T
```

### List Values

```rust
// Empty list
ListValue([]): TList(TAny)

// Non-empty homogeneous list
e1: T, e2: T, ..., en: T
─────────────────────────────
ListValue([e1, e2, ..., en]): TList(T)
```

**Error**: Lists with mixed types are rejected.

## Statement Type Rules

### Variable Assignment

```rust
// First assignment (variable declaration)
Γ ⊢ e: T, x ∉ dom(Γ)
─────────────────────────────────
Γ ⊢ Assignment(x, e): Γ[x ↦ T]

// Reassignment with same type
Γ ⊢ e: T, Γ(x) = T
─────────────────────────
Γ ⊢ Assignment(x, e): Γ

// Type error on reassignment with different type
Γ ⊢ e: T1, Γ(x) = T2, T1 ≠ T2
──────────────────────────────── 
Error: type mismatch
```

### Conditional Statements

```rust
// If-then-else with consistent environments
Γ ⊢ e: TBool, Γ ⊢ s1: Γ1, Γ ⊢ s2: Γ2, merge(Γ1, Γ2) = Γ'
────────────────────────────────────────────────────────────
Γ ⊢ IfThenElse(e, s1, Some(s2)): Γ'

// If-then without else
Γ ⊢ e: TBool, Γ ⊢ s: Γ1, merge(Γ, Γ1) = Γ'
─────────────────────────────────────────────
Γ ⊢ IfThenElse(e, s, None): Γ'
```

**Error**: Condition must be boolean type.

### While Loops

```rust
// While loop
Γ ⊢ e: TBool, Γ ⊢ s: Γ'
──────────────────────────
Γ ⊢ While(e, s): Γ'
```

**Error**: Condition must be boolean type.

### Function Definitions

```rust
// Function definition with parameters and body
Γ' = Γ ∪ {p1: T1, ..., pn: Tn}, Γ' ⊢ body: Γ''
──────────────────────────────────────────────────
Γ ⊢ FuncDef(f, [p1:T1, ..., pn:Tn], body): Γ[f ↦ Function]
```

### Statement Sequences
### For Loops

Typing rules for `for` loops support the following iterable types:

- `TList(T)` iterates elements of type `T`.
- `TString` iterates characters as `TString` (1-length strings).
- `TTuple([T, T, ..., T])` iterates elements if and only if the tuple is homogeneous (all element types equal). Empty tuple is rejected.

Scoping and binding:

- The loop introduces an inner scope for the body using `push()`/`pop()`.
- The iterator variable is bound as immutable to the element type in that inner scope.
- The variable does not escape to the outer scope after the loop finishes.

Formally:

```
Γ ⊢ e : TList(T)      or      Γ ⊢ e : TString      or      Γ ⊢ e : TTuple([T, ..., T]) (homogeneous, non-empty)
Γ, x:T ⊢ s : Γ'    (checked in an inner scope)
———————————————————————————————————————————————————————————————————————————————
Γ ⊢ For(x, e, s) : Γ
```

Errors:

- Non-iterable type in `e` → `[Type Error] Type <...> is not iterable`.
- Empty tuple type → `[Type Error] Cannot iterate over empty tuple type`.
- Heterogeneous tuple → `[Type Error] Can only iterate over homogeneous tuples (all elements same type)`.

```rust
// Sequential composition
Γ ⊢ s1: Γ1, Γ1 ⊢ s2: Γ2
──────────────────────────
Γ ⊢ Sequence(s1, s2): Γ2
```

## Environment Merging

The type checker implements environment merging for control flow statements. When merging environments from different branches:

1. **Consistent Variables**: Variables defined in both branches must have the same type
2. **Conditional Variables**: Variables defined in only one branch are added to the merged environment
3. **Type Conflicts**: Mismatched types result in compilation errors

```rust
fn merge_environments(env1: &Environment<Type>, env2: &Environment<Type>) 
    -> Result<Environment<Type>, ErrorMessage>
```

## Error Handling

The type checker produces descriptive error messages for common type errors:

- **Type Mismatch**: Expected one type but found another
- **Name Error**: Reference to undefined variable
- **Arity Error**: Wrong number of arguments to function calls (planned)
- **Consistency Error**: Inconsistent types across control flow branches

## Implementation Status

### ✅ Implemented Features

- [x] Basic type checking for literals
- [x] Arithmetic and boolean expressions
- [x] Variable assignments and references
- [x] Control flow (if-else, while)
- [x] Function definitions (partial)
- [x] Maybe and Result types
- [x] List type checking
- [x] Environment merging for branches

### 🚧 Work in Progress

- [ ] Function call type checking
- [ ] For loop statement type checking (test cases exist)
- [ ] Return statement type checking (partial implementation)
- [ ] ADT constructor type checking (commented out)

### 📋 Planned Features

- [ ] Generic types and type parameters
- [ ] Pattern matching type checking
- [ ] Recursive type definitions
- [ ] Type inference improvements
- [ ] Better error messages with location information

## Testing

The type checker includes comprehensive test suites covering:
- Expression type checking
- Statement type checking  
- Error conditions
- Environment merging
- Control flow analysis

Test cases are located in the `tests` module at the bottom of `type_checker.rs`.

## Usage Example

```rust
use crate::tc::type_checker::{check_exp, check_stmt};
use crate::environment::environment::Environment;
use crate::ir::ast::{Expression, Statement, Type};

// Create typing environment
let mut env: Environment<Type> = Environment::new();

// Type check an expression
let expr = Expression::Add(
    Box::new(Expression::CInt(1)), 
    Box::new(Expression::CInt(2))
);
let result = check_exp(expr, &env); // Ok(Type::TInteger)

// Type check a statement
let stmt = Statement::Assignment(
    "x".to_string(), 
    Box::new(Expression::CString("hello".to_string()))
);
let new_env = check_stmt(stmt, &env)?; // env with x: TString
```

---

> **Documentation Generation Note**  
> This documentation reflects the current implementation status as of the latest version. As this is a work-in-progress module, some features may be incomplete or subject to change. Please refer to the test cases and source code for the most accurate behavior specification. 