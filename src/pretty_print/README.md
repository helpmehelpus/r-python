# Pretty Printing para AST (Abstract Syntax Tree) - Projeto Rust

Este projeto implementa **pretty printing** para a AST de uma linguagem, facilitando a visualização, depuração e validação da estrutura dos programas em memória. O pretty print exibe cada nó da árvore (tipos, expressões, comandos, funções, argumentos, etc.) em formato legível e indentado.

## Estrutura dos Arquivos

### 1. **Módulos de Pretty Print**
O projeto agora está organizado em arquivos separados por responsabilidade, cada um contendo a trait de pretty print e sua implementação:

- **pretty_type.rs**: Trait e implementação para tipos (`Type`).
- **pretty_expressions.rs**: Trait e implementação para expressões (`Expression`).
- **pretty_statements.rs**: Trait e implementação para comandos/statements (`Statement`).
- **pretty_functions.rs**: Trait e implementação para funções (`Function`) e argumentos formais (`FormalArgument`).
- **pretty_print.rs**: Módulo centralizador que reexporta todas as traits de pretty print, facilitando o uso.

**Importante:**  
Para usar as funções de pretty print, **importe sempre pelo módulo central**:
```rust
use crate::pretty_print::PrettyPrintType;
use crate::pretty_print::PrettyPrintExpression;
use crate::pretty_print::PrettyPrintStatement;
use crate::pretty_print::PrettyPrintFunction;
use crate::pretty_print::PrettyPrintFormalArgument;
```

Cada trait define uma função, normalmente chamada `pretty_print_xxx`, que recebe o nível de indentação e retorna uma `String` formatada.

#### Exemplo de Uso

```rust
let expr = Expression::Add(
    Box::new(Expression::CInt(1)),
    Box::new(Expression::Mul(
        Box::new(Expression::CInt(2)),
        Box::new(Expression::CInt(3)),
    )),
);
println!("{}", expr.pretty_print_expr(0));
```

### 2. **Testes**
Os testes automatizados agora estão **dentro de cada arquivo de pretty print** (por exemplo, em `pretty_type.rs`, `pretty_expressions.rs`, etc.)  
Utilizam o framework de testes do Rust (`#[cfg(test)] mod tests`) para comparar resultados esperados com o retorno das funções de pretty print.

#### Como rodar os testes

No terminal, execute:

```
cargo test
```

Todos os testes devem passar. Se um teste falhar, reveja o resultado da função de pretty printing correspondente.

---

## Como testar e validar

- Crie exemplos de cada enum/struct (`Type`, `Expression`, `Statement`, `Function`, `FormalArgument`).
- Use as funções de pretty print com indentação zero ou customizada.
- Compare o resultado visualmente ou crie testes automatizados.
- Para garantir que mudanças futuras não quebrem a saída, mantenha e atualize os testes unitários.

---

## Como dar manutenção

### Para adicionar novos tipos/enums/structs na AST

1. **Adicione o novo tipo/enum/struct em `ir/ast.rs`.**
2. Implemente uma nova trait de pretty printing para ele em um arquivo específico (ex: `pretty_newtype.rs`).
3. Adapte os enums/structs existentes para usar o novo tipo, se necessário.
4. Adicione testes no próprio arquivo, criando exemplos que usem o novo tipo/enum/struct.

### Para modificar o formato da saída

- Altere o corpo das funções de pretty print nas traits correspondentes.
- Adapte a indentação, nomes, espaçamento, etc.
- Rode os testes automatizados para garantir que a saída continua correta (ou atualize os testes, se a saída esperada mudou).

### Para depurar

- Imprima exemplos de ASTs usando as funções de pretty print.
- Adicione logs temporários ou prints adicionais, se necessário.
- Se a saída não estiver como esperado, revise o match/casos das funções de pretty print.

---

## Recomendações de boas práticas

- **Mantenha os testes unitários sempre atualizados.** Eles garantem que mudanças não quebram o pretty print.
- **Separe as funções de pretty print em traits e arquivos por responsabilidade** para facilitar extensão e manutenção.
- **Documente novos tipos/enums/structs** e explique seu papel na AST.
- **Evite hardcoding de strings**; use constantes para nomes padrão se necessário.
- **Utilize a indentação dinâmica** para facilitar a leitura de árvores aninhadas.

---

## Exemplos rápidos

### Pretty print de um Type

```rust
let typ = Type::TList(Box::new(Type::TInteger));
println!("{}", typ.pretty_print_type(0));
// Saída:
// List(
//   Int
// )
```

### Pretty print de uma função

```rust
let func = Function {
    name: "f".to_string(),
    args: vec![
        FormalArgument { name: "a".to_string(), typ: Type::TInteger },
    ],
    return_type: Some(Type::TBool),
    body: Statement::Return(Box::new(Expression::CTrue)),
};
println!("{}", func.pretty_print_func(0));
// Saída (indentada):
// Function(
//   Name: f
//   Args: [
//     FormalArg(a,   Int)
//   ]
//   Return:   Bool
//   Body:
//     Return(
//       True
//     )
// )
```

---

## Dúvidas ou sugestões

Abra issues no repositório ou entre em contato com os mantenedores.  
Sinta-se livre para contribuir com novos tipos, melhorias na visualização ou novos testes!