# Pretty-Printer para r-python

Este projeto implementa um pretty-printer robusto para a Árvore de Sintaxe Abstrata (AST) da linguagem `r-python`. O objetivo é converter a estrutura da AST em memória de volta para código-fonte legível, com formatação consistente, indentação correta e quebras de linha inteligentes que se adaptam ao espaço disponível. 📜

## Conceitos Fundamentais: O Algoritmo de Wadler/Oppen

A implementação é baseada nos algoritmos formalizados nos artigos de **Derek C. Oppen** e, principalmente, na abordagem funcional e elegante de **Philip Wadler**.

A lógica opera em duas fases principais:

1. **Construção do Documento (`AST` -> `Doc`)**: A AST é convertida para uma representação de layout intermediária e abstrata, chamada `Doc`. Esta estrutura descreve o documento em termos de `text`, `line` (possíveis quebras de linha), e `nest` (indentação), sem se comprometer com uma formatação final.

2. **Renderização do Documento (`Doc` -> `String`)**: Um motor de renderização processa a estrutura `Doc` e a transforma na `String` final. É aqui que a "mágica" acontece: o motor decide qual o melhor layout para uma dada largura de linha. A primitiva `group` é a chave, pois permite definir layouts alternativos (por exemplo, "tente manter em uma linha, mas se não couber, quebre a linha e indente aqui").

Essa arquitetura torna o pretty-printer extremamente flexível e poderoso.

---

## Estrutura do Projeto

A estrutura atual reflete a separação de responsabilidades:

-   `pretty_print.rs`: Contém o **coração do pretty-printer**. Define a estrutura `Doc`, o trait `ToDoc`, e o motor de renderização (`pretty`, `best`, `fits`) que implementa o algoritmo.
-   `pretty_type.rs`: Implementa `ToDoc` para os nós de tipo da AST (`Type`, `ValueConstructor`).
-   `pretty_expressions.rs`: Implementa `ToDoc` para os nós de expressão da AST (`Expression`).
-   `pretty_statements.rs`: Implementa `ToDoc` para os nós de comando da AST (`Statement`, `Function`, `FormalArgument`).
-   `mod.rs`: O ponto de entrada do módulo, que declara os submódulos.

---

## Como Usar

O uso do pretty-printer é centralizado e simples. O fluxo de trabalho é sempre:
1.  Ter uma instância de um nó da AST (uma expressão, um statement, etc.).
2.  Importar o trait `ToDoc` e a função `pretty`.
3.  Chamar o método `.to_doc()` no seu nó da AST para obter a representação `Doc`.
4.  Passar o `Doc` e a largura de linha desejada para a função `pretty()`.

#### Exemplo de Uso

```rust
use crate::ir::ast::{Expression, Statement};
use crate::pretty_print::pretty_print::{pretty, ToDoc}; // Importações principais

// 1. Crie um nó da AST.
let stmt = Statement::VarDeclaration(
    "resultado".to_string(),
    Box::new(Expression::Add(
        Box::new(Expression::CInt(10)),
        Box::new(Expression::CInt(20)),
    )),
);

// 2. Converta a AST para um Doc.
let document = stmt.to_doc();

// 3. Renderize o Doc para uma String com a largura desejada.
let formatted_code = pretty(80, &document); // Largura de 80 colunas

// 4. Imprima o resultado.
println!("{}", formatted_code);
// Saída esperada: var resultado = 10 + 20;


-----

## Layout Flexível: A Magia do `group` 🚀

A principal vantagem desta implementação é sua capacidade de adaptar o layout. Veja o mesmo nó da AST (`FuncCall`) renderizado com larguras diferentes:

#### Exemplo 1: Com Espaço Suficiente (width = 120)

```rust
// AST para: minha_funcao(arg1_longo, arg2_longo, arg3_longo)
let doc = ...;
println!("{}", pretty(120, &doc));
```

**Saída:**

```
minha_funcao(arg1_longo, arg2_longo, arg3_longo)
```

#### Exemplo 2: Com Espaço Limitado (width = 40)

```rust
let doc = ...; // O mesmo doc de antes
println!("{}", pretty(40, &doc));
```

**Saída:**

```
minha_funcao(
  arg1_longo,
  arg2_longo,
  arg3_longo
)
```

-----

## Executando os Testes

Os testes unitários estão localizados dentro de cada módulo e validam tanto a conversão para `Doc` quanto o resultado final da renderização com diferentes larguras.

Para rodar todos os testes do projeto, execute no terminal:

```bash
cargo test
```

-----

## Como Estender o Pretty-Printer

Adicionar suporte para novos nós da AST é um processo direto:

1.  **Crie o tipo** no `ir/ast.rs`.
2.  Abra o arquivo `pretty_*.rs` correspondente (ex: `pretty_expressions.rs` se for uma nova `Expression`).
3.  **Implemente o trait `ToDoc`** para seu novo tipo. Use os construtores (`text`, `line`, `nest`, `group`, etc.) para descrever o layout desejado.
4.  Adicione testes unitários no mesmo arquivo para validar a formatação em diferentes larguras.

É isso\! O motor de renderização cuidará do resto. ✨
