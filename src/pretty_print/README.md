# Biblioteca de Pretty-Printing para RPython 🚀

[](https://www.rust-lang.org/)
[](https://github.com/UnBCIC-TP2/r-python/issues)
[](https://github.com/UnBCIC-TP2/r-python/actions)

Este fork do projeto **RPython** introduz uma implementação robusta de uma biblioteca de **pretty-printing**. O objetivo principal é converter a Árvore de Sintaxe Abstrata (AST) da linguagem de volta para código-fonte legível, com formatação consistente, indentação correta e quebras de linha inteligentes que se adaptam ao espaço disponível.

## 📋 Sobre a Biblioteca de Pretty-Printer

Um pretty-printer é uma ferramenta essencial no ciclo de vida de um compilador ou interpretador. Ele permite que a representação interna do código (a AST) seja visualizada de forma clara e esteticamente agradável, facilitando a depuração, a análise de código e a interação com o programador.

Esta implementação foi adicionada ao projeto RPython criando um novo módulo, `pretty_print`, e integrando-o à estrutura principal do projeto.

## 🏛️ Conceitos Fundamentais: O Algoritmo de Wadler/Oppen

[cite\_start]A implementação é baseada nos algoritmos formalizados nos artigos de **Derek C. Oppen** e, principalmente, na abordagem funcional e elegante de **Philip Wadler**. A lógica opera em duas fases principais:

1.  **Construção do Documento (`AST` -\> `Doc`)**: A AST é convertida para uma representação de layout intermediária e abstrata, chamada `Doc`. Esta estrutura descreve o documento em termos de `text`, `line` (possíveis quebras de linha), e `nest` (indentação), sem se comprometer com uma formatação final.
2.  **Renderização do Documento (`Doc` -\> `String`)**: Um motor de renderização processa a estrutura `Doc` e a transforma na `String` final. É aqui que a "mágica" acontece: o motor decide qual o melhor layout para uma dada largura de linha. A primitiva `group` é a chave, pois permite definir layouts alternativos (por exemplo, "tente manter em uma linha, mas se não couber, quebre a linha e indente aqui").

Essa arquitetura torna o pretty-printer extremamente flexível e poderoso.

## 🏗️ Estrutura e Integração

Para integrar a biblioteca, a seguinte estrutura de pastas e arquivos foi adicionada ao projeto RPython:

```
src/
└── pretty_print/
    ├── mod.rs                # Ponto de entrada do módulo
    ├── pretty_print.rs       # O motor principal do pretty-printer (Doc, pretty, best, etc.)
    ├── pretty_expressions.rs # Implementação de ToDoc para Expressões
    ├── pretty_statements.rs  # Implementação de ToDoc para Comandos
    ├── pretty_type.rs        # Implementação de ToDoc para Tipos
    └── README.md             # Documentação específica do módulo
```

  - **`main.rs` e `lib.rs`**: Não foram atualizados para declarar e expor o novo módulo `pretty_print`, isto será feito caso o projeto seja aceito na sua implementação definida aqui.
  - **`Cargo.toml`**: Permaneceu o mesmo, pois não foram necessárias novas dependências externas.

## ✨ Como Usar

O uso do pretty-printer é centralizado e simples. O fluxo de trabalho é sempre:

1.  Ter uma instância de um nó da AST (uma expressão, um statement, etc.).
2.  Importar o trait `ToDoc` e a função `pretty`.
3.  Chamar o método `.to_doc()` no nó da AST para obter a representação `Doc`.
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
```

## 🚀 Layout Flexível: A Magia do `group`

A principal vantagem desta implementação é sua capacidade de adaptar o layout. Veja o mesmo nó da AST (`FuncCall`) renderizado com larguras diferentes:

#### Exemplo 1: Com Espaço Suficiente (width = 120)

```rust
// AST para: minha_funcao(arg1_longo, arg2_longo, arg3_longo)
let doc = ...;
println!("{}", pretty(120, &doc));
```

**Saída:**

```
minha_funcao( arg1_longo, arg2_longo, arg3_longo )
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

## ✅ Executando os Testes

Os testes unitários estão localizados dentro de cada submódulo e validam tanto a conversão para `Doc` quanto o resultado final da renderização com diferentes larguras.

Para rodar todos os testes do projeto, incluindo os da biblioteca de pretty-print, execute no terminal:

```bash
cargo test
```