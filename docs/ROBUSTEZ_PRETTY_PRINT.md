# Robustez do Pretty Printer

> Objetivo: garantir que o pretty printer seja previsível, observável e extensível antes de ampliarmos o front-end e a geração de código.

Este documento complementa o `REFACTORING_LOG.md` descrevendo como e quando usar os recursos opcionais e novos combinadores do sistema de pretty printing.

## Visão Geral
O pretty printer segue abordagem algébrica (Wadler) com variantes: `Nil, Text, Line, HardLine, Nest, Concat, Group`. Otimizações e extensões introduzidas na Fase 6 visam:
- Observabilidade (tempo, contadores)
- Robustez estrutural (balanceamento, cache opcional)
- Ergonomia (fill, comentários, prelude)

## Combinadores Principais
- `group(doc)`: tenta layout em uma linha; se não couber, quebra nos `Line`.
- `nest(indent, doc)`: aplica indentação às quebras internas.
- `line() / hardline()`: quebra suave vs obrigatória.
- `space()`: atalho `" "`.
- `concat(a, b)`: concatenação direta.
- `punctuate(sep, docs)`: intercala `sep` entre elementos; adaptativo (balanceado) para listas > 8.
- `fill(docs)`: tenta manter itens sucessivos na mesma linha antes de quebrar.
- `hardline_if_nonempty(docs)`: adiciona `hardline` após cada doc quando há pelo menos um.
- Comentários: `line_comment`, `line_comment_ln`, `block_comment`.

## Comentários
- `line_comment("foo")` produz `// foo` (prefixo `//` opcional na entrada é normalizado).
- `line_comment_ln` adiciona quebra obrigatória após o comentário.
- `block_comment`:
  - Entrada single-line => `/* body */`.
  - Multi-linha => formato padronizado com prefixo ` * ` e fechamento em linha separada.
  - Linhas externas vazias são removidas; espaços à esquerda por linha normalizados.

## Prelude
Uso opcional de ergonomia. Para a lista curada de símbolos e política de estabilidade consulte `docs/PRELUDE.md`.
Exemplo rápido:
```
use r_python::prelude::*;
let doc = group(concat(text("a"), concat(line(), text("b"))));
assert_eq!(pretty(4, &doc), "a b");
```

## Features de Instrumentação
| Feature | Propósito | Overhead | Quando usar |
|---------|-----------|----------|-------------|
| `pp-profile` | Contadores (`flatten`, `fits`, grupos) | Muito baixo | Diagnóstico de regressões / tuning rápido |
| `pp-timing` | Tempo total + métricas (struct `PrettyMetrics`) | Baixo (chamada a `Instant`) | Medir impacto de novos combinadores |
| `pp-flatten-cache` | Memoização de `flatten(doc)` por identidade | Médio (HashMap) | Árvores grandes reutilizando subdocs |

Ative combinando:
```
cargo test --features "pp-profile pp-timing"
```

## Estratégias Recomendadas
1. Investigação de desempenho: ligue `pp-profile` para contagem de chamadas.
2. Medição comparativa: adicione `pp-timing` e compare `duration_ns` antes/depois.
3. Casos sintéticos repetitivos: experimente `pp-flatten-cache` e verifique se `flatten_calls` cai proporcionalmente.
4. Uso cotidiano / produção: nenhuma feature para via mais enxuta.

## Padrões de Uso
- Listas grandes: use `punctuate` diretamente; o balanceamento interno evita profundidade excessiva.
- Sequências densas heterogêneas (ex.: parâmetros de função com tamanhos variáveis): considere `fill` para melhor compactação horizontal.
- Comentários antes de blocos: `line_comment_ln` seguido de `group(...)` do bloco garante isolamento visual.
- Documentar módulo público: `block_comment` multi-linha antes de declarações top-level.

## Limitações Atuais
- `fill` ainda não implementa heurística de redistribuição após primeira quebra (versão incremental).
- `block_comment` normaliza indentação em vez de preservar formato ASCII-art.
- Sem suporte a comentários de documentação diferenciados (planejável via futura função `doc_comment`).

## Possíveis Evoluções Futuras
- Interning lazy da forma flatten dentro de `Group` (elimina HashMap externo).
- `fill` avançado (variante com alternância espaço/quebra minimizando altura total).
- Comentários doc vs regulares com marcação distinta.
- Ferramenta de benchmark sintético automatizado (collect + relatório diff).

## Exemplo End-to-End
```
use r_python::prelude::*;

let params: Vec<_> = [
    text("user_id"),
    text("very_long_parameter_name"),
    text("flag"),
].into_iter().collect();

let signature = group(concat(text("fn foo("), concat(fill(&params), text(")"))));
let commented = concat(line_comment_ln("Função de exemplo"), signature);
let out = pretty(40, &commented);
println!("{out}");
```

## Checklist Rápido
- Resultado inesperado flatten? Ative `pp-profile` e observe contadores.
- Layout muito vertical? Tente `fill` em vez de `punctuate` simples.
- Performance degradou? Compare `duration_ns` com e sem alteração.

## Exemplos Práticos

### 1. Instrumentação (tempo + contadores)
Arquivo: `examples/pp_timing.rs`

Somente tempo:
```
cargo run --release --features pp-timing --example pp_timing
```
Tempo + contadores:
```
cargo run --release --features "pp-timing pp-profile" --example pp_timing
```
Mostra variação de layout (larguras 120 / 40 / 15) e, quando `pp-profile` ativado, imprime `flatten=`, `fits=`, `groups=`.

### 2. Comentário seguido de bloco
```
let seq = concat(line_comment_ln("comentário"), group(concat(text("x = 1"), concat(line(), text("y = 2")))));
```

### 3. Lista grande balanceada (> 8 elementos)
```
let xs: Vec<_> = (0..15).map(|i| text(format!("x{i}"))).collect();
let doc = group(punctuate(concat(text(","), line()), &xs));
```

### 4. Fill com itens heterogêneos
```
let docs = vec![text("a"), text("bbbbbbbbbbbb"), text("c"), text("dd")];
let laid = fill(&docs);
```
Mantém densidade à esquerda até a quebra forçada pelo item longo.


---
Manter este arquivo conciso e focado em operação; detalhes cronológicos continuam em `REFACTORING_LOG.md`.
