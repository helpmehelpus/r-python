# PRELUDE

> Superfície pública curada da crate. Hoje concentra utilidades do pretty printer, mas destina-se a agregar também itens estáveis de outros subsistemas (ex.: helpers de diagnóstico, construção de erros, futuras abstrações de ambiente) conforme amadureçam.

## Objetivo
Fornecer um ponto único e estável para importar construções de alto nível da linguagem/engine sem expor internals voláteis. Atualmente o foco é o pretty printer; novos módulos só ingressam quando houver estabilidade mínima e testes claros.

## Escopo Incluído (Estado Atual)
- Pretty printing:
   - Tipos centrais: `Doc`, `ToDoc`.
   - Construtores: `nil`, `text`, `line`, `softline`, `hardline`, `space`, `nest`, `group`, `concat`, `pretty`.
   - Helpers: `punctuate`, `join_balanced`, `hardline_if_nonempty`, `fill`.
   - Comentários: `line_comment`, `line_comment_ln`, `block_comment`.
   - Instrumentação opcional (sob feature): `PrettyMetrics`, `pretty_with_metrics`.

No futuro poderá incluir utilidades estáveis de:
- Diagnostics (formatação padronizada de mensagens de erro).
- Ambiente / runtime (builders de contexto estáveis, não mutáveis).
- Parser simplifications (funções auxiliares neutras em relação a internals).

## Explicitamente Fora do Prelude
| Item | Motivo |
|------|--------|
| `flatten`, `best`, `fits` | Detalhes de implementação sujeitos a refactor. |
| Cache (`pp-flatten-cache`) | Otimização interna; não faz parte da modelagem semântica. |
| Contadores diretos (`pp-profile`) | Acesso indireto via `pretty_with_metrics` pode ser expandido no futuro. |

## Política de Estabilidade
1. Símbolos no prelude não serão removidos sem:
   - Registro prévio no `REFACTORING_LOG.md` indicando deprecação.
2. Mudanças de comportamento serão acompanhadas de testes atualizados e nota de racional.
3. Novos símbolos entram somente após:
   - Terem ao menos 1–2 testes cobrindo caso simples e limite.
   - Não serem marcados como experimentais.

## Quando Usar
- Em testes ou exemplos que apenas constroem Docs ou mensagens.
- Em código de geração de relatórios/diagnósticos (quando forem adicionados).
- Em futuras camadas de codegen.

## Quando Evitar
- Ao modificar o próprio engine (`pretty_print.rs`) ou internals de futuros módulos.
- Ao experimentar combinadores / APIs ainda instáveis (importar direto facilita evolução).
- Se precisar de acesso deliberado a detalhes internos para refactor.

## Processo para Adicionar Algo
1. Implementar função/estrutura em módulo interno.
2. Adicionar testes (happy path + edge mínimo).
3. Registrar racional / estabilidade em `REFACTORING_LOG.md` ou CHANGELOG futuro.
4. Após *cooldown* curto (ou consenso de revisão), reexportar em commit dedicado.

## Exemplos
```rust
use r_python::prelude::*;

let doc = group(concat(text("foo"), concat(line(), text("bar"))));
assert_eq!(pretty(80, &doc), "foo bar");
assert_eq!(pretty(3, &doc), "foo\nbar");
```

Comentários:
```rust
use r_python::prelude::*;
let c = block_comment("/*\n multi\n line \n*/");
let rendered = pretty(40, &c);
assert!(rendered.starts_with("/*"));
```

## Futuras Extensões Planejáveis
- `diagnostic(doc_like)` – construção declarativa de mensagens de erro.
- `env_prelude` (sub-módulo) – acesso somente leitura a abstrações estáveis do ambiente.
- `parser_atoms` – tokens/atoms reutilizáveis para geração de erros ou tooling.

Inclusão condicionada a: baixa chance de refactor imediato + necessidade recorrente externa.

## Dúvidas:
**Posso ignorar o prelude?** Sim, importe de `pretty_print::pretty_print` se preferir granularidade.

**Isso afeta performance?** Não – é só reexport; zero custo de runtime.

**Por que `flatten` não está aqui?** Evita dependência em detalhes que podem ganhar caching/pipeline diferente.

**E se eu quiser algo que não está no prelude?** Importe diretamente; se virar uso recorrente e estável, vamos propor inclusão.

---
Este arquivo deve permanecer curto; detalhes históricos continuam no `REFACTORING_LOG.md`.
