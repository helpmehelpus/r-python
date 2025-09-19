# Pretty Print Refactoring Log

Este documento registra, passo a passo, todas as mudanças realizadas no módulo `pretty_print`.

## Contexto Inicial
- Objetivo: Melhorar corretude (precedência/associatividade), preservar quebras obrigatórias (`HardLine`), preparar base para futuras otimizações.
- Problemas identificados:
  1. Operadores não associativos podem perder parênteses necessários.
  2. `flatten` remove `HardLine`, permitindo quebra obrigatória ser suprimida em grupos.
  3. `fits` potencialmente caro (usa caminho completo de renderização) – otimização futura.
  4. Variantes AST não implementadas ainda (será tratado depois desta primeira rodada).

## Plano de Fase 1
1. Capturar baseline (tests atuais) e anotar aqui.
2. Corrigir precedência/associatividade.
3. Ajustar `flatten` para preservar `HardLine`.
4. Adicionar testes específicos de regressão.
5. Documentar cada alteração com antes/depois.

---

## 2025-09-16 - Baseline
Status dos testes após `cargo test` (baseline antes de alterações de precedência / flatten):
- Crate principal: 31 passed; 0 failed; 7 ignored (módulo `pretty_print` interno ao lib)
- Binário main: 216 passed; 0 failed; 8 ignored
- Testes externos (`tests/`): 6 passed; 0 failed; 9 ignored
- pretty_print_tests (arquivo dedicado em `tests/`): 1 passed

Observação: Nenhuma falha existente — alterações devem manter 0 falhas.

## 2025-09-16 - Correção de Precedência (Planejado)
- Estratégia: Passar `prec+1` ao lado direito para operadores não associativos (Sub, Div, comparação, lógica curta-circuito se aplicável) ou inserir regra condicional de parênteses.
- Rationale: Garante que `a - (b - c)` não seja impresso como `a - b - c`.
- Arquivo(s): `pretty_expressions.rs`.

## 2025-09-16 - Preservar HardLine (Planejado)
- Mudança: Em `flatten`, retornar `HardLine` intacto.
- Justificativa: Quebras obrigatórias não devem desaparecer em layout flatten.
- Impacto: `fits` passará a falhar mais cedo para grupos que contenham `HardLine`.

## 2025-09-16 - Testes Novos (Planejado)
Casos:
- Expressão: `a - (b - c)` mantém parênteses.
- Expressão: `a / (b / c)` mantém parênteses.
- Grupo com `HardLine` dentro não é achatado.

---

## 2025-09-16 - Correção de Precedência (Aplicado)
Mudanças implementadas:
- Introduzido ajuste de precedência para o operando direito em operadores não associativos (Sub, Div) e também para comparações.
- Estratégia: adicionar `right_prec_adjust = 1` passando `parent_precedence + 1` ao formatar o RHS, forçando parênteses quando sua precedência for igual/menor.
- Impacto esperado: evita perda de parênteses em casos como `a - (b - c)` e `a - (b + c)`.

## 2025-09-16 - Flatten preserva HardLine (Aplicado)
Mudanças implementadas:
- Em `flatten`, ramo `HardLine` agora retorna `Doc::HardLine` (antes retornava `Nil`).
- Resultado: qualquer `Group` contendo `HardLine` nunca será considerado "flat" (pois `fits` trata `HardLine` como não cabendo), garantindo quebra obrigatória.

## 2025-09-16 - Testes Novos (Adicionados)
- `test_precedence_sub_nested`
- `test_precedence_div_nested`
- `test_group_with_hardline_never_flattens`

Todos devem passar e manter regressões cobertas.

### Resultado dos testes pós-modificações
- Suite completa: 219 passed (bin), 34 passed (lib), 0 falhas, contagens ignoradas inalteradas.
- Confirmação: novos testes (`sub_nested`, `div_nested`, `group_with_hardline_never_flattens`) aprovados.

### Observações
- Ajuste no teste de HardLine: a ausência de indentação após `hardline()` produz quebra simples `\n` sem espaço; expectativa corrigida.
- Warnings de doc comments não-impactantes (poderão ser limpos em fase de polishing).


### Estado Atual Pós Fase 1
- Itens 1 e 2 concluídos com cobertura de testes específica.
- Restante pendente para próximas fases.

## 2025-09-16 - Fase 2: fits estrutural + variantes faltantes

### Alterações Implementadas
1. Reescrita de `fits` (agora iterativa/estrutural):
  - Removeu chamadas a `best` para medir comprimento.
  - Implementação percorre árvore com pilha (DFS) e consome largura apenas para `Text` e `Line`.
  - `HardLine` retorna imediatamente `false` evitando layout achatado indevido.
2. Otimização em `best`:
  - Cálculo de `flatten(d)` feito uma única vez por `Group` (armazenado em variável `flat`).
3. Implementação das variantes de `Expression` faltantes:
  - `IsError`, `IsNothing`, `Propagate` (impresso como `expr?`).
  - Removido fallback `_` em `Expression` (todas cobertas agora).
4. Implementação das variantes de `Statement` faltantes:
  - `Sequence`, asserts especializados (`AssertTrue`, `AssertFalse`, `AssertEQ`, `AssertNEQ`), `AssertFails`.
  - `TestDef`, `ModTestDef` com prefixos `test` / `modtest`.
  - `TypeDeclaration` (formato tipo bloco: `type Name:` + construtores aninhados + `end`).
  - Removido fallback genérico.
5. Novos testes adicionados:
  - Expressões: `test_is_error_and_propagate_and_is_nothing`.
  - Statements: `test_assert_variants_and_type_declaration`, `test_testdef_and_modtestdef`.
  - Ajustes nas contagens de testes (suite expandida sem regressões).

### Resultado dos Testes Pós Fase 2
- Lib: 37 passed; 0 failed; 7 ignored.
- Bin: 222 passed; 0 failed; 8 ignored.
- Testes externos: inalterados / ok.
- Nenhuma regressão introduzida.

### Observações Técnicas
- `fits` agora O(n) no tamanho do documento para cada decisão de grupo (ainda pode ser multiplicado pelo número de grupos, comportamento esperado do algoritmo clássico).
- Ainda sem cache de `flatten`; custo duplicado potencial se `flatten` reprocessar subárvores muitas vezes — adiar até medir.
- Futuros passos: balancear `join`, remover warnings (`unused_doc_comments`, imports), adicionar helpers ergonômicos, atualizar README.

### Próximos Passos Sugeridos
1. Benchmark simples para medir impacto da reescrita de `fits` vs versão anterior (opcional).
2. Introduzir helpers (`space`, `softline`, `punctuate`).
3. Atualizar README com mudanças e exemplos das novas variantes.
4. Limpeza de warnings (convert docs internos para `//`).
5. (Se necessário) cache/memo de `flatten`.

---

## 2025-09-16 - Fase 3: Instrumentação, join balanceado, cache opcional de flatten

### Objetivos
1. Medir custo real de `flatten` versus `fits`/`Group`.
2. Introduzir `join_balanced` para evitar árvores `Concat` extremamente left-deep.
3. Implementar cache opcional de `flatten` (feature `pp-flatten-cache`) e avaliar redução de chamadas.

### Alterações
- `Cargo.toml`: adicionadas features `pp-profile` e `pp-flatten-cache`.
- `pretty_print.rs`: counters thread-local (`FLATTEN_COUNT`, `FITS_COUNT`, `BEST_GROUP_COUNT`), função `pp_profile_report()`.
- `pretty_print.rs`: nova função pública `join_balanced`.
- `pretty_print.rs`: memo opcional de `flatten` usando `HashMap<usize, Rc<Doc>>` (chave = ponteiro de `Rc`).
- `examples/pp_bench.rs`: benchmark sintético comparando construção linear vs balanceada e efeito do cache.

### Resultados (sem cache `pp-flatten-cache`, somente `pp-profile`)
```
N=50  linear flatten=295  fits=1 groups=1
N=100 linear flatten=1185 fits=3 groups=3
N=200 linear flatten=2975 fits=5 groups=5
N=400 linear flatten=6565 fits=7 groups=7
N=800 linear flatten=13755 fits=9 groups=9
```

### Resultados com cache (`--features pp-profile,pp-flatten-cache`)
```
N=50  linear flatten=199 fits=1 groups=1
N=100 linear flatten=505 fits=3 groups=3
N=200 linear flatten=1005 fits=5 groups=5
N=400 linear flatten=2389 fits=7 groups=7
N=800 linear flatten=4613 fits=9 groups=9
```
Redução aproximada de 32% a 66% nas chamadas de `flatten` conforme o tamanho cresce.

### Observações
- `fits_calls` e `groups` crescem linearmente (esperado); gargalo principal era recomputar `flatten`.
- `join_balanced` evita formação de listas `Concat` skewed; beneficiará casos reais com muitas entradas.
- Cache permanece atrás de feature para não penalizar casos simples (hash map + ponteiro).

### Próximos Passos Propostos
1. Helpers ergonômicos (`space`, `softline`, `punctuate`).
2. Medição de tempo (feature `pp-timing`) se performance virar prioridade.
3. Limpeza de warnings (doc comments e reexports não utilizados sob certas builds).
4. README: seção Performance e Flags.
5. Considerar interning estrutural futuro (armazenar forma flatten dentro de `Doc::Group`).

---

## 2025-09-16 - Fase 4: Helpers, README, Cleanup

### Alterações
- Helpers adicionados: `space`, `softline`, `softline_or_space`, `punctuate`, `hardline_if_nonempty`.
- Substituição parcial em `pretty_expressions.rs` (FuncCall, ListValue, Constructor) usando `punctuate` / `space`.
- Remoção da função `join` antiga e limpeza de doc comments internos inválidos (convertidos para `//`).
- README atualizado com seção "Pretty Printing" detalhando algebra, helpers e features (`pp-profile`, `pp-flatten-cache`).

### Resultados Testes
- Lib: 37 pass, 0 fail, 7 ignored.
- Bin: 222 pass, 0 fail, 8 ignored.
- Externos: 6 pass, 0 fail, 9 ignored.

### Warnings Restantes
- Reexports amplos (`pub use pretty_*::*`) geram warnings em alguns contextos; mantidos para ergonomia.
- Alguns imports em `pretty_statements.rs` ainda podem ser podados (baixo impacto).

### Próximos Passos Potenciais
1. `pp-timing` (medir duração real) + macro de benchmark interna.
2. `Display` para `Doc` e/ou debug estruturado.
3. Unificar `punctuate` com construção balanceada automática para N grande.
4. Adicionar guidelines de estilo de formatação em CONTRIBUTING.
5. Revisar se devemos aplicar `#[allow(unused_imports)]` seletivo ou introduzir um `prelude`.

---
## 2025-09-16 - Fase 5: Simplificações finais e polimento

### Alterações
- Removido helper redundante `softline_or_space` (mantido apenas `softline`).
- Implementado `Display` para `Doc` (usa largura fornecida via `Formatter` ou 80 por default).
- Adicionados testes de largura mínima (`tests/pretty_print_edge_tests.rs`) cobrindo listas, chamadas de função e construtores com `width=1..4`.
- Limpeza de warnings: removidos imports de teste não utilizados em `pretty_statements.rs` e aplicados `#[allow(unused_imports)]` em reexports de `mod.rs` para manter API ergonômica sem ruído.
- README atualizado (remoção de `softline_or_space`, nova nota sobre `Display`).

### Resultado de Testes
- Suite permanece verde: Lib 37/0, Bin 222/0, Edge tests novos 3/0.

### Observações
- Warnings restantes agora intencionais (reexports amplos). Poderemos futuramente avaliar conversão para um `prelude` dedicado.
- Edge tests confirmam comportamento determinístico em larguras reduzidas (quebras forçadas conforme esperado).

### Próximos Itens (eventual)
1. `fill` combinator / alinhamento avançado.
2. Feature `pp-timing` (bench de tempo real) se performance se tornar prioridade de produto.
3. Balanceamento automático em `punctuate` para grandes coleções.
4. Documentar guidelines de estilo em CONTRIBUTING.
5. Possível interning interno da forma flatten em `Group` (lazy) para eliminar cache externo.

---

## 2025-09-19 - Fase 6: Observabilidade (pp-timing) e Robustez (Início)

### Objetivo Geral da Fase 6
Fornecer bases de robustez e observabilidade antes de introduzir novos combinadores e suporte a comentários. A estratégia é implementar primeiro ferramentas de medição (tempo), depois otimizações estruturais (punctuate adaptativo), seguida de novos combinadores (`fill`), ergonomia (comentários, prelude) e documentação/testes.

### Alteração 1: Feature `pp-timing`
Adicionada feature opcional em `Cargo.toml`:
```
[features]
pp-timing = []
```

Componentes introduzidos em `pretty_print.rs` (sob `#[cfg(feature = "pp-timing")]`):
1. `PrettyMetrics` – struct contendo:
  - `width`, `rendered_len`, `duration_ns`.
  - Contadores opcionais (`flatten_calls`, `fits_calls`, `group_decisions`) preenchidos somente se `pp-profile` também estiver ativo.
  - `output` já materializado para evitar recomputar.
2. `pretty_with_metrics(width, &doc)` – wrapper que:
  - Reseta contadores (se `pp-profile`) para produzir métricas relativas à chamada.
  - Mede `Instant::now()` antes/depois da chamada padrão `pretty`.
  - Retorna `PrettyMetrics` preenchido.

### Racional
- Evita overhead por padrão ao isolar em feature.
- Facilita comparação entre estratégias futuras (ex: adaptive punctuate vs linear, com e sem cache de flatten).
- Integra-se naturalmente à instrumentação já existente (`pp-profile`).

### Próximas Alterações Planejadas
1. Punctuate adaptativo: uso automático de `join_balanced` quando o número de elementos exceder limite configurado (const interna), mantendo `punctuate` como API estável.
2. Combinador `fill`: layout compacto de sequências curtas com quebra inteligente quando ultrapassa largura.
3. Comentários: `line_comment("// foo")`, `block_comment("/* multi */")` com preservação e normalização de quebras (`HardLine`).
4. Módulo `prelude`: reexports mínimos (ex: `Doc`, `text`, `line`, `group`, `nest`, `space`, `punctuate`, `fill`, `line_comment`, `block_comment`) reduzindo necessidade de glob broad.
5. Testes adicionais e atualização de README (seção Robustez) consolidando guidelines de uso.

### Estado de Testes Pós Alteração
- Nenhuma regressão – suite completa permanece verde (mesmos números da Fase 5). Feature `pp-timing` adicionada sem ativação padrão (não altera caminho crítico).

---

### Alteração 2: `punctuate` Adaptativo

Implementado limiar interno `PUNCTUATE_BALANCE_THRESHOLD = 8` em `pretty_print.rs`.

- Comportamento: para listas com mais de 8 elementos, `punctuate` delega para `join_balanced`, reduzindo profundidade de árvore e potencial número de nós visitados em decisões de layout.
- API: assinatura inalterada; mudança é totalmente transparente para chamadores existentes.
- Justificativa do limiar: valor pequeno suficiente para ganharmos equilíbrio cedo sem custo extra perceptível em casos comuns (<= 8 elementos) onde implementação linear é marginalmente mais simples.
- Próximos passos relacionados: quando adicionarmos `fill`, poderemos reutilizar `punctuate` adaptativo em construções de listas mais complexas.

Impacto esperado (qualitativo): em coleções grandes, menor cadeia left-deep de `Concat` => menos stack frames lógicos durante `fits` e `best`, principalmente antes de otimizações futuras (ex: interning de flatten). Métricas quantitativas poderão ser coletadas com `pp-timing + pp-profile` em benchmark sintético futuro (planejado após introdução de `fill`).

Estado de testes: sem regressões após modificação (rodaremos novamente ao final do bloco de mudanças agrupadas desta fase).

---

### Alteração 3: Combinador `fill` (versão básica)

Implementado `fill(docs: &[Rc<Doc>]) -> Rc<Doc>` em `pretty_print.rs`.

Abordagem:
- Construção reversa: para sequência `[d1, d2, d3, d4]` produz cadeia de `group(concat(d1, group(concat(d2, group(concat(d3, d4))))))`.
- Cada `group` tenta manter seu par local na mesma linha; se não couber, quebra antes do segundo elemento.
- Resultado é uma aproximação simples do comportamento clássico de *fill* (que em versões completas alterna espaço vs quebra de linha para maximizar densidade). Esta versão não faz heurística de “melhor arranjo global”, mas já fornece benefício incremental.

Limitações conhecidas / Futuro:
- Não colapsa múltiplos `line()` seguidos (depende da estrutura fornecida).
- Não faz coalescing inteligente de espaços quando misturado com `line()` — comportamento aceito nesta fase.
- Possível evolução: implementar variação que aceita um slice de pares `(doc, breakable)` ou gera dinamicamente `line()` onde apropriado.

Testes adicionados em `tests/pretty_print_tests.rs`:
1. `test_fill_keeps_items_on_same_line_when_possible` – garante linha única quando cabe.
2. `test_fill_breaks_progressively` – valida quebra incremental sob largura reduzida e layout plano sob largura ampla.

Estado de testes: suite completa permanece verde após inclusão.

---

### Alteração 4: Helpers de Comentário e Prelude (parcial)

Itens implementados:
1. `line_comment` – normaliza entrada removendo prefixo `//` redundante e trims finais; não adiciona quebra final.
2. `line_comment_ln` – variante que anexa `HardLine` (para uso direto em sequências de statements / cabeçalhos).
3. `block_comment` – suporte multi-linha canônico:
  - Entrada de linha única => `/* body */`.
  - Entrada multi-linha => formato com `/*`, linhas prefixadas por ` * ` e fechamento ` */` em linha separada.
  - Remove delimitadores externos já presentes para evitar duplicação.
  - Normaliza remoção de espaços vazios no início/fim e colapsa linhas vazias externas.
  - Usa `HardLine` entre linhas para garantir que nunca se una acidentalmente a código subsequente em modo flatten.
4. Módulo `prelude` criado: reexports curados (constructors básicos, helpers estáveis, combinadores (`fill`, `punctuate`, `join_balanced`), comentários). Excluídos detalhes internos (`flatten`, `best`, `fits`) e instrumentação opcional – somente exposta sob features.
5. Testes adicionados em `tests/pretty_print_tests.rs` cobrindo casos: linha simples, `line_comment_ln`, bloco simples, bloco multi-linha com espaçamentos irregulares.

Racional:
- Comentários são parte inevitável da formatação de código; fornecer helpers evita reimplementações pontuais.
- Multi-linha padronizado reduz variação de estilo antes de termos uma fase de *formatter* semântico (ex: detectar doc comments vs comentários normais).
- Prelude diminui necessidade de importações amplas (`use pretty_print::pretty_print::*;`) isolando API estável.

Possíveis evoluções:
- `doc_comment` distinto para futuros blocos de documentação.
- Preservação de alinhamento interno (não apenas trim) para blocos ASCII art – optamos por não preservar agora para manter simplicidade.
- Integração com parser futuro para round-trip (comentário -> token -> AST -> pretty -> idempotente).

Estado de testes pós alteração:
- Suite permanece verde; novos testes de comentário aprovados.

Próximos passos imediatos:
1. Adicionar testes adicionais (stress) para punctuate adaptativo em listas > 8.
2. Métrica sintética (opcional) registrando diferença de profundidade ou contagem de grupos.
3. Documentar seção de Robustez (README/Docs) consolidando uso de features.

---
### Notas Pós Testes Stress (2025-09-19)

Adicionados testes:
- `test_punctuate_adaptive_large_list_layout_variation`: valida comportamento linear vs quebrado sob diferentes larguras em lista de 15 elementos.
- `test_fill_with_mixed_length_items`: confirma quebra progressiva quando item longo força expansão.
- `test_comments_integrated_sequence`: integração comentário + bloco agrupado.
- `test_timing_feature_compiles_and_returns_metrics` (gated): garante interface estável sob feature `pp-timing`.

Observações qualitativas (sem bench formal ainda):
- Lista de 15 elementos rende uma única linha com largura 200 (nenhuma `\n`), indicando flatten correto via `join_balanced`.
- Em largura 10, múltiplas quebras confirmam fallback para layout multiline esperado.
- `fill` em largura 15 quebra após segmento longo, preservando densidade máxima inicial.

Próximo (planejado): seção "Robustez" em documentação descrevendo quando ativar: `pp-profile`, `pp-timing`, `pp-flatten-cache` e limites adaptativos (`PUNCTUATE_BALANCE_THRESHOLD`).

---
### Reintrodução do `prelude` (2025-09-19)

Após avaliação, reintroduzido módulo `prelude` com reexports mínimos:
- Racional: oferecer superfície estável para consumidores (tests / exemplos) sem expor internals (`flatten`, `best`, `fits`).
- Estrutura documental: criado `docs/PRELUDE.md` descrevendo política de estabilidade, escopo, FAQ e processo de adição.
- Referência curta mantida em `ROBUSTEZ_PRETTY_PRINT.md` (link para PRELUDE).

Critério de inclusão definido:
1. Ter testes cobrindo comportamento.
2. Não ser experimental / sujeito a alteração imediata.
3. Entrar via commit separado após validação.

Itens não incluídos permanecem considerados instáveis ou internos.

Impacto: nenhum efeito funcional ou de performance; apenas organização de superfície pública.

---


