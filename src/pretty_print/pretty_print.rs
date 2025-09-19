// src/pretty_print/pretty_print.rs

#[cfg(feature = "pp-profile")]
use std::cell::Cell;
#[cfg(feature = "pp-flatten-cache")]
use std::cell::RefCell;
#[cfg(feature = "pp-flatten-cache")]
use std::collections::HashMap;
use std::rc::Rc;
#[cfg(feature = "pp-profile")]
thread_local! {
    static FLATTEN_COUNT: Cell<u64> = Cell::new(0);
    static FITS_COUNT: Cell<u64> = Cell::new(0);
    static BEST_GROUP_COUNT: Cell<u64> = Cell::new(0);
}
#[cfg(feature = "pp-flatten-cache")]
thread_local! {
    static FLATTEN_CACHE: RefCell<HashMap<usize, Rc<Doc>>> = RefCell::new(HashMap::new());
}

/// Representa a estrutura abstrata de um documento formatável.
/// Baseado na abordagem algébrica de Philip Wadler, cada variante é uma instrução
/// de layout que será interpretada pelo motor de renderização.
/// O uso de `Rc<Doc>` permite o compartilhamento eficiente da estrutura de dados
/// sem a necessidade de múltiplas cópias.
#[derive(Clone)]
pub enum Doc {
    /// Um documento vazio. Atua como o elemento neutro para a concatenação.
    Nil,
    /// Um bloco de texto atômico que nunca deve ser quebrado em múltiplas linhas.
    Text(String),
    /// Representa uma possível quebra de linha. Dentro de um `Group`, pode ser
    /// renderizado como um espaço (se o grupo couber em uma linha) ou como uma
    /// quebra de linha real.
    Line,
    /// Uma quebra de linha obrigatória. Sempre será renderizada como uma nova linha,
    /// independentemente do contexto.
    HardLine,
    /// Adiciona um nível de indentação ao documento aninhado. O `usize` representa
    /// o número de espaços a serem adicionados.
    Nest(usize, Rc<Doc>),
    /// Concatena dois documentos sequencialmente.
    Concat(Rc<Doc>, Rc<Doc>),
    /// O operador mais importante para layouts flexíveis. Oferece ao motor
    /// uma escolha: renderizar o documento contido em uma única linha (achatado)
    /// ou com quebras de linha (se não couber).
    Group(Rc<Doc>),
}

/// Um trait que define o contrato para converter uma estrutura de dados (como um nó da AST)
/// em sua representação de `Doc`. Isso desacopla o motor de renderização das
/// estruturas de dados específicas da linguagem.
pub trait ToDoc {
    fn to_doc(&self) -> Rc<Doc>;
}

// --- Funções Construtoras ---
// Estas funções servem como uma API mais limpa e ergonômica para criar `Doc`s.

/// Cria um documento vazio (`Doc::Nil`).
pub fn nil() -> Rc<Doc> {
    Rc::new(Doc::Nil)
}
/// Cria um documento de texto (`Doc::Text`).
pub fn text<S: Into<String>>(s: S) -> Rc<Doc> {
    Rc::new(Doc::Text(s.into()))
}
/// Cria uma quebra de linha suave (`Doc::Line`).
pub fn line() -> Rc<Doc> {
    Rc::new(Doc::Line)
}
/// Cria uma quebra de linha dura (`Doc::HardLine`).
pub fn hardline() -> Rc<Doc> {
    Rc::new(Doc::HardLine)
}
/// Um único espaço.
pub fn space() -> Rc<Doc> {
    text(" ")
}
/// Alias semântico para `line` (quando quisermos sinalizar quebra suave explicitamente).
pub fn softline() -> Rc<Doc> {
    line()
}
/// Quebra suave que vira espaço quando flatten, ou nada se vazio (aqui reusa `line`).
// Removido softline_or_space para evitar duplicidade semântica.
/// Aninha um documento com um nível de indentação (`Doc::Nest`).
pub fn nest(i: usize, doc: Rc<Doc>) -> Rc<Doc> {
    Rc::new(Doc::Nest(i, doc))
}
/// Agrupa um documento, permitindo layouts flexíveis (`Doc::Group`).
pub fn group(doc: Rc<Doc>) -> Rc<Doc> {
    Rc::new(Doc::Group(doc))
}
/// Concatena dois documentos (`Doc::Concat`).
pub fn concat(d1: Rc<Doc>, d2: Rc<Doc>) -> Rc<Doc> {
    Rc::new(Doc::Concat(d1, d2))
}

/// Concatenação balanceada de uma sequência de documentos com um separador já pré-construído.
/// Evita formar listas extremamente left-deep de `Concat`, reduzindo profundidade de árvore.
pub fn join_balanced(sep: Rc<Doc>, docs: &[Rc<Doc>]) -> Rc<Doc> {
    fn build(sep: &Rc<Doc>, slice: &[Rc<Doc>]) -> Rc<Doc> {
        match slice.len() {
            0 => nil(),
            1 => slice[0].clone(),
            2 => concat(slice[0].clone(), slice[1].clone()),
            n => {
                let mid = n / 2;
                let left = build(sep, &slice[..mid]);
                let right = build(sep, &slice[mid..]);
                concat(left, right)
            }
        }
    }
    if docs.is_empty() {
        return nil();
    }
    // Intercala separador manualmente antes de balancear.
    let mut with_sep: Vec<Rc<Doc>> = Vec::with_capacity(docs.len() * 2 - 1);
    for (i, d) in docs.iter().enumerate() {
        if i > 0 {
            with_sep.push(sep.clone());
        }
        with_sep.push(d.clone());
    }
    build(&sep, &with_sep)
}

/// Insere separador entre documentos (versão simples linear) – similar a `punctuate`.
pub fn punctuate(sep: Rc<Doc>, docs: &[Rc<Doc>]) -> Rc<Doc> {
    if docs.is_empty() {
        return nil();
    }
    // Limiar a partir do qual usamos forma balanceada para reduzir profundidade de árvore.
    const PUNCTUATE_BALANCE_THRESHOLD: usize = 8;
    if docs.len() > PUNCTUATE_BALANCE_THRESHOLD {
        // Reutilizamos join_balanced passando docs diretamente (que já inserirá separadores internamente).
        return join_balanced(sep, docs);
    }
    let mut it = docs.iter();
    let first = it.next().unwrap().clone();
    it.fold(first, |acc, d| concat(acc, concat(sep.clone(), d.clone())))
}

/// Adiciona `hardline` após cada doc se a lista não estiver vazia.
pub fn hardline_if_nonempty(docs: &[Rc<Doc>]) -> Rc<Doc> {
    if docs.is_empty() {
        return nil();
    }
    let mut acc = nil();
    for d in docs {
        acc = concat(acc, concat(d.clone(), hardline()));
    }
    acc
}

/// Combinador `fill` simples para uma sequência de documentos já espaçados (ou cujo
/// separador foi incorporado). A ideia do `fill` é tentar manter o máximo de itens
/// na mesma linha antes de quebrar, diferentemente de um único `group` com `line()`
/// que pode quebrar tudo simetricamente.
///
/// Implementação básica: cria uma cadeia de grupos aninhados onde cada passo tenta
/// colocar o próximo item na mesma linha; se não couber, quebra antes dele.
/// Para um vetor [a, b, c, d] gera algo conceitualmente similar a:
/// group(a ++ group(b ++ group(c ++ d)))
/// Isso permite que as primeiras combinações que caibam fiquem "coladas" e as
/// últimas que não caibam quebrem, produzindo aparência de preenchimento.
pub fn fill(docs: &[Rc<Doc>]) -> Rc<Doc> {
    if docs.is_empty() {
        return nil();
    }
    // Estratégia iterativa: partimos do último e vamos agrupando de trás para frente.
    let mut acc = docs[docs.len() - 1].clone();
    if docs.len() >= 2 {
        for d in docs[..docs.len() - 1].iter().rev() {
            // Concatena d com o acumulado e agrupa, permitindo flatten tentativa local.
            acc = group(concat(d.clone(), acc));
        }
    }
    acc
}

// --- Helpers de Comentários ---
// Objetivo: produzir representações canônicas de comentários para futura
// integração com a formatação de código-fonte. Mantemos implementação simples
// e determinística.

/// Cria um comentário de linha (`// ...`). Remove prefixo redundante `//` do input
/// e descarta quebras de linha finais. Não adiciona `HardLine` no final.
pub fn line_comment<S: AsRef<str>>(s: S) -> Rc<Doc> {
    let raw = s.as_ref();
    // Remove \r/\n finais e espaços à direita.
    let trimmed = raw.trim_end_matches(|c| c == '\n' || c == '\r').trim_end();
    // Remove prefixo // opcional para evitar duplicação.
    let body = if let Some(stripped) = trimmed.strip_prefix("//") {
        stripped.trim_start()
    } else {
        trimmed
    };
    if body.is_empty() {
        text("//")
    } else {
        // Construímos `// ` + body sem quebrar.
        concat(text("// "), text(body))
    }
}

/// Versão de `line_comment` que força uma quebra de linha dura após o comentário.
pub fn line_comment_ln<S: AsRef<str>>(s: S) -> Rc<Doc> {
    concat(line_comment(s), hardline())
}

/// Cria um comentário de bloco. Para entrada de uma linha gera `/* body */`.
/// Para múltiplas linhas produz estilo:
/// /*\n * linha1\n * linha2\n */
/// As quebras internas são sempre `HardLine` para estabilidade.
pub fn block_comment<S: AsRef<str>>(s: S) -> Rc<Doc> {
    let raw = s.as_ref();
    // Remove delimitadores caso o chamador tenha passado já com /* */.
    let body = raw.trim();
    let body = body
        .strip_prefix("/*")
        .map(|r| r.trim_start())
        .unwrap_or(body);
    let body = body
        .strip_suffix("*/")
        .map(|r| r.trim_end())
        .unwrap_or(body);
    let body = body.trim_matches(|c: char| c == '\n' || c == '\r');
    if body.lines().count() <= 1 {
        // Comentário de uma linha.
        if body.is_empty() {
            return text("/* */");
        }
        return concat(concat(text("/* "), text(body)), text(" */"));
    }
    // Multi-linha.
    let mut docs: Vec<Rc<Doc>> = Vec::new();
    docs.push(text("/*"));
    docs.push(hardline());
    let mut lines: Vec<&str> = body.lines().collect();
    // Remove linhas vazias no início/fim para estabilidade.
    while lines.first().map(|l| l.trim().is_empty()).unwrap_or(false) {
        lines.remove(0);
    }
    while lines.last().map(|l| l.trim().is_empty()).unwrap_or(false) {
        lines.pop();
    }
    for (i, line_str) in lines.iter().enumerate() {
        // Normalizamos removendo espaços iniciais excessivos e espaços finais.
        let line_body = line_str.trim_end();
        let line_body = line_body.trim_start();
        docs.push(concat(text(" * "), text(line_body)));
        docs.push(hardline());
        // A última quebra será substituída abaixo por fechamento se não quisermos linha extra.
        if i == lines.len() - 1 { /* handled after loop */ }
    }
    docs.push(text(" */"));
    // Concatena sequencialmente (número pequeno de peças típico de comentários).
    let mut acc = nil();
    for d in docs {
        acc = concat(acc, d);
    }
    acc
}

// Implementa Display para Doc usando largura padrão configurável.
impl std::fmt::Display for Doc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = f.width().unwrap_or(80);
        let s = pretty(width, &Rc::new(self.clone()));
        f.write_str(&s)
    }
}

// --- O Motor de Renderização ---

/// A função principal que renderiza um `Doc` em uma `String`, dada uma largura máxima.
///
/// # Argumentos
/// * `width` - A largura máxima da linha desejada.
/// * `doc` - O documento a ser formatado.
pub fn pretty(width: usize, doc: &Rc<Doc>) -> String {
    let mut out = String::new();
    // Inicia o processo de renderização com a largura máxima, posição inicial 0,
    // e indentação inicial 0.
    best(width as isize, 0, &[(0, doc.clone())], &mut out);
    out
}

/// Métricas detalhadas do processo de pretty-printing (habilitadas com feature `pp-timing`).
#[cfg(feature = "pp-timing")]
#[derive(Debug, Clone)]
pub struct PrettyMetrics {
    /// Largura solicitada.
    pub width: usize,
    /// Número de bytes do resultado final.
    pub rendered_len: usize,
    /// Duração em nanos do processo de renderização.
    pub duration_ns: u128,
    /// (Se `pp-profile` ativo) contadores de chamadas – caso contrário ficam em None.
    pub flatten_calls: Option<u64>,
    pub fits_calls: Option<u64>,
    pub group_decisions: Option<u64>,
    /// Resultado já pronto (String) para evitar recomputar.
    pub output: String,
}

/// Renderiza um documento coletando métricas de tempo e (opcionalmente) contadores.
///
/// Esta função é protegida por feature `pp-timing` para não incorrer overhead em builds padrão.
#[cfg(feature = "pp-timing")]
pub fn pretty_with_metrics(width: usize, doc: &Rc<Doc>) -> PrettyMetrics {
    use std::time::Instant;
    let start = Instant::now();
    // Limpamos contadores se pp-profile estiver ativo para obter valores relativos.
    #[cfg(feature = "pp-profile")]
    {
        FLATTEN_COUNT.with(|c| c.set(0));
        FITS_COUNT.with(|c| c.set(0));
        BEST_GROUP_COUNT.with(|c| c.set(0));
    }
    let output = pretty(width, doc);
    let duration_ns = start.elapsed().as_nanos();
    #[cfg(feature = "pp-profile")]
    let (flatten_calls, fits_calls, group_decisions) = (
        Some(FLATTEN_COUNT.with(|c| c.get())),
        Some(FITS_COUNT.with(|c| c.get())),
        Some(BEST_GROUP_COUNT.with(|c| c.get())),
    );
    #[cfg(not(feature = "pp-profile"))]
    let (flatten_calls, fits_calls, group_decisions) = (None, None, None);
    PrettyMetrics {
        width,
        rendered_len: output.len(),
        duration_ns,
        flatten_calls,
        fits_calls,
        group_decisions,
        output,
    }
}

/// Transforma um `Doc` em sua versão "achatada", onde todos os `Line`s
/// são convertidos em espaços. Usado para verificar se um `Group` cabe em uma única linha.
fn flatten(doc: &Rc<Doc>) -> Rc<Doc> {
    #[cfg(feature = "pp-profile")]
    {
        FLATTEN_COUNT.with(|c| c.set(c.get() + 1));
    }
    #[cfg(feature = "pp-flatten-cache")]
    {
        let key = Rc::as_ptr(doc) as usize;
        if let Some(cached) = FLATTEN_CACHE.with(|m| m.borrow().get(&key).cloned()) {
            return cached;
        }
        let produced = _flatten_no_cache(doc);
        FLATTEN_CACHE.with(|m| {
            m.borrow_mut().insert(key, produced.clone());
        });
        produced
    }
    #[cfg(not(feature = "pp-flatten-cache"))]
    {
        _flatten_no_cache(doc)
    }
}

#[allow(dead_code)]
fn _flatten_no_cache(doc: &Rc<Doc>) -> Rc<Doc> {
    match &**doc {
        Doc::Nil => nil(),
        Doc::Text(_) => doc.clone(),
        Doc::Line => text(" "),      // Quebras de linha suaves viram espaços.
        Doc::HardLine => hardline(), // Preserva HardLine para impedir achatamento indevido.
        Doc::Nest(_, d) => flatten(d), // Indentação é ignorada no modo achatado.
        Doc::Concat(d1, d2) => concat(flatten(d1), flatten(d2)),
        Doc::Group(d) => flatten(d), // Grupos aninhados são recursivamente achatados.
    }
}

/// O coração do algoritmo de renderização. Ele percorre a estrutura do `Doc` e
/// decide qual o melhor layout, escrevendo o resultado na string de saída.
///
/// # Argumentos
/// * `w` - A largura máxima da linha (constante durante a recursão).
/// * `k` - A posição atual do cursor na linha.
/// * `docs` - Uma lista de `(indentação, documento)` a serem processados.
/// * `out` - A `String` de saída que está sendo construída.
///
/// # Retorno
/// A nova posição do cursor `k` após processar os documentos.
fn best(w: isize, mut k: isize, docs: &[(usize, Rc<Doc>)], out: &mut String) -> isize {
    for (indent, doc) in docs {
        match &**doc {
            Doc::Nil => {} // Não faz nada para um documento vazio.
            Doc::Text(s) => {
                // Adiciona o texto à saída e avança o cursor.
                k += s.len() as isize;
                out.push_str(s);
            }
            Doc::Line | Doc::HardLine => {
                // Insere uma quebra de linha e a indentação correta.
                out.push('\n');
                let new_indent_str = " ".repeat(*indent);
                // Reseta a posição do cursor para a nova indentação.
                k = *indent as isize;
                out.push_str(&new_indent_str);
            }
            Doc::Nest(i, d) => {
                // Processa o documento aninhado com um nível de indentação aumentado.
                k = best(w, k, &[(indent + i, d.clone())], out);
            }
            Doc::Concat(d1, d2) => {
                // Processa o primeiro documento, depois o segundo.
                k = best(w, k, &[(*indent, d1.clone())], out);
                k = best(w, k, &[(*indent, d2.clone())], out);
            }
            Doc::Group(d) => {
                // Calcula versão achatada uma única vez.
                let flat = flatten(d);
                #[cfg(feature = "pp-profile")]
                {
                    BEST_GROUP_COUNT.with(|c| c.set(c.get() + 1));
                }
                if fits(w - k, &flat) {
                    k = best(w, k, &[(*indent, flat)], out);
                } else {
                    k = best(w, k, &[(*indent, d.clone())], out);
                }
            }
        }
    }
    k
}

/// Verifica se um documento (tipicamente achatado) cabe em `w` colunas restantes.
/// Esta função implementa o "bounded lookahead": ela para assim que o limite é excedido.
///
/// # Argumentos
/// * `w` - O número de colunas restantes na linha.
/// * `doc` - O documento a ser verificado.
fn fits(w: isize, doc: &Rc<Doc>) -> bool {
    #[cfg(feature = "pp-profile")]
    {
        FITS_COUNT.with(|c| c.set(c.get() + 1));
    }
    if w < 0 {
        return false;
    }

    // Pilha de (doc, largura_restante)
    // Usamos largura_restante global; cada Text consome, demais apenas expandem.
    let mut remaining = w;
    let mut stack: Vec<Rc<Doc>> = vec![doc.clone()];

    while let Some(d) = stack.pop() {
        if remaining < 0 {
            return false;
        }
        match &*d {
            Doc::Nil => {}
            Doc::Text(s) => {
                remaining -= s.len() as isize;
                if remaining < 0 {
                    return false;
                }
            }
            Doc::Line => {
                // Linha suave em modo achatado conta como espaço único.
                remaining -= 1;
                if remaining < 0 {
                    return false;
                }
            }
            Doc::HardLine => {
                // HardLine nunca cabe em uma única linha.
                return false;
            }
            Doc::Nest(_, inner) => stack.push(inner.clone()),
            Doc::Concat(a, b) => {
                // Empilha na ordem inversa para processar 'a' primeiro.
                stack.push(b.clone());
                stack.push(a.clone());
            }
            Doc::Group(g) => {
                // Para fits, sempre consideramos a versão achatada do grupo.
                stack.push(flatten(g));
            }
        }
    }
    true
}

/// Exibe métricas de instrumentação (apenas quando a feature pp-profile estiver ativa).
#[cfg(feature = "pp-profile")]
pub fn pp_profile_report() -> String {
    let flatten = FLATTEN_COUNT.with(|c| c.get());
    let fits = FITS_COUNT.with(|c| c.get());
    let groups = BEST_GROUP_COUNT.with(|c| c.get());
    format!("pp-profile: flatten_calls={flatten} fits_calls={fits} groups={groups}")
}

// (Testes internos removidos - agora consolidados em tests/pretty_print_tests.rs)
