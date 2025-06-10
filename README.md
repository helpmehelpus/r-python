# RPython 🚀

[![Rust](https://img.shields.io/badge/rust-stable-orange.svg)](https://www.rust-lang.org/)
[![GitHub issues](https://img.shields.io/github/issues/UnBCIC-TP2/r-python)](https://github.com/UnBCIC-TP2/r-python/issues)
[![CI Status](https://img.shields.io/github/actions/workflow/status/UnBCIC-TP2/r-python/ci.yml?branch=main&label=ci-status&color=blue)](https://github.com/UnBCIC-TP2/r-python/actions)


Um compilador experimental implementado em Rust que interpreta uma linguagem com sintaxe similar ao Python. Este projeto foi desenvolvido como ferramenta de aprendizado para conceitos de técnicas de programação.

## 📋 Sobre o Projeto

RPython é um projeto educacional que visa:
- Implementar um compilador funcional em Rust
- Explorar conceitos fundamentais de técnicas de programação
- Criar uma linguagem com sintaxe amigável similar ao Python

## 📚 Documentação

Para uma compreensão mais profunda dos componentes do projeto, consulte nossa documentação técnica:

- **[Environment Module](docs/environment.md)** - Sistema de gerenciamento de escopo lexical com tabela de símbolos para variáveis e funções. Implementa uma pilha de escopos com resolução adequada da cadeia de escopo.

- **[Parser Component](docs/parser.md)** - Componente de análise sintática que transforma código fonte em Árvore de Sintaxe Abstrata (AST). Usa a biblioteca `nom` e segue um design modular com funcionalidades especializadas para expressões, tipos e declarações.

- **[Type Checker Module](docs/type_checker.md)** - Sistema de verificação de tipos estática que analisa expressões e declarações para garantir segurança de tipos em tempo de compilação. Implementa regras de tipagem bem definidas para todos os construtos da linguagem. *(Em desenvolvimento)*

## 🤝 Contribuindo

Adoraríamos contar com sua contribuição! Por favor, leia nossos guias de contribuição:
- [Guia de Contribuição em Português](CONTRIBUTING_pt.md)
- [Contributing Guidelines in English](CONTRIBUTING_en.md)

## 🚀 Começando

### Pré-requisitos

- Rust (última versão estável)
- Cargo (gerenciador de pacotes do Rust)
- Git (para clonar o repositório)
- Editor de texto ou IDE de sua preferência

### Configuração do Ambiente

1. Primeiro, instale o Rust e Cargo usando rustup:
   - Windows: 
     - Baixe e execute rustup-init.exe em https://rustup.rs
     - Siga as instruções do instalador
   - Linux/macOS:
     - Abra o terminal e execute:
     ```bash
     curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
     ```
     - Siga as instruções do instalador
     - Reinicie seu terminal após a instalação
