#!/bin/bash

function install_languages () {
  # AWK
  npm i -g "awk-language-server@>=0.5.2"
  # Bash
  npm i -g bash-language-server
  # CSS, HTML, JSON
  npm i -g vscode-langservers-extracted
  # Docker
  npm install -g dockerfile-language-server-nodejs
  # Go
  go install golang.org/x/tools/gopls@latest          # LSP
  go install github.com/go-delve/delve/cmd/dlv@latest # Debugger
  go install golang.org/x/tools/cmd/goimports@latest  # Formatter
  # Haskell
  brew install ghc haskell-language-server
  # Typescript
  npm install -g typescript typescript-language-server
  # Jsonnet
  go install github.com/grafana/jsonnet-language-server@latest
  # Java
  brew install jdtls
  # Markdown
  brew install marksman
  # Scala
  brew install coursier/formulas/coursier
  cs setup
  coursier install metals
  # SQL
  npm i -g sql-language-server
  # Prisma
  npm install -g @prisma/language-server
  # Python
  pip install -U 'python-lsp-server[all]' pyright ruff-lsp black mypy
  # Rust
  rustup component add rust-analyzer
  # TOML
  cargo install taplo-cli --locked --features lsp
  # YAML
  brew install yaml-language-server
  # Zig
  brew install zls
}

install_languages
