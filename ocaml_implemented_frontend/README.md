# VKS OCaml Frontend

This directory contains the OCaml implementation of the VKS language frontend. It is responsible for lexing, parsing, type checking, and generating the Higher-level Intermediate Representation (HIR) in JSON format, which is then consumed by the Rust backend.

## 📋 Features
- **Lexer**: Tokenizes VKS source files.
- **Parser**: generates an Abstract Syntax Tree (AST) using Menhir.
- **Type Checker**: Validates types and scope.
- **HIR Generator**: Produces a JSON representation of the program.

## 🛠️ Dependencies
To build and run the OCaml frontend, you need the following dependencies:

- **OCaml** (>= 4.14)
- **opam** (OCaml package manager)
- **dune** (Build system)
- **menhir** (Parser generator)
- **yojson** (JSON library)

Install them via opam:
```bash
opam install dune menhir yojson
```

## 🚀 How to Compile and Run

### Build the project
From the `ocaml_implemented_frontend/frontend` directory, run:
```bash
dune build
```

### Run the frontend
You can execute the frontend on a VKS source file:
```bash
dune exec -- ./main.exe <path_to_file.vks> [-o output.hir.json]
```

Example:
```bash
dune exec -- ./main.exe ../../examples/hello.vks
```

## 📂 Project Structure
- `ast.ml`: Abstract Syntax Tree definitions.
- `lexer.mll`: OCamlLex lexer specification.
- `parser.mly`: Menhir parser grammar.
- `type_checker.ml`: Semantic analysis and type checking.
- `hir.ml`: HIR definitions and conversion logic.
- `hir_gen.ml`: JSON serialization logic.
- `main.ml`: Entry point and CLI handling.
- `dune`: Build configuration.
- `vks_frontend.opam`: Package definition and dependencies.
