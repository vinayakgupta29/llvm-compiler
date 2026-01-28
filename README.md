# VKS Compiler

A functional programming language compiler with OCaml frontend and Rust/LLVM backend.

## Overview

VKS is a statically-typed functional programming language featuring:
- Haskell-like syntax with pattern matching and guard clauses
- Rust-like ownership model (single-binding, no borrowing)
- First-class functions with currying and composition
- Algebraic data types (structs and sum types)
- Module system with hierarchical organization
- Type classes for polymorphism

## Architecture

```
┌─────────────────┐
│  VKS Source     │
│   (.vks)        │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ OCaml Frontend  │
│ - Lexer/Parser  │
│ - Type Checker  │
│ - HIR Gen       │
└────────┬────────┘
         │ (JSON HIR)
         ▼
┌─────────────────┐
│  Rust Backend   │
│ - LLVM IR Gen   │
│ - Optimization  │
│ - Code Gen      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Native Binary   │
│   (x86-64)      │
└─────────────────┘
```

## Building from Source

### Prerequisites
- OCaml >= 4.14 with opam
- Rust >= 1.70
- LLVM >= 14
- Menhir parser generator

### Build Steps

```bash
# Install OCaml dependencies
opam install dune menhir yojson

# Build the compiler
./build.sh

# This creates:
# - vks (compiler CLI)
# - vksi (REPL)
# - std/ (standard library)
```

## Usage

### Compile and run a single file
```bash
vks run hello.vks
```

### Build a project
```bash
vks build main.vks -o myapp
./myapp
```

### Initialize a new project
```bash
vks init myproject
cd myproject
vks build .
```

### Interactive REPL
```bash
vksi
> a = 5
> a + 3
8
> :q
```

## Language Examples

### Hello World
```vks
app Main {
    @start() {
        print("Hello, World!")
    }
}
```

### Functions and Pattern Matching
```vks
func factorial(n: Int) -> Int {
    | n == 0 => 1
    | otherwise => n * factorial(n - 1)
}
```

### Custom Types
```vks
data Vec2i {
    x: Int = 0,
    y: Int = 0,
}

(+)(infixr 5)(a, b: Vec2i) -> Vec2i {
    => Vec2i { x = a.x + b.x, y = a.y + b.y }
}
```

## Project Structure

```
llvm-compiler/
├── ocaml_implemented_frontend/ # OCaml frontend
│   └── frontend/
│       ├── lexer.mll          # Lexer specification
│       ├── parser.mly         # Parser grammar
│       ├── ast.ml             # AST definitions
│       ├── type_checker.ml   # Type checking logic
│       └── hir_gen.ml         # HIR generation (JSON)
├── compiler/                  # Rust backend
│   └── src/
│       ├── backend/           # Codegen and LLVM interaction
│       └── frontend/          # Rust-based frontend parity (in progress)
├── std/                       # Standard library (Prelude, etc.)
├── examples/                  # Example VKS programs
└── build.sh                   # Unified build script
```

## License

See LICENSE file for details.

## Contributing

Contributions are welcome! Please see CONTRIBUTING.md for guidelines.
