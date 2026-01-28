# Language Primitives for LLVM

## Core Types
- Int -> i64
- Uint -> i64 unsigned
- Float -> double
- Bool -> i1
- Char -> i8
- Nil -> void
- String -> struct { i8* ptr; i64 len }

## Operators
- Arithmetic: + - * / ^(exp but only for integers) `_/_` (floor division)
- Logical: && || !(logical not) == `=/=`(not eq) >= <=
- Bitwise: .& .| .^ .~

## Functions & Procedures
- func: LLVM function returning type
- proc: LLVM function returning void
- Currying: LLVM function pointers + closure struct

## Memory & Scope
- Single-binding: immutable stack allocation
- Scope lifetime: stack-based local variables
- Module hierarchy: symbol table with fully qualified names

## Composite Types
- Structs: LLVM struct { ... }
- Sum types/Enums: LLVM struct { tag:i32, union value }
- Vectors/Lists: heap-allocated array or linked-list (more to be looked in haskell for array and lists)
- Sets: heap-allocated array (unique enforced in prelude)

## Prelude Responsibilities
- Currying / partial application
- Typeclasses & operator overloading
- Pattern matching & guards
- Module resolution helpers
- Collection utilities
- Bitwise string ops
- Default constructors / zero values
- High-level math functions (exp, sinh, etc.)

## Compliations and Final Binary Responsibilities
- No dependency on the C run time or the standard C library or any C library for that matter. 
- C-compatible ABI will be done in future versions but not here.
- The comiler binary will be shipped as a Single binary but some modules would be provided such as Prelude, Types.Basic, Math.Basic (definting operators and all). And these will be linked by default regardless of the user putting them in the using directive or not.
- Also the basic release ships with the compiler  + a simple package manager + some libs (including the ones in previous rule and the complete versions of them. Like complete math module, complete Types Modules, etc)
```
vks init . // Inits project default app in an empty dir only.
vks init . --type=module
vks init name // creates a dir with the name at the current level and creates a default project in that dir with the name
vks build . // This loads the current dir flat no children dir are looked up and look for the root file specified in the config file and other modules that it import
vks build main.vks 
vks build main.vks -o outputBin
vks run <file name> // for single files and it compiles and runs them doesn't produce a binary file in the FS.

```
- The init arg if called without any arg turns interactive and asks the user to input project name and select 1 or 2 (app|module) for the project.
- currently supported platform is linux.
- All error for now are compile time error as there are no effects in the language. 
- compile time errors must be of the format 
<path to errored file>line:col <Error msg>

## Toolchain to use

| Stage             | Tool / Approach                          | Notes / Rationale                                                                                                    |
| ----------------- | ---------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| Lexing & Parsing  | Implement in Rust  | Grammar written in textbook-style LR(1) / EBNF; separate lexer; reads `.vks` source → typed AST                      |
| Semantic Analysis | Implement in Rust                  | Functional traversal for type checking, pattern matching, and scope resolution                                       |
| Intermediate IR   | Implement in Rust | Typed AST lowered to a core functional IR; serialized to communicate with Rust backend                               |
| IR Generation     | **Rust (inkwell / llvm-sys)**            | Rust reads serialized HIR → generates LLVM IR using safe LLVM bindings; supports primitives, functions, control flow |
| Optimizations     | **LLVM passes in Rust**                  | Constant folding, inlining, dead code elimination; reuse LLVM optimization infrastructure                            |
| Code Generation   | **LLVM backend via Rust**                | Target x86-64, ARM, or other platforms; generate native code or JIT                                                  |
| Prelude / Runtime | **Custom runtime (Rust / LLVM)**         | Implements functional language features (e.g., GC, pattern matching, closures)                                       |
| REPL / Testing    | **Optional: LLVM JIT via Rust**          | For early experimentation and interactive evaluation; reads HIR → LLVM → executes                                    |

## Release
- the output should be a final binary that can be ran and it should be self contained. not saperate binaries for frontend and backend.
- give a build.sh file which can be ran after cloning the repo and build the compiler from the source code and the output alongwith the std folder is packaged in the release folder which can then be archived as a tarball and distributed as release.
- Give an PKGBUILD for AUR and another install.sh which can be called like other install scripts and installed on any other linux version. `https://<githuburl>/install.sh | sh` and it installs like a normal package for that OS.
- `vks` without any args prints version info and help

### REPL rules
Also ship with a REPL binary `vksi` doesn't take any args.
```
:h - Help
:l <vks file name> // loads a vks file
:q quit
:m module name // loads a std module other than prelude
```
- Expressions are evaluated when pressed enter.
- No multi line support only single line lambda expressions are allowed to defined for multiline functions load a file. 
- By default REPL only loads Prelude
```// allowed
>a = 5
>a
//print's it's value
> a = (x,y)=> x+y
>a(5,3)// prints the result on enter.
> c = a(5,4) // stores the result in the name c doesn't prints it yet.
>a=a(5,3) // first executes a as a function then stores the result in a as a value name/variable
> b = a(5,3) // throws an error as a is a variable now and not a name.
```
- Name reallocation is allowed only in REPL. and that too for user defined names. They can't re-assign if a name is exported by a Module or is defined as a keyword or is a member of Prelude.
