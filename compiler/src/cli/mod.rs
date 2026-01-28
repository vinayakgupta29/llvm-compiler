use std::path::Path;
use std::fs;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "vks")]
#[command(version = "0.1.0")]
#[command(about = "VKS Compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build a VKS project
    Build {
        #[arg(short, long, default_value = "outbin")]
        output: String,
        file: String,
    },
    /// Run a VKS file (WIP)
    Run {
        file: String,
    },
}

pub fn run() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build { output, file } => {
            handle_build(file, output);
        }
        Commands::Run { file } => {
            println!("Run command not yet implemented for Rust frontend: {}", file);
        }
    }
}

fn handle_build(input_file: &str, output_bin: &str) {
    use crate::frontend::{lexer::Token, parser, sema::Sema};
    use crate::backend::codegen::CodeGen;
    use crate::backend::hir::{HIRProgram, HIRModuleMember};
    use logos::Logos;
    use std::process::Command;

    println!("Building {}...", input_file);

    let source = fs::read_to_string(input_file).expect("Failed to read input file");
    
    // 1. Create output directory if it doesn't exist
    // STAGE_DUMP: This code ensures intermediate results are visible to the user
    let output_dir = Path::new("output");
    if !output_dir.exists() {
        fs::create_dir(output_dir).expect("Failed to create output directory");
    }

    // 2. Lexing & Parsing
    println!("Parsing {}...", input_file);
    let tokens: Vec<Token> = Token::lexer(&source).map(|t| t.unwrap_or(Token::Not)).collect(); // Use Not as a temporary error marker if we don't have Error
    let ast = parser::parse(tokens).expect("Parse error");
    
    // 3. Semantic Analysis
    println!("Type checking...");
    let mut sema = Sema::new();
    let hir = sema.analyze(ast).expect("Type check error");

    // STAGE_DUMP: Save HIR JSON to output folder
    let hir_json = serde_json::to_string_pretty(&hir).unwrap();
    fs::write(output_dir.join("stage.hir.json"), hir_json).expect("Failed to dump HIR");

    // 4. IR Generation
    println!("Generating LLVM IR...");
    let mut codegen = CodeGen::new();
    match &hir {
        HIRProgram::App { members, .. } => {
            for m in members {
                if let HIRModuleMember::Func { func } = m {
                    codegen.gen_function(func).expect("Codegen error");
                }
            }
        }
        _ => panic!("Expected App at top level for build"),
    }
    let llvm_ir = codegen.get_output();

    // STAGE_DUMP: Save LLVM IR to output folder
    fs::write(output_dir.join("stage.ll"), &llvm_ir).expect("Failed to dump IR");

    // 5. Linking
    println!("Linking...");
    let temp_ll = output_dir.join("stage.ll");
    let status = Command::new("clang")
        .arg(temp_ll)
        .arg("-o")
        .arg(output_bin)
        .status()
        .expect("Failed to run clang");

    if status.success() {
        println!("Successfully built: {}", output_bin);
    } else {
        println!("Linking failed");
    }
}
