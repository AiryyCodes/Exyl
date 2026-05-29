use std::fs;

use crate::{
    analyzer::SemanticAnalyzer, ast::Program, codegen::Emit, lexer::Lexer, llvm::LlvmGenerator,
};
use clap::Parser;
use inkwell::{
    OptimizationLevel,
    context::Context,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
};

pub mod analyzer;
pub mod ast;
pub mod codegen;
pub mod environment;
pub mod error;
pub mod lexer;
pub mod llvm;
pub mod parser;
pub mod token;
pub mod types;

#[derive(Parser, Debug)]
struct Args {
    #[arg()]
    file: String,

    #[arg(short)]
    output: String,
}

fn link(object_file: &String, output: &String) -> Result<(), String> {
    let out_arg = format!("/OUT:{}", output);

    let (linker, args): (&str, Vec<&str>) = if cfg!(target_os = "windows") {
        (
            "link.exe",
            vec![
                object_file,
                &out_arg,
                "/SUBSYSTEM:CONSOLE",
                "/DEFAULTLIB:libcmt",
            ],
        )
    } else if cfg!(target_os = "macos") {
        ("cc", vec![object_file, "-o", output, "-lSystem"])
    } else {
        // Linux
        ("cc", vec![object_file, "-o", output])
    };

    let status = std::process::Command::new(linker)
        .args(&args)
        .status()
        .map_err(|e| format!("Failed to run linker '{}': {}", linker, e))?;

    if !status.success() {
        return Err(format!("Linking failed with linker '{}'", linker));
    }

    Ok(())
}

fn compile(raw_program: &Program, args: &Args) -> Result<(), String> {
    let mut semantic_analyzer = SemanticAnalyzer::new();
    let typed_program = semantic_analyzer.analyze(raw_program)?;

    let context = Context::create();
    let mut backend = LlvmGenerator::new(&context, "exyl_module");

    for node in &typed_program.nodes {
        node.emit(&mut backend);
    }

    backend.module.print_to_stderr();

    if let Err(err) = backend.module.verify() {
        eprintln!("\n❌ LLVM Verification Failed:\n{}", err.to_string());
        std::process::exit(1);
    }

    Target::initialize_native(&InitializationConfig::default())
        .map_err(|e| format!("Failed to initialize target: {}", e))?;

    let triple = TargetMachine::get_default_triple();
    let target =
        Target::from_triple(&triple).map_err(|e| format!("Failed to get target: {}", e))?;

    let cpu = TargetMachine::get_host_cpu_name().to_string();
    let features = TargetMachine::get_host_cpu_features().to_string();

    let machine = target
        .create_target_machine(
            &triple,
            &cpu,
            &features,
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or("Failed to create target machine")?;

    backend.module.set_triple(&triple);
    backend
        .module
        .set_data_layout(&machine.get_target_data().get_data_layout());

    let obj_path = if cfg!(target_os = "windows") {
        format!("{}.obj", &args.output)
    } else {
        format!("{}.o", &args.output)
    };
    let bin_path = if cfg!(target_os = "windows") {
        format!("{}.exe", &args.output)
    } else {
        format!("{}", &args.output)
    };

    machine
        .write_to_file(&backend.module, FileType::Object, obj_path.as_ref())
        .map_err(|e| format!("Failed to write object file: {}", e))?;

    link(&obj_path, &bin_path)?;

    // Clean up object file
    std::fs::remove_file(obj_path).map_err(|e| format!("Failed to remove object file: {}", e))?;

    Ok(())
}

fn main() {
    let args = Args::parse();

    let source = fs::read_to_string(&args.file).expect("Failed to read source file");

    let mut lexer = Lexer::new(source);
    let tokens = lexer.analyze();

    let mut parser = parser::Parser::new(tokens);
    let (program, parse_errors) = parser.parse();

    if !parse_errors.is_empty() {
        for error in parse_errors {
            println!("{}", error.message);
        }
        return;
    }

    match compile(&program, &args) {
        Ok(_) => {}
        Err(msg) => {
            println!("{msg}");
            return;
        }
    }
}
