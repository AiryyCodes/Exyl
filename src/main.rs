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
pub mod span;
pub mod token;
pub mod types;

#[derive(Parser, Debug)]
struct Args {
    #[arg()]
    file: String,

    #[arg(short)]
    output: String,
}

fn print_diagnostic_error(filename: &str, source: &str, message: &str, line: usize, col: usize) {
    eprintln!("\x1b[1;31merror\x1b[0m: {}", message);
    eprintln!("  \x1b[1;34m-->\x1b[0m {}:{}:{}", filename, line, col);

    // Attempt to isolate and extract the problematic line context from raw string buffer
    if let Some(source_line) = source.lines().nth(line.saturating_sub(1)) {
        eprintln!("   \x1b[1;34m|\x1b[0m");
        eprintln!("{:3} \x1b[1;34m|\x1b[0m {}", line, source_line);
        eprintln!(
            "   \x1b[1;34m|\x1b[0m {: >col$}\x1b[1;31m^\x1b[0m",
            "",
            col = col.saturating_sub(1)
        );
    }
    eprintln!();
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
        .map_err(|e| {
            format!(
                "Linker Engine Failure: Could not successfully call executable binary '{}': {}",
                linker, e
            )
        })?;

    if !status.success() {
        return Err(format!(
            "Linker Execution Failure: Binary linker '{}' exited returning a non-zero status code.",
            linker
        ));
    }

    Ok(())
}

fn compile(raw_program: &Program, source: &str, args: &Args) -> Result<(), String> {
    let semantic_analyzer = SemanticAnalyzer::new();

    let (typed_program, type_errors) = semantic_analyzer.analyze(raw_program);

    // If semantic validation errors are present, format diagnostics and halt before LLVM emission
    if !type_errors.is_empty() {
        for error in &type_errors {
            print_diagnostic_error(
                &args.file,
                source,
                &error.message,
                error.span.line,
                error.span.col,
            );
        }
        return Err(format!(
            "Compilation aborted due to {} previous type errors.",
            Vec::len(&type_errors)
        ));
    }

    let context = Context::create();
    let mut backend = LlvmGenerator::new(&context, "exyl_module");

    for node in &typed_program.nodes {
        node.emit(&mut backend);
    }

    if let Err(err) = backend.module.verify() {
        eprintln!(
            "\n\x1b[1;31m❌ LLVM Ir Verification Pipeline Rejected Code Module:\x1b[0m\n{}",
            err.to_string()
        );
        std::process::exit(1);
    }

    backend.module.print_to_stderr();

    Target::initialize_native(&InitializationConfig::default()).map_err(|e| {
        format!(
            "LLVM Backend Error: Target Initialization routine failed: {}",
            e
        )
    })?;

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).map_err(|e| {
        format!(
            "LLVM Backend Error: Unrecognized or invalid native triple architecture choice: {}",
            e
        )
    })?;

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
        .ok_or("LLVM Backend Error: Could not instantiate system-specific TargetMachine runner component instance.")?;

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
        args.output.clone()
    };

    machine
        .write_to_file(&backend.module, FileType::Object, obj_path.as_ref())
        .map_err(|e| format!("LLVM Backend Error: Code emission could not write raw object stream to target path destination: {}", e))?;

    link(&obj_path, &bin_path)?;

    // Attempt target environment artifact optimization cleanup
    if let Err(e) = fs::remove_file(&obj_path) {
        eprintln!(
            "\x1b[1;33mcompiler warning\x1b[0m: Could not purge volatile build artifact file '{}': {}",
            obj_path, e
        );
    }

    Ok(())
}

fn main() {
    let args = Args::parse();

    let source = match fs::read_to_string(&args.file) {
        Ok(src) => src,
        Err(e) => {
            eprintln!(
                "\x1b[1;31merror\x1b[0m: File access failure. Could not locate or read target source path instruction string '{}': {}",
                args.file, e
            );
            std::process::exit(1);
        }
    };

    let mut lexer = Lexer::new(source.clone());
    let tokens = lexer.analyze();

    let mut parser = parser::Parser::new(tokens);
    let (program, parse_errors) = parser.parse();

    // Catch structural parse errors using the new span-aware error print helper
    if !parse_errors.is_empty() {
        for error in parse_errors {
            print_diagnostic_error(
                &args.file,
                &source,
                &error.message,
                error.span.line,
                error.span.col,
            );
        }
        std::process::exit(1);
    }

    // Attempt backend compilation process
    if let Err(compile_error_msg) = compile(&program, &source, &args) {
        eprintln!("\x1b[1;31merror\x1b[0m: {}", compile_error_msg);
        std::process::exit(1);
    }

    println!(
        "\x1b[1;32mSuccess:\x1b[0m Program executable successfully written to target binary path layout format."
    );
}
