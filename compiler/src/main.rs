use std::{env, fs};

use inkwell::context::Context;

use crate::{codegen::CodeGen, compile::compile_to_binary, lexer::tokenize, parser::parse};

mod ast;
mod codegen;
mod compile;
mod lexer;
mod parser;
pub mod token;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage:");
    }

    let file_path = &args[1];

    let source = fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read contents of file '{}'", file_path));

    println!("\n---- Tokens ----\n");

    let tokens = tokenize(&source)
        .unwrap_or_else(|err| panic!("Failed to tokenize input file: {}", err.message));

    for token in &tokens {
        println!("{:?}", token);
    }

    println!("\n---- AST ----\n");

    let program =
        parse(tokens).unwrap_or_else(|err| panic!("Failed to parse tokens: {}", err.message));

    for stmt in &program.body {
        println!("{:?}", stmt);
    }

    let context = Context::create();
    let codegen = CodeGen::new(&context, "main");

    codegen.declare_print();

    codegen.codegen_program(&program);

    compile_to_binary(&codegen, "output");

    println!("");
}
