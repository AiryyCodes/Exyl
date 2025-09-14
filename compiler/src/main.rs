use std::{env, fs};

use inkwell::context::Context;

use crate::{
    codegen::CodeGen, compile::compile_to_binary, lexer::tokenize, parser::parse,
    type_checker::TypeChecker,
};

mod ast;
pub mod builtin;
mod codegen;
mod compile;
mod lexer;
mod parser;
pub mod token;
mod type_checker;

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

    let program =
        parse(tokens).unwrap_or_else(|err| panic!("Failed to parse tokens: {}", err.message));

    let mut type_checker = TypeChecker::new();
    let typed_program = type_checker
        .type_check_program(program.clone())
        .unwrap_or_else(|err| panic!("Failed to type check program: {:?}", err));

    println!("\n---- AST ----\n");

    for stmt in &typed_program.body {
        println!("{:?}", stmt);
    }

    let context = Context::create();
    let codegen = CodeGen::new(&context, "main");

    codegen.declare_print();

    codegen.codegen_program(&typed_program);

    compile_to_binary(&codegen, "output");

    println!("");
}
