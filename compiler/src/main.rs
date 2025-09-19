use std::{
    env, fs,
    path::{Path, PathBuf},
};

use inkwell::context::Context;

use crate::{
    ast::Program, codegen::CodeGen, compile::compile_to_binary, lexer::tokenize, parser::parse,
    type_checker::TypeChecker,
};

mod ast;
mod codegen;
mod compile;
pub mod function;
mod lexer;
mod parser;
pub mod scope;
pub mod token;
mod type_checker;

fn collect_from_dir(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    for entry in fs::read_dir(dir).map_err(|e| format!("{}: {}", dir.display(), e))? {
        let entry = entry.map_err(|e| e.to_string())?;
        let path = entry.path();
        if path.is_dir() {
            collect_from_dir(&path, out)?;
        } else if path.extension().and_then(|s| s.to_str()) == Some("ex") {
            out.push(path);
        }
    }
    Ok(())
}

fn collect_paths(args: &[String]) -> Result<Vec<PathBuf>, String> {
    if args.is_empty() {
        return Err("Usage: exylc <file.ex> [more.ex ...] | <dir> [more_dirs ...]".into());
    }

    let mut paths = Vec::new();

    for a in args {
        let p = Path::new(a);
        if p.is_file() {
            paths.push(p.to_path_buf());
        } else if p.is_dir() {
            collect_from_dir(p, &mut paths)?;
        } else {
            return Err(format!("Not found: {}", p.display()));
        }
    }

    // Dedup and sort
    paths.sort();
    paths.dedup();

    if paths.is_empty() {
        return Err("No .ex files found".into());
    }

    Ok(paths)
}

fn parse_many(paths: &[PathBuf]) -> Result<Program, String> {
    let mut all = Program { body: Vec::new() };

    for p in paths {
        let src = fs::read_to_string(p).map_err(|e| format!("{}: {}", p.display(), e))?;
        let tokens = tokenize(&src).map_err(|e| format!("{}: {}", p.display(), e.message))?;
        let prog = parse(tokens).map_err(|e| format!("{}: {}", p.display(), e.message))?;
        all.body.extend(prog.body);
    }

    Ok(all)
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    let paths = collect_paths(&args).unwrap_or_else(|m| panic!("{}", m));
    let program = parse_many(&paths).unwrap_or_else(|m| panic!("{}", m));

    let mut type_checker = TypeChecker::new();
    let typed_program = type_checker
        .type_check_program(program)
        .unwrap_or_else(|err| panic!("Failed to type check program: {:?}", err));

    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "main");

    codegen.codegen_program(&typed_program);

    compile_to_binary(&codegen, "output");
}
