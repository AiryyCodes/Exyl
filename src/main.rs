use std::fs;

use crate::lexer::Lexer;
use clap::Parser;

pub mod lexer;
pub mod token;

#[derive(Parser, Debug)]
struct Args {
    #[arg()]
    file: String,
}

fn main() {
    let args = Args::parse();

    let source = fs::read_to_string(args.file).expect("Failed to read source file");

    let mut lexer = Lexer::new(source);
    let tokens = lexer.analyze();

    println!("Tokens: {:?}", tokens);
}
