use std::process::Command;

use crate::codegen::CodeGen;

pub fn compile_to_binary(codegen: &CodeGen, output_name: &str) {
    codegen.module.print_to_file("output.ll").unwrap();
    Command::new("llc")
        .args(&["-filetype=obj", "output.ll", "-o", "output.o"])
        .status()
        .unwrap();
    Command::new("rustc")
        .args(&[
            "runtime/src/lib.rs",
            "--crate-type=staticlib",
            "-o",
            "libruntime.a",
        ])
        .status()
        .unwrap();
    Command::new("clang")
        .args(&["output.o", "-L.", "-lruntime", "-o", output_name])
        .status()
        .unwrap();
}
