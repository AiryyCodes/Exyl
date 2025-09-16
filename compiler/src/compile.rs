use std::process::Command;

use crate::codegen::CodeGen;

pub fn compile_to_binary(codegen: &CodeGen, output_name: &str) {
    // 1️⃣ Dump LLVM IR
    let ll_file = "output.ll";
    codegen.module.print_to_file(ll_file).unwrap();

    // 2️⃣ Compile LLVM IR to object file
    let obj_file = "output.o";
    Command::new("llc")
        .args(&["-filetype=obj", ll_file, "-o", obj_file])
        .status()
        .expect("Failed to run llc");

    // 3️⃣ Compile the C runtime into a static library
    let runtime_c = "runtime/src/runtime.c";
    let runtime_obj = "runtime.o";
    let runtime_lib = "libruntime.a";

    Command::new("clang")
        .args(&["-c", runtime_c, "-o", runtime_obj])
        .status()
        .expect("Failed to compile C runtime");

    Command::new("ar")
        .args(&["rcs", runtime_lib, runtime_obj])
        .status()
        .expect("Failed to create static library for runtime");

    // 4️⃣ Link everything into the final binary
    Command::new("clang")
        .args(&[obj_file, "-L.", "-lruntime", "-lm", "-o", output_name])
        .status()
        .expect("Failed to link binary");
}
