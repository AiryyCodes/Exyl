#pragma once

#include <llvm/IR/Module.h>
#include <string>

bool write_ir_to_file(llvm::Module &module, const std::string &filename);
bool compile_with_clang(const std::string &irFile, const std::string &outputFile, const std::string &runtimeDir, bool verbose);
void run_executable(const std::string &exeFile);
void build_and_run(llvm::Module &module, const std::string &baseName, bool verbose);
