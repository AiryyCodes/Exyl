#include "codegen.h"

#include <string>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

void CodeGen::Compile(const std::string &output)
{
    using namespace llvm;

    visitor.GetModule().print(outs(), nullptr);

    std::error_code errorCode;
    llvm::raw_fd_ostream dest(output, errorCode, sys::fs::OpenFlags::OF_None);

    if (errorCode)
    {
        errs() << "Could not open file: " << errorCode.message();
        return;
    }

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    auto TargetTriple = sys::getDefaultTargetTriple();
    std::string Error;
    const Target *TheTarget = TargetRegistry::lookupTarget(TargetTriple, Error);

    if (!TheTarget)
    {
        errs() << "Target not found: " << Error;
        return;
    }

    auto CPU = "generic";
    auto Features = "";
    TargetOptions opt;
    auto RM = std::optional<Reloc::Model>(Reloc::PIC_);
    TargetMachine *targetMachine = TheTarget->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

    visitor.GetModule().setDataLayout(targetMachine->createDataLayout());
    visitor.GetModule().setTargetTriple(TargetTriple);

    legacy::PassManager pass;
    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, CodeGenFileType::ObjectFile))
    {
        errs() << "Target machine can't emit object file\n";
        return;
    }

    pass.run(visitor.GetModule());
    dest.flush();
}
