#include "analyzer.h"
#include "lexer.h"
#include "parser.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <llvm/IR/Value.h>
#include <llvm/Linker/Linker.h>
#include <string>
#include <utility>
#include <vector>

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/InlineAsm.h>

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"

const std::string TEST_SOURCE = R"(
let myVar: string = "Hello, World!";
let test_var: int = 69;

fun add(a: int, b: int): int
{
    let test_var1 = "This is a test var 1 in add scope";
    //let test_var1 = 69;
}

fun main()
{

}

/*
fun add()
{

}
*/
)";

Lexer lexer(TEST_SOURCE);
Parser parser(lexer);

using namespace llvm;

void printTokens(const std::vector<Token> tokens)
{
    for (const auto &token : tokens)
    {
        printf("Token: Type: %s Value: %s Position: %i\n", tokenTypeToString(token.type).c_str(), token.value.c_str(), token.position);
    }
}

std::string compileModuleToObj(Module &M, StringRef Filename)
{
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OpenFlags::OF_None);

    if (EC)
    {
        errs() << "Could not open file: " << EC.message();
        return "";
    }

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();

    auto TargetTriple = sys::getDefaultTargetTriple();
    std::string Error;
    const Target *TheTarget = TargetRegistry::lookupTarget(TargetTriple, Error);

    if (!TheTarget)
    {
        errs() << "Target not found: " << Error;
        return "";
    }

    auto CPU = "generic";
    auto Features = "";
    TargetOptions opt;
    auto RM = std::optional<Reloc::Model>();
    auto TargetMachine = TheTarget->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

    M.setDataLayout(TargetMachine->createDataLayout());
    M.setTargetTriple(TargetTriple);

    legacy::PassManager pass;
    if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile))
    {
        errs() << "Target machine can't emit object file\n";
        return "";
    }

    pass.run(M);
    dest.flush();

    return std::string(Filename);
}

void linkExecutable(const std::string &objFilename, const std::string &outputFilename)
{

    std::string command = "ld.lld " + objFilename + " std/libExylStd.a -o " + outputFilename + " -Lstd --dynamic-linker /lib64/ld-linux-x86-64.so.2";

    int result = std::system(command.c_str());

    if (result != 0)
    {
        errs() << "Linking failed with error code: " << result << "\n";
    }
}

int main(int argc, char *argv[])
{
    auto ast = parser.Parse();
    for (const auto &node : ast)
    {
        node->Print();
    }

    Analyzer analyzer(ast);
    analyzer.Analyze();

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    LLVMContext context;
    Module *module = new Module("exyl_module", context);

    FunctionType *printType = FunctionType::get(
        llvm::Type::getVoidTy(context),
        {llvm::Type::getInt8PtrTy(context)},
        false);

    Function *printFunc = Function::Create(
        printType,
        Function::ExternalLinkage,
        "print",
        module);

    // Function *printFunc = createPrintFunction(context, module, builder);

    std::vector<llvm::Type *> mainArgs;
    FunctionType *mainType = FunctionType::get(llvm::Type::getInt32Ty(context), mainArgs, false);

    Function *mainFunc = Function::Create(mainType, Function::ExternalLinkage, "main", module);

    BasicBlock *entry = BasicBlock::Create(context, "entry", mainFunc);
    IRBuilder<> builder(entry);

    llvm::Value *strPtr = builder.CreateGlobalStringPtr("Hello, Exyl!");
    builder.CreateCall(printFunc, {strPtr});

    builder.CreateRet(ConstantInt::get(llvm::Type::getInt32Ty(context), 0));

    module->print(outs(), nullptr);

    // Linker linker(*module);
    // linker.linkModules(*module, )

    std::string objFile = compileModuleToObj(*module, "output.o");
    if (!objFile.empty())
    {
        // linkExecutable(objFile, "exyl_program");
    }

    delete module;

    /*
    std::ofstream out("out.asm");

    AsmGenerator generator;
    for (const auto &node : ast)
    {
        generator.generate(node.get(), out);
    }

    out.close();
    */

    /*
    std::vector<Token> tokens;
    Lexer newLexer(TEST_SOURCE);
    while (newLexer.HasNextToken())
    {
        tokens.push_back(newLexer.NextToken());
    }

    printTokens(tokens);
    */

    return EXIT_SUCCESS;
}
