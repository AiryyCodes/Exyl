#include "Finish.h"
#include <format>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <string>
#include <filesystem>
#include <system_error>

bool write_ir_to_file(llvm::Module &module, const std::string &filename)
{
    std::error_code EC;
    llvm::raw_fd_ostream dest(filename, EC);

    if (EC)
    {
        llvm::errs() << "Could not open file: " << EC.message() << "\n";
        return false;
    }

    llvm::WriteBitcodeToFile(module, dest);
    dest.flush();

    return true;
}

bool compile_with_clang(const std::string &irFile, const std::string &outputFile, const std::string &runtimeDir, bool verbose)
{
    std::string cmd = "clang -O2 -Wall " + irFile;

#ifdef _WIN32
    const char *nullDev = "NUL";
#else
    const char *nullDev = "/dev/null";
#endif

    if (!verbose)
    {
        cmd += std::format(" -w > {}", nullDev);
    }

    // Walk runtime directory
    for (const auto &entry : std::filesystem::recursive_directory_iterator(runtimeDir))
    {
        if (!entry.is_regular_file())
            continue;

        const auto &path = entry.path();

        // Accept C sources (add .cpp if you want)
        if (path.extension() == ".c")
        {
            cmd += " " + path.string();
        }
    }

    cmd += " -o " + outputFile;

    int ret = std::system(cmd.c_str());
    return ret == 0;
}

void run_executable(const std::string &exeFile)
{
    std::string cmd = "./" + exeFile;
    std::system(cmd.c_str());
}

void build_and_run(llvm::Module &module, const std::string &baseName, bool verbose)
{
    std::string irFile = baseName + ".ll";
    std::string exeFile = baseName;

    if (!write_ir_to_file(module, irFile))
    {
        llvm::errs() << "Failed to write IR file\n";
        return;
    }

    if (!compile_with_clang(irFile, exeFile, "Library", verbose))
    {
        llvm::errs() << "Clang compilation failed\n";
        return;
    }

    run_executable(exeFile);
}
