
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell rec {
  name = "my-cmake-glfw-opengl-shell";

  buildInputs = with pkgs; [
    cmake
    clang-tools
    clang
    llvmPackages_16.libllvm
    llvmPackages_16.libcxxClang
  ];

  shellHook = ''
    echo "Welcome to the development environment for CMake, GLFW, and OpenGL!"
    export LIBCLANG_PATH=${pkgs.libclang.lib}/lib
    export PKG_CONFIG_PATH="${pkgs.glib.dev}/lib/pkgconfig:${pkgs.openssl.dev}/lib/pkgconfig"
    # export LLVM_DIR="${pkgs.llvmPackages.libllvm}/lib/cmake/llvm"  # Ensure LLVM_DIR is set
    # export LLVM_CONFIG=$(which llvm-config)
  '';
}

