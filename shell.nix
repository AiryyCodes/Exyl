# shell.nix for your CMake, GLFW, OpenGL project
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell rec {
  # Name of the development environment
  name = "my-cmake-glfw-opengl-shell";

  # Dependencies for building and running the project
  buildInputs = with pkgs;[
    cmake
    clang-tools
    clang
  ];

  # Environment variables for the shell (if needed)
  shellHook = ''
    echo "Welcome to the development environment for CMake, GLFW, and OpenGL!"
	export LIBCLANG_PATH=${pkgs.libclang.lib}/lib
  '';
}
