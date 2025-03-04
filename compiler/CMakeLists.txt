cmake_minimum_required(VERSION 3.28)
project(ExylCompiler)

file(GLOB_RECURSE COMPILER_SOURCES
    "src/**.cpp"
)

add_executable(${PROJECT_NAME} ${COMPILER_SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE "src/")
