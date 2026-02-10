cmake_minimum_required(VERSION 4.1)
project(Exyl)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 23)

file(GLOB_RECURSE COMPILER_SOURCES
    "Compiler/**.cpp"
)

add_executable(exylc ${COMPILER_SOURCES})

add_custom_target(copy_runtime ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory
            ${CMAKE_SOURCE_DIR}/Library
            $<TARGET_FILE_DIR:exylc>/Library
)

add_dependencies(exylc copy_runtime)

find_package(LLVM CONFIG REQUIRED)

target_include_directories(exylc
    PRIVATE
    ${LLVM_INCLUDE_DIRS}
)

target_link_libraries(exylc PRIVATE
    LLVM
)
