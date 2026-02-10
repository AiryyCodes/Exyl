cmake_minimum_required(VERSION 4.1)
project(Exyl)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 23)

file(GLOB_RECURSE COMPILER_SOURCES
    "Compiler/**.cpp"
)

add_executable(exylc ${COMPILER_SOURCES})

add_custom_command(
    TARGET exylc POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory
    ${CMAKE_SOURCE_DIR}/Library
    $<TARGET_FILE_DIR:exylc>/Library
)


find_package(LLVM REQUIRED)

target_link_libraries(exylc PRIVATE
    LLVM
)
