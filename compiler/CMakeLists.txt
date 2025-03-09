cmake_minimum_required(VERSION 3.28)
project(ExylCompiler)

file(GLOB_RECURSE COMPILER_SOURCES
    "src/**.cpp"
)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# find_library(LLD_LIBRARY NAMES lld REQUIRED)
# find_library(LLD_COMMON_LIBRARY NAMES lldCommon REQUIRED)

add_executable(${PROJECT_NAME} ${COMPILER_SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE "src/")

llvm_map_components_to_libnames(LLVM_LIBS
    core
    support
    native
    executionengine
    scalaropts
    analysis
    target
)
target_link_libraries(${PROJECT_NAME} PRIVATE ${LLVM_LIBS})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")

# Optional: Set up LLVM versions for more flexibility in managing dependencies
set(LLVM_VERSION ${LLVM_PACKAGE_VERSION})
