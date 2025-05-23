project(Exylc C)

file(GLOB_RECURSE COMPILER_SOURCES
    "**.c"
)

add_executable(${PROJECT_NAME} ${COMPILER_SOURCES})
