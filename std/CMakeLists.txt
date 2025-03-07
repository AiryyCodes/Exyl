cmake_minimum_required(VERSION 3.28)
project(ExylStd)

file(GLOB_RECURSE EXYL_NATIVE_SOURCES
    "native/**.cpp"
)

add_library(${PROJECT_NAME} ${EXYL_NATIVE_SOURCES})
