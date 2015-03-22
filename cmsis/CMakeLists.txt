cmake_minimum_required(VERSION 3.0)

file(GLOB_RECURSE cmsis-src "*.c")
include_directories(../)
include_directories(./)

add_library (cmsis STATIC ${cmsis-src})
