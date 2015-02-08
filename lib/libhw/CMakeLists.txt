cmake_minimum_required(VERSION 2.8.7)

set (libhw-src libhw.c)
include_directories(../)
include_directories(../../)
include_directories(../../inc)
include_directories(../../lib)

add_library (libhw STATIC ${libhw-src})
