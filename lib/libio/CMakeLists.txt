cmake_minimum_required(VERSION 2.8.7)

set (io-src button.c)
include_directories(./)
include_directories(../../)
include_directories(../../lib)

add_library (io STATIC ${io-src})

find_library(libcarm thumb/libc.a)

target_link_libraries(io driverlib)
target_link_libraries(io std)
