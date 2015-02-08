cmake_minimum_required(VERSION 2.8.7)

set (libstd nexus.c)

include_directories(../../)

add_library (libstd STATIC ${libstd})

find_library(libcarm thumb/libc.a)

target_link_libraries(libstd driverlib)
target_link_libraries(libstd utils)
target_link_libraries(libstd ${libcarm})
