cmake_minimum_required(VERSION 2.8.7)

set (libstd nexus.c)

include_directories(../../)

add_library (libstd STATIC ${libstd})

target_link_libraries(libstd driverlib)
