cmake_minimum_required(VERSION 2.8.7)

set (shell-src shell.c)
include_directories(../../)
include_directories(../../lib)
include_directories(../../lib/libstd)
include_directories(../../lib/libhw)

add_library (shell STATIC ${shell-src})
target_link_libraries(shell libstd)
target_link_libraries(shell libhw)
