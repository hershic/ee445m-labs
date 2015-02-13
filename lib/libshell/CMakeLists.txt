cmake_minimum_required(VERSION 2.8.7)

set (shell-src shell.c)
include_directories(../../)
include_directories(../../lib)

add_library (shell STATIC ${shell-src})
target_link_libraries(shell std)
target_link_libraries(shell hw)
