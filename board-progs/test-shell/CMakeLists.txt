cmake_minimum_required(VERSION 2.8.7)

project(test-shell)
set(test-shell-srcs test-shell.c startup_gcc.c)

include_directories(../../)
include_directories(../../inc)
include_directories(../../driverlib)
include_directories(../../lib)

add_executable(test-shell ${test-shell-srcs})
target_link_libraries(test-shell driverlib)
target_link_libraries(test-shell utils)
target_link_libraries(test-shell timer)
target_link_libraries(test-shell shell)
target_link_libraries(test-shell uart)
