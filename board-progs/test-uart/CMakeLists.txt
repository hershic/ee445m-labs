cmake_minimum_required(VERSION 2.8.7)

project(test-uart)
set(test-uart-srcs test-uart.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-uart ${test-uart-srcs})
target_link_libraries(test-uart driverlib)
target_link_libraries(test-uart uart)
target_link_libraries(test-uart heartbeat)
