cmake_minimum_required(VERSION 2.8.7)

project(test-timer)
set(test-timer-srcs test-timer.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-timer ${test-timer-srcs})
target_link_libraries(test-timer driverlib)
target_link_libraries(test-timer utils)
target_link_libraries(test-timer timer)
