cmake_minimum_required(VERSION 2.8.7)

project(test-display)
set(test-display-srcs test-display.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-display ${test-display-srcs})
target_link_libraries(test-display driverlib)
target_link_libraries(test-display display)
target_link_libraries(test-display utils)
target_link_libraries(test-display std)
