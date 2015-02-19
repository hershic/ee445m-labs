cmake_minimum_required(VERSION 2.8.7)

project(test-button)
set(test-button-srcs test-button.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-button ${test-button-srcs})
target_link_libraries(test-button driverlib)
target_link_libraries(test-button utils)
target_link_libraries(test-button io)
