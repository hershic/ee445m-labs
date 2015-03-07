cmake_minimum_required(VERSION 2.8.7)

project(test-edf)
set(test-edf-srcs test-edf.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-edf ${test-edf-srcs})
target_link_libraries(test-edf driverlib)
target_link_libraries(test-edf os)
