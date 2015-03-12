cmake_minimum_required(VERSION 2.8.7)

project(test-combination)
set(test-combination-srcs test-combination.c ../test-edf/startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-combination ${test-combination-srcs})
target_link_libraries(test-combination driverlib)
target_link_libraries(test-combination timer)
target_link_libraries(test-combination button)
target_link_libraries(test-combination hw)
target_link_libraries(test-combination os)
target_link_libraries(test-combination display)
target_link_libraries(test-combination adc)
target_link_libraries(test-combination shell)
