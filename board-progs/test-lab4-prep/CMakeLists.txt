cmake_minimum_required(VERSION 2.8.7)

project(test-lab4-prep)
set(test-lab4-prep-srcs test-lab4-prep.c ../test-edf/startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-lab4-prep ${test-lab4-prep-srcs})
target_link_libraries(test-lab4-prep driverlib)
target_link_libraries(test-lab4-prep adc)
target_link_libraries(test-lab4-prep std)
target_link_libraries(test-lab4-prep display)
target_link_libraries(test-lab4-prep button)
target_link_libraries(test-lab4-prep timer)
