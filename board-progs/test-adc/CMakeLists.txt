cmake_minimum_required(VERSION 2.8.7)

project(test-adc)
set(test-adc-srcs test-adc.c ../test-edf/startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-adc ${test-adc-srcs})
target_link_libraries(test-adc driverlib)
target_link_libraries(test-adc adc)
target_link_libraries(test-adc std)
target_link_libraries(test-adc display)
