cmake_minimum_required(VERSION 2.8.7)

project(test-fft)
set(test-fft-srcs test_fft.cpp ../test-edf/startup_gcc.c)

include_directories(../../cmsis/Include/)
include_directories(../../)
include_directories(../../lib)
include_directories(../../driverlib)

add_executable(test-fft ${test-fft-srcs})

target_link_libraries(test-fft cmsis)
