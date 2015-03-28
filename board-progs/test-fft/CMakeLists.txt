cmake_minimum_required(VERSION 2.8.7)

project(test-fft)
set(test-fft-srcs test_fft.cpp ../test-edf/startup_gcc.c adc_sim.cpp)

include_directories(../../cmsis/Include/)
include_directories(../../)
include_directories(../../lib)
include_directories(../../driverlib)

add_executable(test-fft ${test-fft-srcs})

target_link_libraries(test-fft cmsis)
target_link_libraries(test-fft adc)
