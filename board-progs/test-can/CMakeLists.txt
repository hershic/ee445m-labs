cmake_minimum_required(VERSION 2.8.7)

project(test-can)
set(test-can-srcs test-can.c ../test-edf/startup_gcc.c)

include_directories(../../cmsis/Include/)
include_directories(../../)
include_directories(../../lib)

add_executable(test-can ${test-can-srcs})
target_link_libraries(test-can driverlib)
target_link_libraries(test-can adc)
target_link_libraries(test-can std)
target_link_libraries(test-can display)
target_link_libraries(test-can button)
target_link_libraries(test-can timer)
target_link_libraries(test-can shell)
target_link_libraries(test-can cmsis)
