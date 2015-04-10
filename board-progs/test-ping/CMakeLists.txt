cmake_minimum_required(VERSION 2.8.7)

project(test-ping)
set(test-ping-srcs test-ping.c ../test-edf/startup_gcc.c)

include_directories(../../cmsis/Include/)
include_directories(../../)
include_directories(../../lib)

add_executable(test-ping ${test-ping-srcs})
target_link_libraries(test-ping driverlib)
target_link_libraries(test-ping std)
target_link_libraries(test-ping cmsis)
target_link_libraries(test-ping os)
target_link_libraries(test-ping hw)
