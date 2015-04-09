cmake_minimum_required(VERSION 2.8.7)

project(ti-can-rx)
set(ti-can-rx-srcs simple_rx.c ../test-edf/startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(ti-can-rx ${ti-can-rx-srcs})
target_link_libraries(ti-can-rx driverlib)
target_link_libraries(ti-can-rx timer)
target_link_libraries(ti-can-rx os)
target_link_libraries(ti-can-rx utils)
