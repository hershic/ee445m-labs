cmake_minimum_required(VERSION 2.8.7)

project(ti-can-tx)
set(ti-can-tx-srcs simple_tx.c ../test-edf/startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(ti-can-tx ${ti-can-tx-srcs})
target_link_libraries(ti-can-tx driverlib)
target_link_libraries(ti-can-tx timer)
target_link_libraries(ti-can-tx os)
target_link_libraries(ti-can-tx utils)
