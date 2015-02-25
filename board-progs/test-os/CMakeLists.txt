cmake_minimum_required(VERSION 2.8.7)

project(test-os)
set(test-os-srcs test-os.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-os ${test-os-srcs})
target_link_libraries(test-os driverlib)
target_link_libraries(test-os utils)
target_link_libraries(test-os button)
target_link_libraries(test-os os)
