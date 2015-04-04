cmake_minimum_required(VERSION 2.8.7)

project(test-lab5)
set(test-lab5-srcs test-lab5.c ../startup_gcc_canonical.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-lab5 ${test-lab5-srcs})
target_link_libraries(test-lab5 driverlib)
target_link_libraries(test-lab5 std)
target_link_libraries(test-lab5 display)
target_link_libraries(test-lab5 shell)
target_link_libraries(test-lab5 fatfs)
