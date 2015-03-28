cmake_minimum_required(VERSION 2.8.7)

project(test-lab4)
set(test-lab4-srcs test-lab4.c ../test-edf/startup_gcc.c)

include_directories(../../cmsis/Include/)
include_directories(../../)
include_directories(../../lib)

add_executable(test-lab4 ${test-lab4-srcs})
target_link_libraries(test-lab4 driverlib)
target_link_libraries(test-lab4 adc)
target_link_libraries(test-lab4 std)
target_link_libraries(test-lab4 display)
target_link_libraries(test-lab4 button)
target_link_libraries(test-lab4 timer)
target_link_libraries(test-lab4 shell)
target_link_libraries(test-lab4 cmsis)
