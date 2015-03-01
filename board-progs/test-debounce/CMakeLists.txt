cmake_minimum_required(VERSION 2.8.7)

project(test-debounce)
set(test-debounce-srcs test-debounce.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-debounce ${test-debounce-srcs})
target_link_libraries(test-debounce driverlib)
target_link_libraries(test-debounce timer)
target_link_libraries(test-debounce button)
target_link_libraries(test-debounce hw)
target_link_libraries(test-debounce os)
target_link_libraries(test-debounce display)
