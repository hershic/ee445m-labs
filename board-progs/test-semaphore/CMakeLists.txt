cmake_minimum_required(VERSION 2.8.7)

project(test-semaphore)
set(test-semaphore-srcs test-semaphore.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-semaphore ${test-semaphore-srcs})
target_link_libraries(test-semaphore driverlib)
target_link_libraries(test-semaphore utils)
target_link_libraries(test-semaphore os)
