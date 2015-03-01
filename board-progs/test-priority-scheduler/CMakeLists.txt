cmake_minimum_required(VERSION 2.8.7)

project(test-priority-scheduler)
set(test-priority-scheduler-srcs test-priority-scheduler.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-priority-scheduler ${test-priority-scheduler-srcs})
target_link_libraries(test-priority-scheduler driverlib)
target_link_libraries(test-priority-scheduler os)
