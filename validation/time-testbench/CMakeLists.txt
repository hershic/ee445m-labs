cmake_minimum_required(VERSION 3.0)

project(time-testbench)
set(time-testbench-srcs tk_tb.c)

include_directories(../../lib/libtime)

add_executable(time-testbench ${time-testbench-srcs})

target_link_libraries(time-testbench time)
