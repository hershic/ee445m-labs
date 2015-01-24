cmake_minimum_required(VERSION 2.8.7)

project(console-clock)
set(console-clock-srcs console-fb.c)

include_directories(../../lib/libgraphics2d)
include_directories(../../lib/libclock)
include_directories(../../lib/libtime)

add_executable(console-clock ${console-clock-srcs})

target_link_libraries(console-clock graphics2d)
target_link_libraries(console-clock time)
target_link_libraries(console-clock clock)
