cmake_minimum_required(VERSION 3.0)

project(console-grapher)
set(console-grapher-srcs console-grapher.c)

include_directories(../../lib/libgraphics2d)
include_directories(../../lib/liboscope-grapher)
include_directories(../../lib/libconsole)

add_executable(console-grapher ${console-grapher-srcs})

target_link_libraries(console-grapher graphics2d)
target_link_libraries(console-grapher oscope-grapher)
target_link_libraries(console-grapher console)
