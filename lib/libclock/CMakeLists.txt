cmake_minimum_required(VERSION 3.0)

set (clock-src clock.c)

include_directories(../../lib/libgraphics2d)
include_directories(../../lib/libtime)

add_library (clock STATIC ${clock-src})
target_link_libraries(clock time)
target_link_libraries(clock graphics2d)