cmake_minimum_required(VERSION 2.8.7)

set (hw-src hardware.c)
include_directories(../)
include_directories(../../)
include_directories(../../inc)
include_directories(../../lib)

add_library (hw STATIC ${hw-src})

# TODO: fix this not needing to be linked
# target_link_libraries(hw uart)
target_link_libraries(hw timer)
