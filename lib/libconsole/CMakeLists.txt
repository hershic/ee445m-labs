cmake_minimum_required(VERSION 3.0)

set (console-src console_framebuffer.c)

include_directories(../../lib/libgraphics2d)

add_library (console SHARED ${console-src})
target_link_libraries(console graphics2d)
