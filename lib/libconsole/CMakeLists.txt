cmake_minimum_required(VERSION 2.8.7)

set (console-src console_framebuffer.c)

include_directories(../../lib/)

add_library (console STATIC ${console-src})
target_link_libraries(console graphics2d)
