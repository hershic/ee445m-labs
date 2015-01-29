cmake_minimum_required(VERSION 2.8.7)

set (graphics2d-src shape.c framebuffer.c graphics2d.c)
include_directories(../)

add_library (graphics2d STATIC ${graphics2d-src})
