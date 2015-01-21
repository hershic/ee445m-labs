cmake_minimum_required(VERSION 3.0)

set (graphics2d-src shape.c framebuffer.c graphics2d.c)

add_library (graphics2d STATIC ${graphics2d-src})