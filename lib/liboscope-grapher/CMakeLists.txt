cmake_minimum_required(VERSION 3.0)

set (oscope-grapher-src graphlib.c graph_framebuffer.c)

include_directories(../../lib/libgraphics2d)

add_library (oscope-grapher STATIC ${oscope-grapher-src})
target_link_libraries(oscope-grapher graphics2d)
