cmake_minimum_required(VERSION 3.0)

file(GLOB_RECURSE can-src "*.c")

add_library (can STATIC ${can-src})
