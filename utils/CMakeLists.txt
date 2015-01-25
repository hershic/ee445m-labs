cmake_minimum_required(VERSION 3.0)

file(GLOB utils-src  *.c)
include_directories(../)

add_library (utils STATIC ${utils-src})