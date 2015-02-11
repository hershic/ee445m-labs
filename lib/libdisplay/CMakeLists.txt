cmake_minimum_required(VERSION 2.8.7)

set (display-src ST7735.c)

include_directories(../../)

add_library (display STATIC ${display-src})
