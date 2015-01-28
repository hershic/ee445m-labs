cmake_minimum_required(VERSION 2.8.7)

set (uart-src uart.c)

include_directories(../../)

add_library (uart STATIC ${uart-src})

find_library(libcarm thumb/libc.a)

target_link_libraries(uart driverlib)
target_link_libraries(uart utils)
target_link_libraries(uart ${libcarm})
