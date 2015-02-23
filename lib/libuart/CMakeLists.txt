cmake_minimum_required(VERSION 2.8.7)

set (uart-src uart.c)

include_directories(../../)
include_directories(../../lib)

add_library (uart STATIC ${uart-src})

target_link_libraries(uart std)
target_link_libraries(uart driverlib)
