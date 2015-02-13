cmake_minimum_required(VERSION 2.8.7)

set (std-src nexus.c)

include_directories(../../)

add_library (std STATIC ${std-src})

target_link_libraries(std driverlib)
