cmake_minimum_required(VERSION 2.8.7)

set (utils-src uartstdio.c)

include_directories(../lib/)
include_directories(../)

add_library (utils STATIC ${utils-src})
target_link_libraries(utils driverlib)
