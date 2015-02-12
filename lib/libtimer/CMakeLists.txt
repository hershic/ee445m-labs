cmake_minimum_required(VERSION 2.8.7)

set (timer-src timer.c)

include_directories(../../lib/)
include_directories(../../)

add_library (timer STATIC ${timer-src})
target_link_libraries(timer driverlib)
target_link_libraries(timer libhw)

# TODO: REMOVE THIS!
target_link_libraries(timer uart)
