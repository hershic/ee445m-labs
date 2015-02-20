cmake_minimum_required(VERSION 2.8.7)

set (heartbeat-src heartbeat.c)
include_directories(../../)
include_directories(../../lib)

add_library (heartbeat STATIC ${heartbeat-src})

target_link_libraries(heartbeat os)
