cmake_minimum_required(VERSION 2.8.7)

set (timer-src timer.c)

include_directories(../../lib/)
include_directories(../../)

add_library (timer STATIC ${timer-src})
