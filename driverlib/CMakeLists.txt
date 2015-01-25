cmake_minimum_required(VERSION 3.0)

file(GLOB driverlib-src  *.c)
include_directories(../)

add_library (driverlib STATIC ${driverlib-src})