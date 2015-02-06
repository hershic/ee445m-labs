cmake_minimum_required(VERSION 2.8.7)

set (time-src timekit.c)

add_library (time STATIC ${time-src})
