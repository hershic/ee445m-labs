cmake_minimum_required(VERSION 3.0)

set (time-src timekit.c)

add_library (time SHARED ${time-src})