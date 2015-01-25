cmake_minimum_required(VERSION 2.8.7)

project(blinky)
set(blinky-srcs blinky.c startup_gcc.c)

include_directories(../../)

add_executable(blinky ${blinky-srcs})
