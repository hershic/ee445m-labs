cmake_minimum_required(VERSION 3.0)

project(blinky)
set(blinky-srcs blinky.c startup_gcc.c)

include_directories(../../)
configure_file(map.ld map.ld COPYONLY)

set(CMAKE_C_COMPILE_FLAGS "-g -Wall -ffunction-sections -fdata-sections -Dgcc -DPART_TM4C123GH6PM -DPART_LM4F120H5QR -nostartfiles -nostdlib ")
set(CMAKE_EXE_LINKER_FLAGS "-T map.ld --entry Reset_Handler -nostdlib")

add_executable(blinky ${blinky-srcs})
