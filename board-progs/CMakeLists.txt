cmake_minimum_required(VERSION 2.8.7)

set(CMAKE_C_COMPILE_FLAGS "-g -Wall -ffunction-sections -fdata-sections -Dgcc -DPART_TM4C123GH6PM -DPART_LM4F120H5QR -nostartfiles -nostdlib ")
set(CMAKE_EXE_LINKER_FLAGS "-T ../map.ld --entry Reset_Handler -nostdlib")
configure_file(map.ld map.ld COPYONLY)

add_subdirectory(blinky)
