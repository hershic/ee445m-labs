cmake_minimum_required(VERSION 2.8.7)

set(CMAKE_C_COMPILE_FLAGS "-g -Wall -ffunction-sections -fdata-sections -Dgcc")
set(CMAKE_EXE_LINKER_FLAGS "-T ../map.ld --entry Reset_Handler -nostdlib")
configure_file(map.ld map.ld COPYONLY)

add_subdirectory(blinky)
add_subdirectory(test-uart)
add_subdirectory(test-adc)
add_subdirectory(test-timer)
add_subdirectory(test-shell)
add_subdirectory(test-display)
add_subdirectory(test-os)

