cmake_minimum_required(VERSION 2.8.7)

# TODO: Why is this here? What does it do? Why is it required?
set(CMAKE_C_COMPILE_FLAGS "-g -Wall -ffunction-sections -fdata-sections -Dgcc")

# TODO: Why can't I put this in the toolchain file
set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} -fno-rtti -fno-exceptions -MD")

set(CMAKE_EXE_LINKER_FLAGS "-T ../map.ld --entry Reset_Handler -nostdlib")
configure_file(map.ld map.ld COPYONLY)

# add_subdirectory(blinky)
add_subdirectory(test-uart)
add_subdirectory(test-adc)
add_subdirectory(test-timer)
add_subdirectory(test-shell)
# add_subdirectory(test-display)
# add_subdirectory(test-os)
add_subdirectory(test-button)
# add_subdirectory(test-semaphore)
add_subdirectory(test-debounce)
# add_subdirectory(test-priority-scheduler)
add_subdirectory(test-edf)
add_subdirectory(test-combination)
add_subdirectory(test-cpp)
add_subdirectory(test-lab4-prep)
add_subdirectory(test-fft)
add_subdirectory(test-lab4)
add_subdirectory(lab5-valvano)
add_subdirectory(test-lab5)
