cmake_minimum_required(VERSION 2.8.7)

project(countdown)
set(countdown-srcs ../../startup_gcc_canonical.c
../common/blinker.cpp ../common/timerpp.cpp ../common/uartpp.cpp ../common/semaphorepp.cpp countdown.cpp)

configure_file(../../map.ld ../map.ld COPYONLY)

include_directories(../common)
include_directories(../../../)
include_directories(../../../lib)

add_executable(countdown ${countdown-srcs})
target_link_libraries(countdown driverlib)
target_link_libraries(countdown os)
target_link_libraries(countdown uart)
