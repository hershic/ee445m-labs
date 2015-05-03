cmake_minimum_required(VERSION 2.8.7)

project(test-countdown)
set(test-countdown-srcs ../../startup_gcc_canonical.c
../common/blinker.cpp ../common/timerpp.cpp ../common/uartpp.cpp ../common/semaphorepp.cpp test-countdown.cpp)

configure_file(../../map.ld ../map.ld COPYONLY)

include_directories(../common)
include_directories(../../../)
include_directories(../../../lib)

add_executable(test-countdown ${test-countdown-srcs})
target_link_libraries(test-countdown driverlib)
target_link_libraries(test-countdown os)
target_link_libraries(test-countdown uart)
