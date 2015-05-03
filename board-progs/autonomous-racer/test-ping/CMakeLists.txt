cmake_minimum_required(VERSION 2.8.7)

project(test-ping)
set(ping-srcs ../../startup_gcc_canonical.c
../common/blinker.cpp ../common/timerpp.cpp ../common/uartpp.cpp ../common/semaphorepp.cpp ../common/pingpp.cpp test-ping.cpp)

configure_file(../../map.ld ../map.ld COPYONLY)

include_directories(../common)
include_directories(../../../)
include_directories(../../../lib)

add_executable(test-ping ${ping-srcs})
target_link_libraries(test-ping driverlib)
target_link_libraries(test-ping os)
target_link_libraries(test-ping uart)
