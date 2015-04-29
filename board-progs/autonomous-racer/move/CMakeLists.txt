cmake_minimum_required(VERSION 2.8.7)

project(move-main)
set(move-main-srcs ../../startup_gcc_canonical.c
../common/blinker.cpp ../common/timerpp.cpp ../common/
../common/uartpp.cpp ../common/shellpp.cpp ../common/semaphorepp.cpp
../common/canpp.cpp ../common/adcpp.cpp ../common/
../common/motorpp.cpp ../common/drivepp.cpp ../common/ir.cpp
../common/switchpp.cpp
../common/pingpp.cpp move-main.cpp)

configure_file(../../map.ld ../map.ld COPYONLY)

include_directories(../common)
include_directories(../../../)
include_directories(../../../lib)

add_executable(move-main ${move-main-srcs})
target_link_libraries(move-main driverlib)
target_link_libraries(move-main os)
target_link_libraries(move-main uart)

# Local Variables:
# firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/")
# End:
