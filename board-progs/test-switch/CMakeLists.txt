cmake_minimum_required(VERSION 2.8.7)

project(test-switch)
set(test-switch-srcs ../startup_gcc_canonical.c ../autonomous-racer/common/blinker.cpp ../autonomous-racer/common/semaphorepp.cpp ../autonomous-racer/common/uartpp.cpp ../autonomous-racer/common/shellpp.cpp ../autonomous-racer/common/switchpp.cpp test-switch.cpp)

include_directories(../autonomous-racer/common/)
include_directories(../../)
include_directories(../../lib)

add_executable(test-switch ${test-switch-srcs})
target_link_libraries(test-switch os)

# Local Variables:
# firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/")
# End:
