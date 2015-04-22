cmake_minimum_required(VERSION 2.8.7)

project(test-cpp)
set(test-cpp-srcs ../startup_gcc_canonical.c blinker.cpp timerpp.cpp uartpp.cpp bufferpp.cpp shellpp.cpp semaphorepp.cpp canpp.cpp adcpp.cpp motorpp.cpp test-cpp.cpp)

include_directories(../../)
include_directories(../../lib)

add_executable(test-cpp ${test-cpp-srcs})
target_link_libraries(test-cpp driverlib)
target_link_libraries(test-cpp os)
target_link_libraries(test-cpp uart)
