cmake_minimum_required(VERSION 2.8.7)

project(test-uart)
set(test-uart-srcs test-uart.c startup_gcc.c)

include_directories(../../)
include_directories(../../lib)

add_executable(test-uart ${test-uart-srcs})
target_link_libraries(test-uart driverlib)
target_link_libraries(test-uart utils)
target_link_libraries(test-uart uart)
target_link_libraries(test-uart /opt/local/lib/gcc/arm-none-eabi/4.7.3/thumb/libgcc.a)
target_link_libraries(test-uart /opt/local/arm-none-eabi/lib/thumb/libc.a)
target_link_libraries(test-uart /opt/local/arm-none-eabi/lib/thumb/libm.a)
