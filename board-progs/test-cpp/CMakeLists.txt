cmake_minimum_required(VERSION 2.8.7)

project(test-cpp)
set(test-cpp-srcs ../test-edf/startup_gcc.c blinker.cpp test-cpp.cpp)
# set(test-cpp-srcs test-cpp.cpp blinker.cpp startup.c)

include_directories(../../)
include_directories(../../lib)

# find_library(libstdc++ thumb/libstdc++.a)
# find_library(libcarm thumb/libc.a)

add_executable(test-cpp ${test-cpp-srcs})
target_link_libraries(test-cpp driverlib)
target_link_libraries(test-cpp /opt/local/arm-none-eabi/lib/thumb/libstdc++.a)
# target_link_libraries(test-cpp /opt/local/arm-none-eabi/lib/thumb/libsupc++.a)
target_link_libraries(test-cpp /opt/local/arm-none-eabi/lib/thumb/libc.a)
# target_link_libraries(test-cpp /opt/local/arm-none-eabi/lib/thumb/libg.a)
target_link_libraries(test-cpp /opt/local/arm-none-eabi/lib/thumb/libm.a)
# target_link_libraries(test-cpp /opt/local/arm-none-eabi/lib/thumb/libssp.a)
target_link_libraries(test-cpp gcc)

