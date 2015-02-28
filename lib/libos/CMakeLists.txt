cmake_minimum_required(VERSION 2.8.7)

set (os-src os.c system.c)
include_directories(./)
include_directories(../../)
include_directories(../../lib)

add_library (os STATIC ${os-src})

target_link_libraries(os driverlib)
target_link_libraries(os std)
target_link_libraries(os timer)
