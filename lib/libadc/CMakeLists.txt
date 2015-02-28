cmake_minimum_required(VERSION 2.8.7)

set (adc-src adc.c)

include_directories(../../)
include_directories(../../lib)

add_library (adc STATIC ${adc-src})
target_link_libraries(adc driverlib)
target_link_libraries(adc timer)