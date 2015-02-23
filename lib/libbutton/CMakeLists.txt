cmake_minimum_required(VERSION 2.8.7)

set (button-src button.c)

include_directories(../../lib/)
include_directories(../../)

add_library (button STATIC ${button-src})
target_link_libraries(button driverlib)
target_link_libraries(button hw)
