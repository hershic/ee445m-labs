cmake_minimum_required(VERSION 2.8.7)

set (schedule-src schedule.c)
include_directories(./)
include_directories(../../)
include_directories(../../lib)

add_library (schedule STATIC ${schedule-src})

target_link_libraries(schedule driverlib)
target_link_libraries(schedule std)
target_link_libraries(schedule hw)
