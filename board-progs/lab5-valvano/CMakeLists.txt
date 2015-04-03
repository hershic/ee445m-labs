cmake_minimum_required(VERSION 2.8.7)

project(lab5-valvano)
set(lab5-valvano-srcs SDCFile.c ../startup_gcc_canonical.c)

include_directories(../../)
include_directories(../../lib)

add_executable(lab5-valvano ${lab5-valvano-srcs})
target_link_libraries(lab5-valvano driverlib)
target_link_libraries(lab5-valvano std)
target_link_libraries(lab5-valvano display)
target_link_libraries(lab5-valvano shell)
target_link_libraries(lab5-valvano fatfs)
