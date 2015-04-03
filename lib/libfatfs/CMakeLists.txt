cmake_minimum_required(VERSION 2.8.7)

set (fatfs-src ff.c diskio.c)

include_directories(../../lib/)
include_directories(../../)

add_library (fatfs STATIC ${fatfs-src})
