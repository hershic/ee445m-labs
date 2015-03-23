cmake_minimum_required(VERSION 3.0)

file(GLOB_RECURSE cmsis-src "*.c")

include_directories(Include/)
include_directories(Device/ARM/ARMCM0/Include/)
include_directories(Device/ARM/ARMCM0plus/Include/)
include_directories(Device/ARM/ARMCM3/Include/)
include_directories(Device/ARM/ARMCM4/Include/)
include_directories(Device/ARM/ARMSC000/Include/)
include_directories(Device/ARM/ARMSC300/Include/)
include_directories(Device/_Template_Vendor/Vendor/Device/Include/)
include_directories(RTOS/)
include_directories(DSP_Lib/Examples/Common/Include/)

add_library (cmsis STATIC ${cmsis-src})
