# CMake Toolchain file for the gcc-arm-embedded toolchain.
# https://launchpad.net/gcc-arm-embedded
#
# Copyright (c) 2013 Swift Navigation Inc.
# Contact: Fergus Noble <fergus@swift-nav.com>
#
# This source is subject to the license found in the file 'LICENSE' which must
# be be distributed together with this source. All other rights reserved.
#
# THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
# EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.

include(CMakeForceCompiler)

# Targeting an embedded system, no OS.
set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR cortex-m4)
set(BUILD_FOR_EMBEDDED True)

CMAKE_FORCE_C_COMPILER(arm-none-eabi-gcc GNU)
CMAKE_FORCE_CXX_COMPILER(arm-none-eabi-g++ GNU)

# Find the target environment prefix..
# First see where gcc is keeping libc.a
execute_process(
  COMMAND ${CMAKE_C_COMPILER} -print-file-name=libc.a
  OUTPUT_VARIABLE CMAKE_INSTALL_PREFIX
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
# Strip the filename off
get_filename_component(CMAKE_INSTALL_PREFIX
  "${CMAKE_INSTALL_PREFIX}" PATH
)
# Then find the canonical path to the directory one up from there
get_filename_component(CMAKE_INSTALL_PREFIX
  "${CMAKE_INSTALL_PREFIX}/.." REALPATH
)
set(CMAKE_INSTALL_PREFIX  ${CMAKE_INSTALL_PREFIX} CACHE FILEPATH
    "Install path prefix, prepended onto install directories.")

message(STATUS "Cross-compiling with the gcc-arm-embedded toolchain")
message(STATUS "Toolchain prefix: ${CMAKE_INSTALL_PREFIX}")

set(CMAKE_FIND_ROOT_PATH  ${CMAKE_INSTALL_PREFIX})

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

set(CMAKE_C_FLAGS
  "${CMAKE_C_FLAGS}"
  "-g -fno-common -ffunction-sections -fdata-sections -fomit-frame-pointer -Dgcc"
  "-unresolved-symbols=ignore-all -fsingle-precision-constant -fno-unwind-tables"
  "-DPART_TM4C123GH6PM -DTARGET_IS_BLIZZARD_RA1"
)

if (CMAKE_SYSTEM_PROCESSOR STREQUAL "cortex-m4")
  message(STATUS "Setting processor to cortex-m4")
  set(CMAKE_C_FLAGS
    "${CMAKE_C_FLAGS}"
    "-mcpu=cortex-m4 -march=armv7e-m -mthumb"
    "-mfloat-abi=softfp -mfpu=fpv4-sp-d16"
  )
elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "cortex-m3")
  message(STATUS "Setting processor to cortex-m3")
  set(CMAKE_C_FLAGS
    "${CMAKE_C_FLAGS}"
    "-mcpu=cortex-m3 -march=armv7-m -mthumb"
    "-msoft-float"
  )

else ()
  message(WARNING
    "Processor not recognised in toolchain file, "
    "compiler flags not configured."
  )
endif ()

# When we break up long strings in CMake we get semicolon
# separated lists, undo this here...
string(REGEX REPLACE ";" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS}" CACHE STRING "")

set(BUILD_SHARED_LIBS OFF)