cmake_minimum_required(VERSION 2.8.7)

# If we're not building for an embedded system, then we should also
# build the validation programs
if (BUILD_FOR_EMBEDDED)
  add_subdirectory(board-progs)
  add_subdirectory(driverlib)
  # add_subdirectory(cmsis)
else()
  message("\n**** INFO: ****\nTo build the board programs, do:\n  rm -rf * && cmake .. -DCMAKE_TOOLCHAIN_FILE=../toolchain-gcc-arm-embedded.cmake\n")
  add_subdirectory(validation)
endif()

add_subdirectory(lib)
add_subdirectory(utils)
