cmake_minimum_required(VERSION 3.0)

# If we're not building for an embedded system, then we should also
# build the validation programs
if (BUILD_FOR_EMBEDDED)
  add_subdirectory(board-progs)
else()
  message("To build the board programs, do: rm -rf * && cmake .. -DCMAKE_TOOLCHAIN_FILE=../toolchain-gcc-arm-embedded.cmake")
  add_subdirectory(validation)
endif()

add_subdirectory(lib)
