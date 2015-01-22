cmake_minimum_required(VERSION 3.0)

# If we're not building for an embedded system, then we should also
# build the validation programs
if (NOT BUILD_FOR_EMBEDDED)
  add_subdirectory(validation)
endif()

add_subdirectory(lib)
