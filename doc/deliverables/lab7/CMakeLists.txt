cmake_minimum_required (VERSION 2.8)

project (ee445m-doc-lab6)
include (../UseLATEX.cmake)

set (doc-staging-area "./doc-staging")

add_custom_target (copy)
add_custom_command (
  TARGET copy
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/autonomous-racer/common/motorpp.cpp ${doc-staging-area}/motorpp.cpp
    COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/autonomous-racer/common/motorpp.hpp ${doc-staging-area}/motorpp.hpp
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/autonomous-racer/common/switchpp.cpp ${doc-staging-area}/switchpp.cpp
    COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/autonomous-racer/common/switchpp.hpp ${doc-staging-area}/switchpp.hpp
    COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/autonomous-racer/common/drivepp.hpp ${doc-staging-area}/drivepp.hpp
    COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/autonomous-racer/move/move-main.cpp ${doc-staging-area}/move-main.cpp
    COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/autonomous-racer/sense/sense-main.cpp ${doc-staging-area}/sense-main.cpp

)

add_latex_document (writeup.tex
  IMAGE_DIRS img
  DEPENDS copy
  CONFIGURE writeup.tex
  USE_GLOSSARY
  DEFAULT_PDF
)
