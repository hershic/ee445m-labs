cmake_minimum_required (VERSION 2.8)

project (ee445m-doc-lab5)
include (../UseLATEX.cmake)

set (doc-staging-area "./doc-staging")

add_custom_target (copy)
add_custom_command (
  TARGET copy
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/test-lab5/test-lab5.c ${doc-staging-area}/lab5.c
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../lib/libfatfs/ff.c ${doc-staging-area}/ff.c
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../lib/libfatfs/ff.h ${doc-staging-area}/ff.h
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../lib/libshell/shell.c ${doc-staging-area}/shell.c
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../lib/libshell/shell.h ${doc-staging-area}/shell.h
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../lib/libos/system.c ${doc-staging-area}/system.c
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../lib/libos/system.h ${doc-staging-area}/system.h
)

add_latex_document (writeup.tex
  IMAGE_DIRS img
  DEPENDS copy
  CONFIGURE writeup.tex
  USE_GLOSSARY
  DEFAULT_PDF
)
