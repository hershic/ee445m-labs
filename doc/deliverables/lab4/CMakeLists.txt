cmake_minimum_required (VERSION 2.8)

project (ee445m-doc-lab4)
include (../UseLATEX.cmake)

set (doc-staging-area "./doc-staging")

add_custom_target (copy)
add_custom_command (
  TARGET copy
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/test-lab4/test-lab4.c ${doc-staging-area}/lab4.c
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/test-lab4/filter_gen.m ${doc-staging-area}/filter_gen.m
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/test-lab4/signal_gen.m ${doc-staging-area}/signal_gen.m
  COMMAND ${CMAKE_COMMAND} -E copy ../../../../board-progs/test-lab4/test.m ${doc-staging-area}/test.m
)

add_latex_document (writeup.tex
  IMAGE_DIRS img
  DEPENDS copy
  CONFIGURE writeup.tex
  USE_GLOSSARY
  DEFAULT_PDF
)
