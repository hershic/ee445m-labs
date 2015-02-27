cmake_minimum_required (VERSION 2.8)

project (ee445m-doc-lab2)
include (../UseLATEX.cmake)
add_latex_document (writeup.tex
  USE_GLOSSARY
  DEFAULT_PDF
)