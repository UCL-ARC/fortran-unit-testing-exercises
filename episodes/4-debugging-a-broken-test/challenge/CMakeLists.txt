cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "ep-4-ch-1"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Matrix transforms"
)

# Define src files
file(GLOB PROJ_SRC_FILES "${PROJECT_SOURCE_DIR}/src/*.f90")

add_subdirectory("test/pfunit")