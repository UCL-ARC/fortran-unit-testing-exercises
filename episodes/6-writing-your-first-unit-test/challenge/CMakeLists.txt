cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "temp_conversions"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Library for converting between various temperatures"
)

# Define src file(s)
set(PROJ_SRC_FILES "${PROJECT_SOURCE_DIR}/src/temp_conversions.f90")

enable_testing()
add_subdirectory("test")
