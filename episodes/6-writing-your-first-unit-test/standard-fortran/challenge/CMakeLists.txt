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

# Define test file(s)
set(PROJ_TEST_FILES 
    "${PROJECT_SOURCE_DIR}/test/test_temp_conversions.f90"
    # Add any further test files here
)

# Build test executable
add_executable(test_temp_conversions_exec
    "${PROJ_SRC_FILES}"
    "${PROJ_TEST_FILES}"
)

# Add test as ctest
enable_testing()
add_test(NAME test_temp_conversions COMMAND test_temp_conversions_exec)
