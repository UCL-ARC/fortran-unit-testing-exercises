cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "intro-to-unit-tests"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Exercises to teach unit testing"
)

# Define src and test files
set(PROJ_SRC_FILES "${PROJECT_SOURCE_DIR}/src/maths.f90")
set(PROJ_TEST_FILES "${PROJECT_SOURCE_DIR}/test/test_maths.f90")

# Build test executable
add_executable(test_maths_exec
    "${PROJ_SRC_FILES}"
    "${PROJ_TEST_FILES}"
)

# Add test as ctest
enable_testing()
add_test(NAME test_maths COMMAND test_maths_exec)
