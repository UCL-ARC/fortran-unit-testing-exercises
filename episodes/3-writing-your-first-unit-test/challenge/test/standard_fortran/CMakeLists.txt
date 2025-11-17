message(STATUS "Using standard Fortran")

set(TEST_DIR "${PROJECT_SOURCE_DIR}/test/standard_fortran")

# Define test file(s)
file(GLOB test_srcs "${TEST_DIR}/*.f90")

list(LENGTH test_srcs num_test_srcs)
if (num_test_srcs EQUAL 0)
    message(FATAL_ERROR "No .f90 files found in: ${TEST_DIR}")
endif()

# Build test executable
add_executable(test_temp_conversions_exec
    "${PROJ_SRC_FILES}"
    "${test_srcs}"
)

# Add test as ctest
add_test(NAME test_temp_conversions COMMAND test_temp_conversions_exec)

