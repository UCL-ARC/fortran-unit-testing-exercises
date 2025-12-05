message(STATUS "Using standard Fortran")

set(TEST_DIR "${PROJECT_SOURCE_DIR}/test/standard_fortran")

# Define test file(s)
set(test_srcs "test_temp_conversions.f90")

# Build test executable
add_executable(test_temp_conversions_exec
    "${PROJ_SRC_FILES}"
    "${test_srcs}"
)

# Add test as ctest
add_test(NAME test_temp_conversions COMMAND test_temp_conversions_exec)

