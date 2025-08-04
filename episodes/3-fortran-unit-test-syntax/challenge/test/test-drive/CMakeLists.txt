# Define test-drive related variables
set(testdrive_url "https://github.com/fortran-lang/test-drive")

set(TEST_DRIVE_BUILD_TESTING OFF CACHE BOOL "Disable test-drive internal tests")

# Download test-drive sources
if(NOT TARGET test-drive)
message(STATUS "Retrieving test-drive from ${testdrive_url}")
include(FetchContent)
FetchContent_Declare(
    "test-drive"
    GIT_REPOSITORY "${testdrive_url}"
    GIT_TAG "HEAD"
    )
FetchContent_MakeAvailable("test-drive")
endif()

# Allows using test-drive via target_link_libraries()
add_library("test-drive::test-drive" INTERFACE IMPORTED)
target_link_libraries("test-drive::test-drive" INTERFACE "test-drive")

# We need the module directory in the subproject before we finish the configure stage
FetchContent_GetProperties("test-drive" SOURCE_DIR "TEST_DRIVE_SOURCE_DIR")
FetchContent_GetProperties("test-drive" BINARY_DIR "TEST_DRIVE_BINARY_DIR")
if(NOT EXISTS "${TEST_DRIVE_BINARY_DIR}/include")
    make_directory("${TEST_DRIVE_BINARY_DIR}/include")
endif()

# Define test files
set(PROJ_TEST_DRIVE_DIR "${PROJECT_SOURCE_DIR}/test/test-drive")

# Filter out the main.f90 files. We can only have one main() function in our tests
set(PROJ_SRC_FILES_EXEC_MAIN ${PROJ_SRC_FILES})
list(FILTER PROJ_SRC_FILES_EXEC_MAIN EXCLUDE REGEX ".*game_of_life.f90")

# List the tests we have available. 
# NOTE: This must match the names of testsuites in main.f90 and the suffixes of the test module files
set(
  tests
  "evolve_board"
  "check_for_steady_state"
  "find_steady_state"
  "read_model_from_file"
)

# Define a list of test source files
set(
  test-srcs
  "main.f90"
)
foreach(t IN LISTS tests)
  string(MAKE_C_IDENTIFIER ${t} t) 
  list(APPEND test-srcs "${PROJ_TEST_DRIVE_DIR}/test_${t}.f90")
endforeach()

# Create an executable which calls the main entrypoint for the tests
add_executable(
  "test_${PROJECT_NAME}-test-drive"
  "${test-srcs}"
  "${PROJ_SRC_FILES_EXEC_MAIN}"
)

# Link test-drive to our new test executable
target_link_libraries(
  "test_${PROJECT_NAME}-test-drive"
  PRIVATE
  "test-drive"
)

# Add specific tests which pass the test name to our new executable
foreach(t IN LISTS tests)
  add_test(NAME "testdrive_${t}" COMMAND "test_${PROJECT_NAME}-test-drive" "${t}" WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
endforeach()
