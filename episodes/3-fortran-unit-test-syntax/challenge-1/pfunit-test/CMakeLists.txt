find_package(PFUNIT REQUIRED)
enable_testing()

# Define test files
set(PROJ_PFUNIT_DIR "${PROJECT_SOURCE_DIR}/pfunit-test")

# Filter out the main.f90 files. We can only have one main() function in our tests
set(PROJ_SRC_FILES_EXEC_MAIN ${PROJ_SRC_FILES})
list(FILTER PROJ_SRC_FILES_EXEC_MAIN EXCLUDE REGEX ".*main.f90")

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES_EXEC_MAIN})

file(GLOB
  test_srcs
  "${PROJ_PFUNIT_DIR}/*.pf"
)

# Ensure test data is available at runtime
file(CREATE_LINK "${PROJECT_SOURCE_DIR}/test-models"
        "${CMAKE_CURRENT_BINARY_DIR}/test-models" SYMBOLIC)

# evolve_board tests
set(test_evolve_board_src ${test_srcs})
list(FILTER test_evolve_board_src INCLUDE REGEX ".*test_evolve_board.pf")

add_pfunit_ctest (evolve_board_tests
  TEST_SOURCES ${test_evolve_board_src}
  LINK_LIBRARIES sut # your application library
  )

# check_for_steady_state tests
set(test_check_for_steady_state_src ${test_srcs})
list(FILTER test_check_for_steady_state_src INCLUDE REGEX ".*test_check_for_steady_state.pf")

add_pfunit_ctest (check_for_steady_state_tests
  TEST_SOURCES ${test_check_for_steady_state_src}
  LINK_LIBRARIES sut # your application library
  )

# read_model_from_file tests
set(test_read_model_from_file_src ${test_srcs})
list(FILTER test_read_model_from_file_src INCLUDE REGEX ".*test_read_model_from_file.pf")

add_pfunit_ctest (read_model_from_file_tests
  TEST_SOURCES ${test_read_model_from_file_src}
  LINK_LIBRARIES sut # your application library
  )
