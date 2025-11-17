find_package(PFUNIT REQUIRED)
enable_testing()

# Ensure test data is available at runtime
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test")
file(CREATE_LINK "${PROJECT_SOURCE_DIR}/test/models"
        "${CMAKE_CURRENT_BINARY_DIR}/test/models" SYMBOLIC)

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES})

# evolve_board tests
add_pfunit_ctest (pfunit_evolve_board_tests
  TEST_SOURCES "${PROJECT_SOURCE_DIR}/test/test_evolve_board.pf"
  LINK_LIBRARIES sut # your application library
  )

# check_for_steady_state tests
add_pfunit_ctest (pfunit_check_for_steady_state_tests
  TEST_SOURCES "${PROJECT_SOURCE_DIR}/test/test_check_for_steady_state.pf"
  LINK_LIBRARIES sut # your application library
  )

# read_model_from_file tests
add_pfunit_ctest (pfunit_read_model_from_file_tests
  TEST_SOURCES "${PROJECT_SOURCE_DIR}/test/test_read_model_from_file.pf"
  LINK_LIBRARIES sut # your application library
  )

# find_steady_state tests
add_pfunit_ctest (pfunit_find_steady_state_tests
  TEST_SOURCES "${PROJECT_SOURCE_DIR}/test/test_find_steady_state.pf"
  LINK_LIBRARIES sut # your application library
  )
