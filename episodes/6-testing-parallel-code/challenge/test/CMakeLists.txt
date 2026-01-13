find_package(PFUNIT REQUIRED)
enable_testing()

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fopenmp")

# Ensure test data is available at runtime
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test")
file(CREATE_LINK "${PROJECT_SOURCE_DIR}/test/models"
        "${CMAKE_CURRENT_BINARY_DIR}/test/models" SYMBOLIC)

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES})

set(TO_LINK_TO_SRC MPI::MPI_Fortran)
if(OpenMP_Fortran_FOUND)
  list(APPEND TO_LINK_TO_SRC OpenMP::OpenMP_Fortran)
endif()

target_link_libraries(sut "${TO_LINK_TO_SRC}")

#########################
# Serial tests
#########################

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

#########################
# MPI tests
#########################

# get_local_grid_info tests
add_pfunit_ctest (pfunit_get_local_grid_info_tests
  TEST_SOURCES "${PROJECT_SOURCE_DIR}/test/test_get_local_grid_info.pf"
  LINK_LIBRARIES sut # your application library
  MAX_PES 4
  )
  
# find_steady_state tests
# Your changes here...

# exchange_boundaries tests
# Your changes here...
