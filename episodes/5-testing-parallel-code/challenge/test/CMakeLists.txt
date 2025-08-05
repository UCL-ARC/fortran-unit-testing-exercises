find_package(PFUNIT REQUIRED)
enable_testing()

set(CMAKE_Fortran_FLAGS "-fopenmp")

# Ensure test data is available at runtime
file(CREATE_LINK "${PROJECT_SOURCE_DIR}/test/models"
        "${CMAKE_CURRENT_BINARY_DIR}/test-models" SYMBOLIC)

# Filter out the main.f90 files. We can only have one main() function in our tests
set(PROJ_SRC_FILES_EXEC_MAIN ${PROJ_SRC_FILES})
list(FILTER PROJ_SRC_FILES_EXEC_MAIN EXCLUDE REGEX ".*main.f90")

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES_EXEC_MAIN})

set(TO_LINK_TO_SRC MPI::MPI_Fortran)
list(APPEND TO_LINK_TO_SRC OpenMP::OpenMP_Fortran)
if(OpenMP_Fortran_FOUND)
endif()

target_link_libraries(sut "${TO_LINK_TO_SRC}")

# List all  test files
file(GLOB
  test_srcs
  "${PROJECT_SOURCE_DIR}/test/pfunit/*.pf"
)

#########################
# Serial tests
#########################

# evolve_board tests
set(test_evolve_board_src ${test_srcs})
list(FILTER test_evolve_board_src INCLUDE REGEX ".*test_evolve_board.pf")

add_pfunit_ctest (pfunit_evolve_board_tests
  TEST_SOURCES ${test_evolve_board_src}
  LINK_LIBRARIES sut # your application library
  )

# check_for_steady_state tests
set(test_check_for_steady_state_src ${test_srcs})
list(FILTER test_check_for_steady_state_src INCLUDE REGEX ".*test_check_for_steady_state.pf")

add_pfunit_ctest (pfunit_check_for_steady_state_tests
  TEST_SOURCES ${test_check_for_steady_state_src}
  LINK_LIBRARIES sut # your application library
  )

# read_model_from_file tests
set(test_read_model_from_file_src ${test_srcs})
list(FILTER test_read_model_from_file_src INCLUDE REGEX ".*test_read_model_from_file.pf")

add_pfunit_ctest (pfunit_read_model_from_file_tests
  TEST_SOURCES ${test_read_model_from_file_src}
  LINK_LIBRARIES sut # your application library
  )

#########################
# MPI tests
#########################

# get_local_grid_info tests
set(test_get_local_grid_info_src ${test_srcs})
list(FILTER test_get_local_grid_info_src INCLUDE REGEX ".*test_get_local_grid_info.pf")

add_pfunit_ctest (pfunit_get_local_grid_info_tests
  TEST_SOURCES ${test_get_local_grid_info_src}
  LINK_LIBRARIES sut # your application library
  MAX_PES 4
  )

# exchange_boundaries tests
set(test_exchange_boundaries_src ${test_srcs})
list(FILTER test_exchange_boundaries_src INCLUDE REGEX ".*test_exchange_boundaries.pf")

add_pfunit_ctest (pfunit_exchange_boundaries_tests
  TEST_SOURCES ${test_exchange_boundaries_src}
  LINK_LIBRARIES sut # your application library
  MAX_PES 4
  )

# find_steady_state tests
set(test_find_steady_state_src ${test_srcs})
list(FILTER test_find_steady_state_src INCLUDE REGEX ".*test_find_steady_state.pf")

add_pfunit_ctest (pfunit_find_steady_state_tests
  TEST_SOURCES ${test_find_steady_state_src}
  LINK_LIBRARIES sut # your application library
  )
