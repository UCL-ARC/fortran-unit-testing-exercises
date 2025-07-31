find_package(PFUNIT REQUIRED)
enable_testing()

# Filter out the main.f90 files. We can only have one main() function in our tests
set(PROJ_SRC_FILES_EXEC_MAIN ${PROJ_SRC_FILES})
list(FILTER PROJ_SRC_FILES_EXEC_MAIN EXCLUDE REGEX ".*main.f90")

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES_EXEC_MAIN})

# List all  test files
file(GLOB
  test_srcs
  "${PROJECT_SOURCE_DIR}/test/pfunit/*.pf"
)

# evolve_board tests
set(test_transpose_src ${test_srcs})
list(FILTER test_transpose_src INCLUDE REGEX ".*test_transpose.pf")

add_pfunit_ctest (pfunit_transpose_tests
  TEST_SOURCES ${test_transpose_src}
  LINK_LIBRARIES sut # your application library
  )
