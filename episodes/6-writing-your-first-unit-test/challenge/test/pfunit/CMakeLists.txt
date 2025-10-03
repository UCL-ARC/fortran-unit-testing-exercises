message(STATUS "Using pFUnit")

set(TEST_DIR "${PROJECT_SOURCE_DIR}/test/pfunit")

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES})

# List all test files
file(GLOB
    PROJ_TEST_FILES
    "${TEST_DIR}/*.pf"
)

list(LENGTH test_srcs num_test_srcs)
if (num_test_srcs EQUAL 0)
    message(FATAL_ERROR "No .pf files found in: ${TEST_DIR}")
endif()

add_pfunit_ctest (pfunit_test_temp_conversions_exec
    TEST_SOURCES ${test_srcs}
    LINK_LIBRARIES sut # your application library
    )

