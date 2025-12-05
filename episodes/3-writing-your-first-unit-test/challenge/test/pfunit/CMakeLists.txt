message(STATUS "Using pFUnit")

set(TEST_DIR "${PROJECT_SOURCE_DIR}/test/pfunit")

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES})

# List all test files
set(test_srcs "test_temp_conversions.pf")

add_pfunit_ctest (pfunit_test_temp_conversions_exec
    TEST_SOURCES ${test_srcs}
    LINK_LIBRARIES sut # your application library
    )

