message(STATUS "Using pFUnit")

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES})

add_pfunit_ctest (pfunit_test_temp_conversions_exec
    TEST_SOURCES "test_temp_conversions.pf"
    LINK_LIBRARIES sut # your application library
    )
