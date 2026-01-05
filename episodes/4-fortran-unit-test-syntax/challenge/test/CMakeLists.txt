find_package(PFUNIT REQUIRED)
enable_testing()

# Ensure test data is available at runtime
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test")
file(CREATE_LINK "${PROJECT_SOURCE_DIR}/test/models"
        "${CMAKE_CURRENT_BINARY_DIR}/test/models" SYMBOLIC)

# Create library for src code
add_library (sut STATIC ${PROJ_SRC_FILES})

# evolve_board tests
# Your changes here...

# check_for_steady_state tests
# Your changes here...

# read_model_from_file tests
# Your changes here...

# find_steady_state tests
# Your changes here...
