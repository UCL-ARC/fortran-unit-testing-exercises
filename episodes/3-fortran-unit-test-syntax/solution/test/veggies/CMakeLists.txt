# Install the test-drive dependency
set(veggies_url "https://gitlab.com/everythingfunctional/veggies")

message(STATUS "Retrieving veggies from ${veggies_url}")
include(FetchContent)
FetchContent_Declare(
    "veggies"
    GIT_REPOSITORY "${veggies_url}"
    GIT_TAG "HEAD"
    )
FetchContent_MakeAvailable("veggies")

# Ignore tests from test-drive itself
configure_file(${CMAKE_SOURCE_DIR}/test/veggies/CTestCustom.cmake ${CMAKE_BINARY_DIR})

# Ensure test data is available at runtime
file(CREATE_LINK "${PROJECT_SOURCE_DIR}/test/models"
        "${CMAKE_CURRENT_BINARY_DIR}/test-models" SYMBOLIC)

# Define test files
set(PROJ_VEGGIES_DIR "${PROJECT_SOURCE_DIR}/test/veggies")

# Filter out the main.f90 files. We can only have one main() function in our tests
set(PROJ_SRC_FILES_EXEC_MAIN ${PROJ_SRC_FILES})
list(FILTER PROJ_SRC_FILES_EXEC_MAIN EXCLUDE REGEX ".*game_of_life.f90")

# Unit testing
set(
  tests
  "evolve_board"
  "check_for_steady_state"
  "find_steady_state"
  "read_model_from_file"
)
set(
  test-srcs
  "main.f90"
)
foreach(t IN LISTS tests)
  string(MAKE_C_IDENTIFIER ${t} t) 
  list(APPEND test-srcs "${PROJ_VEGGIES_DIR}/test_${t}.f90")
endforeach()

add_executable(
  "test_${PROJECT_NAME}-veggies"
  "${test-srcs}"
  "${PROJ_SRC_FILES_EXEC_MAIN}"
)
target_link_libraries(
  "test_${PROJECT_NAME}-veggies"
  PRIVATE
  "veggies"
)

foreach(t IN LISTS tests)
  add_test(NAME "veggies_${t}" COMMAND "test_${PROJECT_NAME}-veggies" "-f" "${t}")
endforeach()
