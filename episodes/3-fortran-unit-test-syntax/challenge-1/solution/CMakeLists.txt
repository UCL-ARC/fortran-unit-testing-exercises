cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "game-of-life"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Conway's game of life"
)

# Define src files
file(GLOB PROJ_SRC_FILES "${PROJECT_SOURCE_DIR}/src/*.f90")

# Build src executables
add_executable("${PROJECT_NAME}" "${PROJ_SRC_FILES}")

enable_testing()

#--------------------------------------#
#               veggies                #
#--------------------------------------#
# add_subdirectory("veggies-test")

#--------------------------------------#
#             test-drive               #
#--------------------------------------#
add_subdirectory("test-drive-test")

#--------------------------------------#
#               pFUnit                 #
#--------------------------------------#
add_subdirectory("pfunit-test")
