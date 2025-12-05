cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "game-of-life"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Conway's game of life"
)

# Define src files
set(PROJ_SRC_FILES "${PROJECT_SOURCE_DIR}/src/game_of_life.f90")

# Build src executables
add_executable("${PROJECT_NAME}" "${PROJ_SRC_FILES}")
