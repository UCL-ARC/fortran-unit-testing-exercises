cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "game-of-life"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Conway's game of life"
)

# Define src files
set(SRC_DIR "${PROJECT_SOURCE_DIR}/src")
set(PROJ_SRC_FILES 
  "${SRC_DIR}/main.f90"
  "${SRC_DIR}/animation.f90"
  "${SRC_DIR}/cli.f90"
  "${SRC_DIR}/game_of_life.f90"
  "${SRC_DIR}/io.f90"
)

# Build src executables
add_executable("${PROJECT_NAME}" "${PROJ_SRC_FILES}")
