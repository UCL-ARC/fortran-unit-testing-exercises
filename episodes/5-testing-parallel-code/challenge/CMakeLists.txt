cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "game-of-life"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Conway's game of life"
)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")

# OpenMP
find_package(OpenMP)
find_package(MPI COMPONENTS Fortran REQUIRED)

# Define src files
file(GLOB PROJ_SRC_FILES "${PROJECT_SOURCE_DIR}/src/*.f90")

# Build src executables
add_executable("${PROJECT_NAME}" "${PROJ_SRC_FILES}")

set(TO_LINK_TO_SRC MPI::MPI_Fortran)
if(OpenMP_Fortran_FOUND)
  list(APPEND TO_LINK_TO_SRC OpenMP::OpenMP_Fortran)
endif()

target_link_libraries("${PROJECT_NAME}" "${TO_LINK_TO_SRC}")

enable_testing()

#--------------------------------------#
#               pFUnit                 #
#--------------------------------------#
add_subdirectory("test")
