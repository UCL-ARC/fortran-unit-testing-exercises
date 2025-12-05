find_package(PFUNIT QUIET)

add_subdirectory("standard_fortran")

if (PFUNIT_FOUND)
    add_subdirectory("pfunit")
endif()
