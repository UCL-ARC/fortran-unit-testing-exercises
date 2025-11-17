find_package(PFUNIT QUIET)

if (PFUNIT_FOUND)
    add_subdirectory("pfunit")
else()
    add_subdirectory("standard_fortran")
endif()
