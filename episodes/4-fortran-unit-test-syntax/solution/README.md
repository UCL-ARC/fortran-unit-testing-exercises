<!-- markdownlint-disable MD029 -->
# Fortran Unit Test Syntax - Solution: Write tests using pFUnit

## Building

The solution provided here is an entirely self-contained project which can be built using CMake.

```bash
cmake -B build -DCMAKE_PREFIX_PATH=/path/to/pfunit/install
cmake --build build
./build/game-of-life ../models/model-1.dat # Or another data file
```

## Tasks

> 1. Finish the partially implemented tests in test.

Completed pFUnit tests can be found in [test](./test/).

> 2. Write a completely new pFUnit test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90).

The new test can be found in [test/test_find_steady_state.pf](./test/test_find_steady_state.pf)

> 3. Add your new test to the CMake build system.

The test has been added to the end of [test/CMakeLists.txt](./test/CMakeLists.txt) as a new `add_pfunit_ctest`.
