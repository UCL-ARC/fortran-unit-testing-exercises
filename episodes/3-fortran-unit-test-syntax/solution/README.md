# Fortran Unit Test Syntax - Solution: Rewrite veggies tests using test-drive and pFUnit

## Building

The solution provided here is an entirely self-contained project which can be built using FPM or CMake.

> NOTE: pFUnit tests will only be built if CMake is used.

### FPM

```sh
fpm test
```

### CMake

```sh
cmake -B build-cmake -DCMAKE_PREFIX_PATH=</path/to/pFUnit/installed/dir>
cmake --build build-cmake
cd build-cmake
ctest
```

## Tasks

### pFUnit

#### Task 1

> Finish the partially implement tests in test/pfunit.

Completed pFUnit tests can be found in [test/pfunit](./test/pfunit/).

#### Task 2

> Write a completely new pFUnit test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90).

The new test can be found in [test/pfunit/test_find_steady_state.pf](./test/pfunit/test_find_steady_state.pf)

#### Task 3

> Add your new test to the CMake build system.

The test has been added to the end of [test/test-drive/CMakeLists.txt](./test/test-drive/CMakeLists.txt) as a new `add_pfunit_ctest`.

### test-drive

#### Task 1

> Can you implement the unfinished sections in test/test-drive?

Completed test-drive tests can be found in [test/test-drive](./test/test-drive/).

#### Task 2

> Write a completely new test-drive test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90).

The new test can be found in [test/test-drive/test_find_steady_state.pf](./test/test-drive/test_find_steady_state.pf)

#### Task 3

> Add your new test to the FPM build system.

The test has been added to the end of [fpm.toml](fpm.toml).

#### Task 4

> Add your new test to the CMake build system.

The test has been added to [test/test-drive/CMakeLists.txt](./test/test-drive/CMakeLists.txt) as a new line in the `tests` variable.
