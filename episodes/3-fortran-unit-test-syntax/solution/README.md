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

## Task 1

> Can you implement the unfinished sections in test/pfunit?

Completed pFUnit tests can be found in [test/pfunit](./test/pfunit/).

## Task 2

> Can you implement the unfinished sections in test/test-drive?

Completed test-drive tests can be found in [test/test-drive](./test/test-drive/).
