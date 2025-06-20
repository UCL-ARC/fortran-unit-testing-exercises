# Episode 4 - Challenge 1: Debugging a failing unit test

In [src](./src/) there is a module containing a subroutine to transpose a matrix. In [test](./test/) there are tests for this subroutine written with [Veggies](./test/veggies/), [test-drive](./test/test-drive/) and [pFUnit](./test/pfunit/). 

Try running the tests with either FPM or CMake. You should find that some are failing.

>Remember, pFUnit can only be built via CMake and you can only build one of test-drive or Veggies at a time, via CMake.

#### CMake

```sh
cmake -B build-cmake -DCMAKE_PREFIX_PATH=<path/to/pFUnit/build/installed>
cmake --build build-cmake
cd build-cmake
ctest --output-on-failure
```

#### FPM

```sh
fpm test
```

## Tasks

1. Which tests are failing?
2. What is causing these failures? Is it the test(s) or the src code?
3. Fix the tests/src.
