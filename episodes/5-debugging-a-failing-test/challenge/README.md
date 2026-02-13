# Understanding test output - Challenge: Debugging a failing unit test

This exercise aims to teach how to identify a failing unit test and translate this failure output to a fix for the src code.

## The code

In [src](./src/) there is a module containing a subroutine to transpose a matrix. In [test](./test/) there are tests for this
subroutine written with [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit).

Try running the tests with CMake. You should find that some are failing.

```sh
cmake -B build -DCMAKE_PREFIX_PATH=<path/to/pFUnit/build/installed>
cmake --build build
ctest --test-dir build --output-on-failure
```

## Tasks

1. Which tests are failing?
2. What is causing these failures? Is it the test(s) or the src code?
3. Fix the tests/src.
