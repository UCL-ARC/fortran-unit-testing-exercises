# Episode 3 - Challenge 1 - Solution: Rewrite veggies tests using test-drive and pFUnit

This solution can be built with either FPM or CMake.

>NOTE: pFUnit tests will only be built if CMake is used.

#### FPM

```sh
fpm test
```

#### CMake

```sh
cmake -B build-cmake -DCMAKE_PREFIX_PATH=</path/to/pFUnit/installed/dir>
cmake --build build-cmake
cd build-cmake
ctest
```

## Question 1 - Can you implement the unfinished sections in pfunit-test?

Completed pFUnit tests can be found in [pfunit-test](./pfunit-test/).

## Question 2 - Can you implement the unfinished sections in test-drive-test?

Completed test-drive tests can be found in [test-drive-test](./test-drive-test/).
