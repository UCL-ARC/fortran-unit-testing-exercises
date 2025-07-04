# Episode 3 - Challenge 1 - Solution: Rewrite veggies tests using test-drive and pFUnit

This solution can be built with either FPM or CMake.

>NOTE: pFUnit tests will only be built and run if CMake is used.

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
