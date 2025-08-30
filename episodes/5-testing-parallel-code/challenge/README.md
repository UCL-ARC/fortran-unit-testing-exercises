# Testing parallel code - Challenge: Rewrite serial test as a parallel test

This exercise aims to teach how to write pFUnit tests which test MPI enabled code and test across multiple numbers of MPI
processes.

## The code

Take a look at the [src](./src) code provided. This is an MPI parallelized implementation of
[Conway's game of life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). The program reads in a data file which represents
the starting state of the system. The system is then evolved and printed to the terminal screen for each time step. To build and
run the src code use the following commands from within this dir.

```bash
cmake -B build-cmake -DCMAKE_PREFIX_PATH=/path/to/pfunit/install
cmake --build build-cmake
./build-cmake/game-of-life ../models/model-1.dat # Or another data file
```

> If you are using the devcontainer, there is an installation of pFUnit at /home/vscode/pfunit/build/installed

## Task

In [test](./test) there are some pFUnit tests. Where necessary, these tests have been re-written from their serial versions to test
the new parallel implementation. However, one of the tests still needs to be adapted for the new implementation.
[test_find_steady_state.pf](./test/test_find_steady_state.pf) is still setup to test the serial version of our src code.

There are some examples of parallel tests in [test_exchange_boundaries.pf](./test/test_exchange_boundaries.pf) and
[test_get_local_grid_info.pf](./test/test_get_local_grid_info.pf).

1. Re-write test_find_steady_state.pf so that it uses the pfunit library instead of funit. We need to make sure that the subroutine
   find_steady_state reaches steady state within the same number of generations as its serial version.

2. Make sure your test automatically runs across different numbers of ranks via ctest. You will need to make changes in
   [CMakeLists.txt](./CMakeLists.txt) as well as within the test itself.
