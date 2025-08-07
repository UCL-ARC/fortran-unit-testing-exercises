# Testing parallel code - Challenge: Rewrite serial test as a parallel test

This exercises aims to teach how to write pFUnit tests which test MPI enabled code and test across multiple numbers of MPI
processes.

## The code

In [src](./src) there is a hybrid MPI-OpenMP parallel implementation of the Conway's game of life code we have seen in previous
exercises.

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
