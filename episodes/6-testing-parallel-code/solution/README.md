# Testing parallel code - Solution: Rewrite serial test as a parallel test

## Task 1

> Re-write test_find_steady_state.pf so that it uses the pfunit library instead of funit. We need to make sure that the subroutine
> find_steady_state reaches steady state within the same number of generations as its serial version.

A parallel solution is provided in the form of the file [test_find_steady_state.pf](./test_find_steady_state.pf). This file
contains a pFUnit test module. To use this solution, replace the contents of the serial version of test_find_steady_state.pf with
that of the parallel solution.

## Task 2

> i. Update the [test/Makefile](./test/Makefile) to allow building MPI enabled pFUnit tests and build your new test with make.

To compile MPI enabled pFUnit tests via Make you must ensure you include `-lpfunit` in the **TEST_FLAGS** and add your new test to
the list **tests_TESTS**.

> ii. Make sure your test automatically runs across different numbers of ranks via ctest. You will need to make changes in
> [CMakeLists.txt](./CMakeLists.txt) as well as within the test itself.

In order to run this parallel solution with different numbers of MPI ranks via ctest, you will also need to update CMakeLists.txt
to add the following.

```cmake
add_pfunit_ctest (pfunit_find_steady_state_tests
  TEST_SOURCES "${PROJECT_SOURCE_DIR}/test/test_find_steady_state.pf"
  LINK_LIBRARIES sut # your application library
  MAX_PES 8
)
```

## Task 3

> Add a new test for the function `comms.exchange_boundaries`. This will need to be an MPI enabled test. Due to the nature of
> `comms.exchange_boundaries` different ranks should expect different results depending on its own rank and the total number of
> ranks employed.

A completed test is provided in [test_exchange_boundaries.pf](./test_exchange_boundaries.pf). You will also need to make similar
changes to those done for [Task 2](#task-2) in order to integrate your test with Make or CMake.
