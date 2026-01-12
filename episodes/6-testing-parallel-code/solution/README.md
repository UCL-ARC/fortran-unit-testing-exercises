# Testing parallel code - Solution: Rewrite serial test as a parallel test

## Task 1

> Re-write test_find_steady_state.pf so that it uses the pfunit library instead of funit.

A parallel solution is provided in the form of the file [test_find_steady_state.pf](./test_find_steady_state.pf). This file
contains a pFUnit test module. To use this solution, replace the contents of the serial version of test_find_steady_state.pf with
that of the parallel solution.

## Task 2

> Make sure your test automatically runs across different numbers of ranks via ctest

In order to run this parallel solution with different numbers of MPI ranks via ctest, you will also need to update CMakeLists.txt
with the following change.

```diff
--- a/episodes/6-testing-parallel-code/challenge/test/CMakeLists.txt
+++ b/episodes/6-testing-parallel-code/challenge/test/CMakeLists.txt
@@ -62,4 +62,5 @@ add_pfunit_ctest (pfunit_exchange_boundaries_tests
 add_pfunit_ctest (pfunit_find_steady_state_tests
   TEST_SOURCES "${PROJECT_SOURCE_DIR}/test/test_find_steady_state.pf"
   LINK_LIBRARIES sut # your application library
+  MAX_PES 8
   )
```
