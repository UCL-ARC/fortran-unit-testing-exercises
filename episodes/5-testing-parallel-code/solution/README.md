# Episode 5 - Solution: Rewrite serial test as a parallel test.

A parallel solution is provided in the form of the file
[test_find_steady_state.pf](./test_find_steady_state.pf). This file contains
a pFUnit test module. To use this solution, replace the contents of the serial
version of test_find_steady_state.pf with that of the parallel solution. 

In order to run this parallel solution with different numbers of MPI ranks via
ctest, you will also need to update CMakeLists.txt with the following change.

```diff
--- a/episodes/5-testing-parallel-code/challenge/test/CMakeLists.txt
+++ b/episodes/5-testing-parallel-code/challenge/test/CMakeLists.txt
@@ -89,4 +89,5 @@ list(FILTER test_find_steady_state_src INCLUDE REGEX ".*test_find_steady_state.p
 add_pfunit_ctest (pfunit_find_steady_state_tests
   TEST_SOURCES ${test_find_steady_state_src}
   LINK_LIBRARIES sut # your application library
+  MAX_PES 8
   )
```
