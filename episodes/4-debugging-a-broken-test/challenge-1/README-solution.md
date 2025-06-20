# Episode 4 - Challenge 1 - Solution: Debugging a failing unit test

## 1. Which tests are failing?

The error messages from each of the frameworks shown below are all pointing to the same
issue. The test that checks that a 3x3 asymmetric matrix is transposed is failing.

#### Veggies

```
Running Tests

Test that
    transpose
        a matrix is transposed as expected

A total of 1 test cases

Failed
Took 4.47e-4 seconds

Test that
    transpose
        a matrix is transposed as expected
            Expected
                    |[[1.0, 4.0, 0.0],
                      [6.0, 1.0, 0.0],
                      [0.0, 3.0, 1.0]]|
                to be within |±1.0e-5| of
                    |[[1.0, 6.0, 0.0],
                      [4.0, 1.0, 3.0],
                      [0.0, 0.0, 1.0]]|

1 of 1 cases failed
1 of 2 assertions failed
```

#### test-drive

```
# Running testdrive tests suite
# Testing: transpose
  Starting 3x3 identity matrix ... (1/2)
       ... 3x3 identity matrix [PASSED]
  Starting 3x3 asymmetric matrix ... (2/2)
       ... 3x3 asymmetric matrix [FAILED]
  Message: Unexpected value for output(1,2), got 4.0 expected 6.0                          
1 test(s) failed!
ERROR STOP 1

Error termination. Backtrace:
#0  0x10079ad2f
#1  0x10079b8d7
#2  0x10079cb1f
#3  0x100153503
#4  0x100153973
#5  0x1001539b7
```


#### pFUnit

```
    Start 2: pfunit_transpose_tests
1/1 Test #2: pfunit_transpose_tests ...........***Failed  Error regular expression found in output. Regex=[Encountered 1 or more failures/errors during testing]  0.01 sec
 

 Start: <test_transpose_suite.TestTranspose[3x3 Identity][3x3 Identity]>
.   end: <test_transpose_suite.TestTranspose[3x3 Identity][3x3 Identity]>
 

 Start: <test_transpose_suite.TestTranspose[3x3 Asymmetric][3x3 Asymmetric]>
. Failure in <test_transpose_suite.TestTranspose[3x3 Asymmetric][3x3 Asymmetric]>
F   end: <test_transpose_suite.TestTranspose[3x3 Asymmetric][3x3 Asymmetric]>

Time:         0.000 seconds
  
Failure
 in: 
test_transpose_suite.TestTranspose[3x3 Asymmetric][3x3 Asymmetric]
  Location: 
[test_transpose.pf:59]
ArrayAssertEqual failure:
      Expected: <4.00000000>
        Actual: <6.00000000>
    Difference: <2.00000000> (greater than tolerance of 0.999999975E-5)
      at index: [2,1]
  
 FAILURES!!!
Tests run: 2, Failures: 1, Errors: 0
, Disabled: 0
STOP *** Encountered 1 or more failures/errors during testing. ***


0% tests passed, 1 tests failed out of 1

Total Test time (real) =   0.02 sec

The following tests FAILED:
          2 - pfunit_transpose_tests (Failed)
Errors while running CTest
```

## 2. What is causing these failures? Is it the test(s) or the src code?

The tests appear to be correct as we are inputting

```F90
input_matrix(1,:) = [1,4,0]
input_matrix(2,:) = [6,1,0]
input_matrix(3,:) = [0,3,1]
```

and expect the transposed version to be

```F90
expected_output_matrix(1,:) = [1,6,0]
expected_output_matrix(2,:) = [4,1,3]
expected_output_matrix(3,:) = [0,0,1]
```

Therefore, the problem likely lies in the src code itself. Taking a closer look at `matrix_transforms::transpose`
we can see the issue. When we are transposing `temp_matrix` into `matrix` we are not flipping the order of the
indices, but this is required to populate the transpose. This also explains why the test(s) of the 3x3 identity
matrix are not failing as the identity is a symmetric matrix and is equal to its own transpose, thus the order
of the indices would not matter.

## 3. Fix the tests/src.

We can fix the src with the following change

```diff
--- a/episodes/4-debugging-a-broken-test/challenge-1/src/matrix_transforms.f90
+++ b/episodes/4-debugging-a-broken-test/challenge-1/src/matrix_transforms.f90
@@ -20,7 +20,7 @@ contains
 
         do row = 1, nrow
             do col = 1, ncol
-                matrix(row, col) = temp_matrix(row, col)
+                matrix(col, row) = temp_matrix(row, col)
             end do
         end do
     end subroutine transpose
```
