# Episode 4 - Challenge 1 - Solution: Debugging a failing unit test

## 1. Which tests are failing?

The error messages from each of the frameworks (Veggies, test-drive and pFUnit) should show you that they are
all pointing to the same issue. The test that checks that a 3x3 asymmetric matrix is transposed is failing.

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
