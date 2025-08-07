# Introduction to unit testing - Solution: Unit test bad practices

## Task 1

> Can you identify the aspects of this test which make it a bad unit test?

There are several issues with `maths_test` 

- Our test function `test_maths` is not **minimal** as it is testing the combination of calling both
  `maths::double` and then `maths::factorial`. 
- Currently, we are only testing one scenario where the input is `2`. This means there is likely to
  be only very small coverage of our src code.

## Task 2

> What changes would improve this unit test?

- To ensure the unit tests remain **minimal**, we should split the single test `test_maths` into two
  unit tests, one for testing `maths::double` and another for testing `maths::factorial`.
- To ensure we have good test coverage of our src code, we should make our test more generic so that
  we can parameterize it and test many input values, including edge cases.

## Task 3

> If you are familiar with veggies, try to implement your suggested changes.

The solution here is a single test file [maths_test_solution.f90](./maths_test_solution.f90) written
using Veggies. To use this solution, replace the contents of [challenge/test/maths_test.f90](../challenge/test/maths_test.f90) with the solution. Then run the tests as before with... 

```sh
fpm test
```
