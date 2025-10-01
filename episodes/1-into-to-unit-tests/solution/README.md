# Introduction to unit testing - Solution: Unit test bad practices

## Task 1

> Can you identify the aspects of this test which make it a bad unit test?

There are several issues with `test_maths.f90`

- Our test function `test` is not **minimal** as it is testing the combination of calling both `maths::double` and then
  `maths::factorial`.
- Currently, we are only testing one scenario where the input is `2`. This means there is likely to be only very small coverage of
  our src code.

## Task 2

> What changes would improve this unit test?

- To ensure the unit tests remain **minimal**, we should split the single test `test` into two unit tests, one for testing
  `maths::double` and another for testing `maths::factorial`.
- To ensure we have good test coverage of our src code, we should make our test more generic so that we can parameterize it and
  test many input values, including edge cases.

## Task 3

> Try to implement your suggested changes.

The solution here is a single test file [test_maths_solution.f90](./test_maths_solution.f90). To use this solution, replace the
contents of [challenge/test/test_maths.f90](../challenge/test/test_maths.f90) with the solution. Then run the tests as before
with...

```sh
cmake -B build
cmake --build build
ctest --test-dir build --output-on-failure
```
