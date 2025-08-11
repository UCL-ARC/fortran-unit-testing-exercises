# Introduction to unit testing - Challenge: Unit test bad practices

This exercise aims to highlight aspects that define a good unit test.

## Tasks

Take a look at the [src](./src/maths.f90) and [test](./test/maths_test.f90) code provided.

1. Can you identify the aspects of this test which make it a bad unit test?
2. What changes would improve this unit test?
3. Try to implement your suggested changes.

These tests are written without using any test framework just pure fortran. The only file you should need to update is
[test/test_maths.f90](./test/test_maths.f90).

## Running the tests

An [fpm.toml](./fpm.toml) is provided to make running these test easier. From
within the challenge directory, run the command

```sh
fpm test
```
