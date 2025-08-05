# Introduction to unit testing - Challenge: Unit test bad practices

Take a look at the [src](./src/maths.f90) and [test](./test/maths_test.f90) code provided. 

1. Can you identify the aspects of this test which make it a bad unit test?
2. What changes would improve this unit test?
3. If you are familiar with veggies, try to implement your suggested changes.

> **Note:** These tests are written using [veggies](https://gitlab.com/everythingfunctional/veggies). To allow the tests to run there is extra boilerplate code already written. However, the only file you should need to update is [test/maths_test.f90](./test/maths_test.f90).

## Running the tests

An [fpm.toml](./fpm.toml) is provided to make running these test easier. From within the challenge-2 directory, run the command

```sh
fpm test
```
