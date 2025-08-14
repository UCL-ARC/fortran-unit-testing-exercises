# Fortran Unit Test Syntax - Challenge: Rewrite veggies tests using test-drive and pFUnit

This exercise aims to teach how to write unit tests using [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit),
[test-drive](https://github.com/fortran-lang/test-drive) and [veggies](https://gitlab.com/everythingfunctional/veggies)

## The code

In [src](./src/) there is a copy of [the src from the episode-2 solution](../../2-intro-to-fortran-unit-tests/solution/src/).
You will also find a copy of [the veggies tests from the episode-2 solution](../../2-intro-to-fortran-unit-tests/solution/test/)
in [test/veggies](./test/veggies/)

## Tasks

Now, take a look at the partially implemented tests within [test/pfunit](./test/pfunit/) and [test/test-drive](./test/test-drive/).
The sections to be completed have been labelled by comments prefixed with `TASK`.

### pFUnit

1. Finish the partially implement tests in [test/pfunit](./test/pfunit/).
    - [test_check_for_steady_state.pf](./test/pfunit/test_check_for_steady_state.pf)
    - [test_read_model_from_file.pf](./test/pfunit/test_read_model_from_file.pf)
2. Write a completely new pFUnit test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90).
3. Add your new test to the CMake build system.

### test-drive

1. Finish the partially implement tests in [test/test-drive](./test/test-drive/).
    - [test_check_for_steady_state.pf](./test/test-drive/test_check_for_steady_state.pf)
    - [test_read_model_from_file.pf](./test/test-drive/test_read_model_from_file.pf)
2. Write a completely new test-drive test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90)
3. Add your new test to the FPM build system.
4. Add your new test to the CMake build system.

### Veggies

1. Finish the partially implement tests in [test/veggies](./test/veggies/).
    - [test_check_for_steady_state.pf](./test/veggies/test_check_for_steady_state.pf)
    - [test_read_model_from_file.pf](./test/veggies/test_read_model_from_file.pf)
2. Write a completely new Veggies test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90)
3. Add your new test to the FPM build system.
4. Add your new test to the CMake build system.
