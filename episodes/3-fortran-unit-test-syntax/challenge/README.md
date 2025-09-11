# Fortran Unit Test Syntax - Challenge: Write tests using pFUnit, test-drive and Veggies

This exercise aims to teach how to write unit tests using [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit),
[test-drive](https://github.com/fortran-lang/test-drive) and [veggies](https://gitlab.com/everythingfunctional/veggies)

## The code

Take a look at the [src](./src) code provided. This is an implementation of
[Conway's game of life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). The program reads in a data file which represents
the starting state of the system. The system is then evolved and printed to the terminal screen for each time step. To build and
run the src code use the following commands from within this dir.

```bash
cmake -B build-cmake -DCMAKE_PREFIX_PATH=/path/to/pfunit/install
cmake --build build-cmake
./build-cmake/game-of-life ../models/model-1.dat # Or another data file
```

> If you are using the devcontainer, there is an installation of pFUnit at /home/vscode/pfunit/build/installed

## Tasks

### pFUnit

1. Finish the partially implemented tests in [test/pfunit](./test/pfunit/).
    - [test_check_for_steady_state.pf](./test/pfunit/test_check_for_steady_state.pf)
    - [test_read_model_from_file.pf](./test/pfunit/test_read_model_from_file.pf)
2. Write a completely new pFUnit test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90).
3. Add your new test to the CMake build system.

### test-drive

1. Finish the partially implemented tests in [test/test-drive](./test/test-drive/).
    - [test_check_for_steady_state.f90](./test/test-drive/test_check_for_steady_state.f90)
    - [test_read_model_from_file.f90](./test/test-drive/test_read_model_from_file.f90)
2. Write a completely new test-drive test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90)
3. Add your new test to the FPM build system.
4. Add your new test to the CMake build system.

### Veggies

1. Finish the partially implemented tests in [test/veggies](./test/veggies/).
    - [test_check_for_steady_state.f90](./test/veggies/test_check_for_steady_state.f90)
    - [test_read_model_from_file.f90](./test/veggies/test_read_model_from_file.f90)
2. Write a completely new Veggies test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90)
3. Add your new test to the FPM build system.
4. Add your new test to the CMake build system.
