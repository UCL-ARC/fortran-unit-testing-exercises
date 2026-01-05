# Fortran Unit Test Syntax - Challenge: Write tests using pFUnit

This exercise aims to teach how to write unit tests using [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit).

## The code

Take a look at the [src](./src) code provided. This is an implementation of
[Conway's game of life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). The program reads in a data file which represents
the starting state of the system. The system is then evolved and printed to the terminal screen for each time step. To build and
run the src code use the following commands from within this dir.

```bash
cmake -B build -DCMAKE_PREFIX_PATH=/path/to/pfunit/install
cmake --build build
./build/game-of-life ../models/model-1.dat # Or another data file
```

> If you are using the devcontainer, there is an installation of pFUnit at /home/vscode/pfunit/build/installed

## Tasks

1. Update the [test/CMakeLists.txt](./test/CMakeLists.txt) to build the fully implemented test, **test_evolve_board.pf**.
2. Finish the partially implemented tests in [test](./test/) and ensure they are built via CMake.
    - [test_check_for_steady_state.pf](./test/test_check_for_steady_state.pf)
    - [test_read_model_from_file.pf](./test/test_read_model_from_file.pf)
3. Write a completely new pFUnit test for the subroutine `find_steady_state` in [game_of_life_mod.f90](./src/game_of_life_mod.f90)
   and ensure it is built via CMake.
