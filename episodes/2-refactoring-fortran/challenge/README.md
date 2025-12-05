# Refactoring Fortran

This exercise aims to teach principles of writing better Fortran; that is Fortran which is clear, maintainable and testable.

To do this, there is a [src](./src) code provided which has been written using bad practice. The task is to refactor this src
using best practice principles so that the result is a more testable code but the actual behaviour of the program unchanged.

## The src code

Take a look at the [src](./src) code provided. This is an implementation of
[Conway's game of life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). The program reads in a data file which represents
the starting state of the system. The system is then evolved and printed to the terminal screen for each time step. To build and
run the src code use the following commands from within this dir.

```bash
cmake -B build
cmake --build build
./build/game-of-life ../models/model-1.dat # Or another data file
```

## Tasks

Implement the principles described in [the refactoring lesson](https://github-pages.arc.ucl.ac.uk/fortran-unit-testing-lesson/2-refactor-fortran.html).

To ensure you are not changing the actual behaviour of the src code, every time you make a change,
compare the output before and after. To do this store the output before making a change in a file
called `before.dat`

```sh
./build/game-of-life path/to/model/file > before.dat
```

Then, after you make a change regenerate the output and store within a file `after.dat`

```sh
./build/game-of-life path/to/model/file > after.dat
```

If you have successfully not changed the behaviour there should be no output from the command

```sh
diff before.dat after.dat
```
