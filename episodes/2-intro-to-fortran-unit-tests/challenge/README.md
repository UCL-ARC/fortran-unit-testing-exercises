# Introduction to Unit Testing in Fortran - Challenge: Identify bad practice for unit testing Fortran

Take a look at the [src](./src) code provided. This is an implementation of
[Conway's game of life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). The program
reads in a data file which represents the starting state of the system. The system is then
evolved and printed to the terminal screen for each time step. To build and run the src
code use the following command from within this dir.

```bash
fpm run -- ../models/model-1.dat # Or another dat file
```

## Questions

1. Can you identify the aspects of this Fortran code which make it difficult to unit test?
2. Try to improve the src to make it more unit testable.

A solution is provided in the [solution](./solution/) dir.
