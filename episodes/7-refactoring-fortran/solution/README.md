# Introduction to Unit Testing in Fortran - Solution: Identify bad practice for unit testing Fortran

The solution provided here is an entirely self-contained project which can be run, as before, using
the following commands from within this dir.

```bash
cmake -B build
cmake --build build
./build/game-of-life ../models/model-1.dat # Or another data file
```

## Tasks

> Implement the principles described in [the refactoring lesson](#tasks)

1. Replace magic numbers with constants
2. Change of variable name
3. Break large procedures into smaller units
4. Wrap program functionality in procedures
5. Replace repeated code with a procedure
6. Replace global variables with procedure arguments
7. Separate code concepts into files or modules
