# Refactoring Fortran - Solution

The solution provided here is an entirely self-contained project which can be run, as before, using
the following commands from within this dir.

```bash
cmake -B build
cmake --build build
./build/game-of-life ../models/model-1.dat # Or another data file
```

## Tasks

> Implement the principles described in [the refactoring lesson](https://github-pages.arc.ucl.ac.uk/fortran-unit-testing-lesson/2-refactor-fortran.html)

1. [Replace magic numbers with constants](https://github.com/UCL-ARC/fortran-unit-testing-exercises/commit/e9765a26a9e368571eb162771cd45cd3933c03c4)
2. [Change of variable name](https://github.com/UCL-ARC/fortran-unit-testing-exercises/commit/30cfcceb1fc80ef236230e21dae574bdebf64c87)
3. [Break large procedures into smaller units](https://github.com/UCL-ARC/fortran-unit-testing-exercises/commit/fb06543e12e217e6f39ffc9df2f13108f64ca7ac)
4. [Wrap program functionality in procedures](https://github.com/UCL-ARC/fortran-unit-testing-exercises/commit/ee860cac1cc2b1c2f0f2ed99b2fe26060e576ce4)
5. [Replace repeated code with a procedure](https://github.com/UCL-ARC/fortran-unit-testing-exercises/commit/da18b6af1a01c82235975ed1589d0496cf6b23f2)
6. [Replace global variables with procedure arguments](https://github.com/UCL-ARC/fortran-unit-testing-exercises/commit/02da8d0b614d7d7412a066fbdd3c249eb308f5a9)
7. [Separate code concepts into files or modules](https://github.com/UCL-ARC/fortran-unit-testing-exercises/commit/b4c6afcdb2e3a37c602051966e55ad764cdb6203)
