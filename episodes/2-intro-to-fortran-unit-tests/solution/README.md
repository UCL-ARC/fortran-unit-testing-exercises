# Introduction to Unit Testing in Fortran - Solution: Identify bad practice for unit testing Fortran

The solution provided here is an entirely self-contained project which can be built using FPM.

```bash
fpm build
```

Tests are also provided, which can be run using FPM.

```bash
fpm test
```

## Task 1

> Can you identify the aspects of this Fortran code which make it difficult to unit test?

There are several issues with this Fortran code which make it hard to unit test. Find the suggested fixes listed below.

1. Everything is contained within a single program. This prevents us from using individual procedures within test modules.
   Effectively preventing us from testing them.

2. There is a lot of global state used across multiple procedures. This makes tests dependent on one another therefore
   complicating the management of data/state between tests.

3. There is a lot of logic not contained within procedures. Wrapping this in procedures opens up more of the code which can be
   tested.

## Task 2

> Try to improve the src to make it more unit testable.

1. To allow procedures to be tested, they have been moved out of the program into the module file
   [game_of_life_mod.f90](./src/game_of_life_mod.f90) which can now be used within a test.

2. The reliance on global state has been removed by passing all required values into each procedure at the point it is called.

3. Logic originally in main program code has been wrapped into procedures allowing it to be tested. For example, there ar two 
   new procedures `find_steady_state` and `read_model_from_file`.
