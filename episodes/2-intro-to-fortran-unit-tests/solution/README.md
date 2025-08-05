# Introduction to Unit Testing in Fortran - Solution: Identify bad practice for unit testing Fortran

The solution provided here is an entirely self-contained project which can be built using FPM.

```bash
fpm build
```

Tests are also provided, which can be run using FPM.

```bash
fpm test
```

## Question 1 - Can you identify the aspects of this Fortran code which make it difficult to unit test?

There are several issues with this Fortran code which make it hard to unit test. Find the suggested fixes listed below.

1. Everything is containing within a single program. This prevents us from using individual procedures within test modules.
   Effectively preventing us from testing them.

2. There is a lot of global state used across multiple procedures. This makes tests dependent on one another therefore
   complicating the management of data/state between tests.

3. There is a lot of logic not contained within procedures. Wrapping this in procedures opens up more of the code which can be
   tested.

## Question 2 - Try to improve the src to make it more unit testable.

An example implementation of an improved src code is provided in [src](./src/). Comments beginning with `FIX_<fix_number>`
are provided above the implemented fixes for each of the numbered list items above.
