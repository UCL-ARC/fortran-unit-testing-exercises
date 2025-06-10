# Episode 2 - Challenge 1 - Solution: Identify bad practice for unit testing Fortran

The solution provided here is an entirely self-contained project which can be built using FPM.

```bash
fpm build
```

Tests are also provided, which can be run using FPM.

```bash
fpm test
```

## Question 1

>Can you identify the aspects of this Fortran code which make it difficult to unit test?

There are several issues with this Fortran code which make it hard to unit test. Find the suggested fixes listed below.

1. Everything is containing within a single program. This prevents us from using individual procedures within test modules. Effectively preventing us from testing them.

2. There is a lot of global state used across multiple procedures. This makes tests dependent on one another therefore complicating the management of data/state between tests.

3. TODO: There is a lot of logic not contained within procedures. Wrapping this in a procedure opens up more of the code which can be tested.
