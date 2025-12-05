# Writing your first unit test - Standard Fortran

This exercise aims to teach the principles of unit testing and how to write a good
unit test. The tests within this challenge are intended to be written using standard
Fortran without the use of a testing framework, in order to teach the principles alone.

## The src code

In [src](./src) you will find a library [temp_conversions.f90](./src/temp_conversions.f90)
which provides functions for converting between various units of temperature. The functions
provided are...

- **fahrenheit_to_celsius**: Which takes in a temperature in Fahrenheit and returns a temperature in Celsius.
- **celsius_to_kelvin**: Which takes in a temperature in Celsius and returns a temperature in Kelvin.

## The tasks

### Part 1 - Test with Standard Fortran

Imagine you wish to use the temp_conversions library to convert Fahrenheit to Kelvin. We
know that there is no function which does this direct conversion. With this is mind, write
a test, or tests, to give you confidence that temp_conversions can correctly convert
Fahrenheit to Kelvin.

To get you started, the file [test_temp_conversions.f90](./test/standard_fortran/test_temp_conversions.f90)
has been provided. `test_temp_conversions.f90` contains some boilerplate to make writing a
test easier. There is an empty test subroutine `test` provided which takes in a logical
`passed` and a character array `failure_message`. The logical `passed` should indicate if
the test was successful. The character array `failure_message`, should be populated with a
message that will be printed to the terminal in the event that `passed` is `.false.`. Once
the test subroutine is written it should be called within the main body of the test program
as indicated in `test_temp_conversions.f90`.

> Not: If you add a new test file or change the name of `test_temp_conversions.f90`, you will
> need to update list of tests (`test_src`) in [test/pfunit/CMakeLists.txt](./test/pfunit/CMakeLists.txt)

### Part 2 - Convert tests to use pFUnit

Convert your tests from [Part 1](#part-1---test-with-standard-fortran), to use
[pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit).

A file [test_temp_conversions.pf](./test/pfunit/test_temp_conversions.pf) containing a template
for your pFUnit test(s) has been provided. Comments within this file indicate the aspects of
the pFUnit test you must write.

> Note: This template has been written to facilitate conversion of
> [test_temp_conversions.f90](./test/standard_fortran/test_temp_conversions.f90) as provided with this repo
> to pFUnit. If your version of test_temp_conversions.f90, produced in Part 1, is significantly
> different, You may prefer to use a different structure to the one provided in the template.

To build and run your pFUnit test(s) you must add the pFUnit lib to the `CMAKE_PREFIX_PATH`
when building via the following command.

```bash
cmake -B build -DCMAKE_PREFIX_PATH=/path/to/pfunit/install
cmake --build build
ctest --test-dir build --output-on-failure
```

If your test does not get built, ensure you have added it to the list of tests (`test_src`)
in [test/pfunit/CMakeLists.txt](./test/pfunit/CMakeLists.txt)

> If you are using the devcontainer, there is an installation of pFUnit at /home/vscode/pfunit/build/installed
