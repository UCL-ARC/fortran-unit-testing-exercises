# Writing your first unit test - Standard Fortran

This exercise aims to teach the principles of unit testing and how to write a good
unit test. The tests within this challenge are intended to be written using standard
Fortran without the use of a testing framework, in order to teach the principles alone.

## The src code

in [src](./src) you will find a library [temp_conversions.f90](./src/temp_conversions.f90)
which provides functions for converting between various units of temperature. The functions
provided are...

- **fahrenheit_to_celsius**: Which takes in a temperature in Fahrenheit and returns a temperature in Celsius.
- **celsius_to_kelvin**: Which takes in a temperature in Celsius and returns a temperature in Kelvin.

## The task

Imagine you wish to use the temp_conversions library to convert Fahrenheit to Kelvin. We
know that there is no function which does this direct conversion. With this is mind, write
a test, or tests, to give you confidence that temp_conversions can correctly convert
Fahrenheit to Kelvin.

To get you started, the file [test_temp_conversions.f90](./test/test_temp_conversions.f90)
has been provided. `test_temp_conversions.f90` contains some boilerplate to make writing a
test easier. There is an empty test subroutine `test` provided which takes in a logical
`passed` and a character array `failure_message`. The logical `passed` should indicate if
the test was successful. The character array `failure_message`, should be populated with a
message that will be printed to the terminal in the event that `passed` is `.false.`. Once
the test subroutine is written it should be called within the main body of the test program
as indicated in `test_temp_conversions.f90`.
