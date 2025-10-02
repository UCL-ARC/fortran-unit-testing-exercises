# Writing your first unit test - Standard Fortran - Solution

The solution is provided in the form of a single test file [test_temp_conversions.f90](./test_temp_conversions.f90)
which replaces the file of the same name provided in the [challenge/test](../challenge/test/) directory.

## Key points

There are several key aspects within the solution that are important to implement in any test.

### Isolated test subroutine

Each test subroutine in [test_temp_conversions.f90](./test_temp_conversions.f90) calls and tests only one src
function. For example, `test_fahrenheit_to_celsius` calls and tests `fahrenheit_to_celsius` and
`test_celsius_to_kelvin` calls and tests `celsius_to_kelvin`.

This is important as, in the event of a test failing it will be clear which src function is the cause of the failure.
If instead we had implemented a single test subroutine which calls both `test_fahrenheit_to_celsius` and
`fahrenheit_to_celsius`, then a failure in such a test could have been caused by either of those src functions and further
investigation would be required.

### Parameterised tests

Each test subroutine in [test_temp_conversions.f90](./test_temp_conversions.f90) take in an `input` and an `expected_output`.
This allows the same test subroutine to be called with multiple different inputs to test several scenarios with the same test
code. Therefore, we are able to test edge cases and other key scenarios more easily.

### Clear failure message

Each test subroutine in [test_temp_conversions.f90](./test_temp_conversions.f90) populates `failure_message` with a clear
message that aims to make it as easy as possible to diagnose a failing test. Importantly, the `failure_message` includes the
`input`, the `expected_output` and the actual value which we have compared to the `expected_output`.

### Comparing floats within an appropriate tolerance

We cannot compare two floats directly as due to rounding errors they will almost always
not be exactly the same. Therefore, we must check the difference between two floats that
we expect to be equal and ensure it is less than some appropriate tolerance. This tolerance
should be as small as possible whilst still making sense with the code we are testing.
