program test_temp_conversions
    use temp_conversions, only : fahrenheit_to_celsius, celsius_to_kelvin
    implicit none

    ! The tolerance to use when comparing floats
    real, parameter :: tolerance = 1e-6

    integer :: i

    ! Declare passed and failure message arrays to be set by a test subroutine(s)
    logical :: passed(7)
    character(len=200) :: failure_message(7)

    ! Define set of tests for fahrenheit_to_celsius by calling test with various inputs and expected outputs
    call test_fahrenheit_to_celsius(0.0, -17.777779, passed(1), failure_message(1))
    call test_fahrenheit_to_celsius(32.0, 0.0, passed(2), failure_message(2))
    call test_fahrenheit_to_celsius(-100.0, -73.333336, passed(3), failure_message(3))
    call test_fahrenheit_to_celsius(1.23,-17.094444, passed(4), failure_message(4))

    ! Define set of tests for celsius_to_kelvin by calling test with various inputs and expected outputs
    call test_celsius_to_kelvin(0.0, 273.15, passed(5), failure_message(5))
    call test_celsius_to_kelvin(-273.15, 0.0, passed(6), failure_message(6))
    call test_celsius_to_kelvin(-173.15, 100.0, passed(7), failure_message(7))

    if (all(passed)) then
        write(*,*) "All tests passed!"
    else
      do i = 1, size(passed)
          if (.not. passed(i)) then
              write(*,*) "FAIL: ", trim(failure_message(i))
          end if
      end do
      stop 1
    end if

contains
    !> Unit test subroutine for celsius_to_kelvin
    subroutine test_fahrenheit_to_celsius(input, expected_output, passed, failure_message)
        !> The input fahrenheit value to pass to fahrenheit_to_celsius
        real, intent(in) :: input
        !> The celsius value we expect to be returner from fahrenheit_to_celsius
        real, intent(in) :: expected_output
        !> A logical to track whether the test passed or not
        logical, intent(out) :: passed
        !> A failure message to be displayed if passed is false
        character(len=200), intent(out) :: failure_message

        real :: actual_output

        ! Get the actual celsius value returned from fahrenheit_to_celsius
        actual_output = fahrenheit_to_celsius(input)

        ! Check that the actual value is within some tolerance of the expected value
        passed = abs(actual_output - expected_output) < tolerance

        ! Populate the failure message
        write(failure_message, '(A,F7.2,A,F7.2,A,F7.2,A)') "Failed With ", input, "°F: Expected ", expected_output, &
                                                         "°C but got ", actual_output, "°C"
    end subroutine test_fahrenheit_to_celsius

    !> Unit test subroutine for celsius_to_kelvin
    subroutine test_celsius_to_kelvin(input, expected_output, passed, failure_message)
        !> The input celsius value to pass to celsius_to_kelvin
        real, intent(in) :: input
        !> The kelvin value we expect to be returner from celsius_to_kelvin
        real, intent(in) :: expected_output
        !> A logical to track whether the test passed or not
        logical, intent(out) :: passed
        !> A failure message to be displayed if passed is false
        character(len=200), intent(out) :: failure_message

        real :: actual_output

        ! Get the actual celsius value returned from celsius_to_kelvin
        actual_output = celsius_to_kelvin(input)

        ! Check that the actual value is within some tolerance of the expected value
        passed = abs(actual_output - expected_output) < tolerance

        ! Populate the failure message
        write(failure_message, '(A,F7.2,A,F7.2,A,F7.2,A)') "Failed With ", input, "°C: Expected ", expected_output, &
                                                           "°K but got ", actual_output, "°K"
    end subroutine test_celsius_to_kelvin
end program test_temp_conversions
