program test_temp_conversions
    use temp_conversions, only : fahrenheit_to_celsius, celsius_to_kelvin
    implicit none

    ! Declare passed and failure message to be set by a test subroutine
    logical :: passed
    character(len=200) :: failure_message

    ! Call your test subroutine here
    call test(passed, failure_message)

    if (.not. passed) then
        write(*,*) "FAIL: ", trim(failure_message)
        stop 1
    else
        write(*,*) "All tests passed!"
    end if

contains
    !> The test subroutine
    subroutine test(passed, failure_message)
      !> A logical to track whether the test passed or not
      logical, intent(out) :: passed
      !> A failure message to be displayed if passed is false
      character(len=200), intent(out) :: failure_message

      ! No test has been written yet so just default passed to .true.
      passed = .true.

      ! Populate the failure message
      write(failure_message, '(A,A)') "It is useful to include input, expected output and actual output values here. To do ", &
                                    "that, replace (A,A) with the correct format for your values, for example (A,I3,A,I3,A,I3)."
    end subroutine test
end program test_temp_conversions
