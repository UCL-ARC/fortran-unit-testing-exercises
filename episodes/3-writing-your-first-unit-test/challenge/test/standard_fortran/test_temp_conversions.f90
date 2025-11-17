program test_temp_conversions
    use temp_conversions, only : fahrenheit_to_celsius, celsius_to_kelvin
    implicit none

    integer :: i

    ! Declare passed and failure message arrays to be set by a test subroutine(s)
    logical :: passed(1)
    character(len=200) :: failure_message(1)

    ! Call your test subroutine(s) here
    call test(passed(1), failure_message(1))

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
    !> The test subroutine
    subroutine test(passed, failure_message)
        !> A logical to track whether the test passed or not
        logical, intent(out) :: passed
        !> A failure message to be displayed if passed is false
        character(len=200), intent(out) :: failure_message

        ! No test has been written yet so just default passed to .true.
        passed = .true.

        ! Populate the failure message
        write(failure_message, '(A,A,A)') "It is useful to include input, expected output and actual output values here. To do ", &
                                        "that, replace (A,A) with the correct format for your values, for example ", &
                                        "(A,F7.2,A,F7.2,A,F7.2)."
    end subroutine test
end program test_temp_conversions
