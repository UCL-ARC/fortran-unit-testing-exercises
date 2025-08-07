program test_maths

    implicit none

    integer :: i

    integer :: passed(7)
    character(len=80) :: failure_message(7)

    call test_double(passed(1), failure_message(1), 2, 4)
    call test_double(passed(2), failure_message(2), 0, 0)
    call test_double(passed(3), failure_message(3), -1, -2)
    call test_double(passed(4), failure_message(4), 5000, 10000)

    call test_factorial(passed(5), failure_message(5), 4, 24)
    call test_factorial(passed(6), failure_message(6), 0, 1)
    call test_factorial(passed(7), failure_message(7), 10, 3.6288e6)

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
    !> A unit test for the maths::double function.
    subroutine test_double(passed, failure_message, input, expected_output)
        implicit none

        logical, intent(out) :: passed
        character(len=80), intent(out) :: failure_message
        integer, intent(in) :: input, expected_output

        integer :: actual_value

        ! When we double our input
        actual_value = double(input)

        ! Then we expect the actual_value to match the expected_value
        passed = actual_value == expected_output

        ! Write the failure message if the test fails
        write(failure_message, '(A,I3,A,I3,A,I3)') "test_double with ", input, " failed, Expected ", expected_output, &
                                                   " but got ", actual_value

    end subroutine test_double

    !> A unit test for the maths::factorial function.
    subroutine test_factorial(passed, failure_message, input, expected_output)
        implicit none

        logical, intent(out) :: passed
        character(len=80), intent(out) :: failure_message
        integer, intent(in) :: input, expected_output

        integer :: actual_value

        ! When we get the factorial of our input
        actual_value = factorial(input)

        ! Then we expect the actual_value to match the expected_value
        passed = actual_value == expected_output

        ! Write the failure message if the test fails
        write(failure_message, '(A,I3,A,I3,A,I3)') "test_factorial with ", input, " failed, Expected ", expected_output, &
                                                   " but got ", actual_value
    end subroutine test_factorial

end program test_maths
