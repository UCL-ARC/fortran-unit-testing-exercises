program test_maths
    use maths, only : double, factorial
    implicit none

    integer :: i

    logical :: passed(7)
    character(len=80) :: failure_message(7)

    call test_double(passed(1), failure_message(1), 2, 4)
    call test_double(passed(2), failure_message(2), 0, 0)
    call test_double(passed(3), failure_message(3), -1, -2)
    call test_double(passed(4), failure_message(4), 5000, 10000)

    call test_factorial(passed(5), failure_message(5), 4, 24)
    call test_factorial(passed(6), failure_message(6), 0, 1)
    call test_factorial(passed(7), failure_message(7), 10, 3628800)

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
        !> true if the test was successful
        logical, intent(out) :: passed
        !> A message to be logged if passed is false
        character(len=80), intent(out) :: failure_message
        !> The input to be passed into double
        integer, intent(in) :: input
        !> The output expected from double
        integer, intent(in) :: expected_output

        integer :: actual_value

        ! When we double our input
        actual_value = double(input)

        ! Then we expect the actual_value to match the expected_value
        passed = actual_value == expected_output

        ! Write the failure message if the test fails
        write(failure_message, '(A,I5,A,I10,A,I10)') "test_double with ", input, " failed, Expected ", expected_output, &
                                                   " but got ", actual_value

    end subroutine test_double

    !> A unit test for the maths::factorial function.
    subroutine test_factorial(passed, failure_message, input, expected_output)
        !> true if the test was successful
        logical, intent(out) :: passed
        !> A message to be logged if passed is false
        character(len=80), intent(out) :: failure_message
        !> The input to be passed into factorial
        integer, intent(in) :: input
        !> The output expected from factorial
        integer, intent(in) :: expected_output

        integer :: actual_value

        ! When we get the factorial of our input
        actual_value = factorial(input)

        ! Then we expect the actual_value to match the expected_value
        passed = actual_value == expected_output

        ! Write the failure message if the test fails
        write(failure_message, '(A,I3,A,I10,A,I10)') "test_factorial with ", input, " failed, Expected ", expected_output, &
                                                   " but got ", actual_value
    end subroutine test_factorial

end program test_maths
