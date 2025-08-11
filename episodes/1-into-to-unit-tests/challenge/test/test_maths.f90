program test_maths
    use maths, only : double, factorial
    implicit none

    logical :: passed
    character(len=80) :: failure_message

    call test(passed, failure_message)

    if (.not. passed) then
        write(*,*) "FAIL: ", trim(failure_message)
        stop 1
    else
        write(*,*) "All tests passed!"
    end if

contains
    !> A unit test for the maths module.
    subroutine test(passed, failure_message)
      !> A logical to track whether the test passed or not
      logical, intent(out) :: passed
      !> A failure message to be displayed if passed is false
      character(len=80), intent(out) :: failure_message

      integer :: actual_value, expected_value, input

      ! Given we have an input of 2
      input = 2
      expected_value = 24

      ! When we apply our maths operations
      actual_value = double(input)
      actual_value = factorial(actual_value)

      ! Then we expect the actual_value to match the expected_value
      passed = actual_value == expected_value

      ! Populate the failure message
      write(failure_message, '(A,I3,A,I3,A,I3)') "testing maths with ", input, " failed, Expected ", expected_value, " but got ", &
                                                 actual_value
    end subroutine test
end program test_maths
