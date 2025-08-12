!> Driver for unit testing
program test_main
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type, &
            & select_suite, run_selected, get_argument

    use testdrive_evolve_board_test, only : evolve_board_test_suite
    use testdrive_check_for_steady_state_test, only : check_for_steady_state_test_suite
    use testdrive_find_steady_state_test, only : find_steady_state_test_suite
    use testdrive_read_model_from_file_test, only : read_model_from_file_test_suite

    implicit none

    !> Array of test suites to be executed
    type(testsuite_type), allocatable :: testsuites(:)

    ! Populate the test suites
    testsuites = [ &
        new_testsuite("evolve_board", evolve_board_test_suite), &
        new_testsuite("check_for_steady_state", check_for_steady_state_test_suite), &
        new_testsuite("find_steady_state", find_steady_state_test_suite), &
        new_testsuite("read_model_from_file", read_model_from_file_test_suite) &
    ]

    ! Run the test suites
    call run_tests(testsuites)

contains

    !> Subroutine to run the test suites
    subroutine run_tests(testsuites)
        use, intrinsic :: iso_fortran_env, only : error_unit

        !> Array of test suites to be executed
        type(testsuite_type), allocatable, intent(in) :: testsuites(:)

        !> Status of the test execution
        integer :: stat
        !> Index of the selected test suite
        integer :: is
        !> Name of the selected test suite (if provided as a command-line argument)
        character(len=:), allocatable :: suite_name
        !> Name of the selected test case (if provided as a command-line argument)
        character(len=:), allocatable :: test_name
        !> Format string for logging
        character(len=*), parameter :: fmt = '("#", *(1x, a))'

        stat = 0

        ! Get command-line arguments for suite and test names
        call get_argument(1, suite_name)
        call get_argument(2, test_name)

        ! Log the start of the test suite execution
        write(error_unit, fmt) "Running testdrive tests suite"
        if (allocated(suite_name)) then
            ! If a specific suite is provided, select and run it
            is = select_suite(testsuites, suite_name)
            if (is > 0 .and. is <= size(testsuites)) then
                if (allocated(test_name)) then
                    ! Run a specific test case within the selected suite
                    write(error_unit, fmt) "Suite:", testsuites(is)%name
                    call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
                    if (stat < 0) then
                        error stop 1
                    end if
                else
                    ! Run all test cases in the selected suite
                    write(error_unit, fmt) "Testing:", testsuites(is)%name
                    call run_testsuite(testsuites(is)%collect, error_unit, stat)
                end if
            else
                ! Log available test suites if the specified suite is not found
                write(error_unit, fmt) "Available testsuites"
                do is = 1, size(testsuites)
                    write(error_unit, fmt) "-", testsuites(is)%name
                end do
                error stop 1
            end if
        else
            ! Run all test suites if no specific suite is provided
            do is = 1, size(testsuites)
                write(error_unit, fmt) "Testing:", testsuites(is)%name
                call run_testsuite(testsuites(is)%collect, error_unit, stat)
            end do
        end if

        ! Log the number of failed tests (if any)
        if (stat > 0) then
            write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
            error stop 1
        end if
    end subroutine run_tests

end program test_main
