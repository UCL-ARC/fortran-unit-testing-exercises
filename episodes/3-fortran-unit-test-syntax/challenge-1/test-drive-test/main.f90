!> Driver for unit testing
program test_main
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type, &
            & select_suite, run_selected, get_argument

    use testdrive_evolve_board_test, only : testdrive_evolve_board_test_suite

    implicit none

    type(testsuite_type), allocatable :: testsuites(:)

    testsuites = [ &
        new_testsuite("evolve_board", testdrive_evolve_board_test_suite) &
    ]

    call run_tests(testsuites)

contains

    subroutine run_tests(testsuites)
        use, intrinsic :: iso_fortran_env, only : error_unit

        type(testsuite_type), allocatable, intent(in) :: testsuites(:)

        integer :: stat, is
        character(len=:), allocatable :: suite_name, test_name
        character(len=*), parameter :: fmt = '("#", *(1x, a))'

        stat = 0

        call get_argument(1, suite_name)
        call get_argument(2, test_name)

        write(error_unit, fmt) "Running testdrive tests suite"
        if (allocated(suite_name)) then
            is = select_suite(testsuites, suite_name)
            if (is > 0 .and. is <= size(testsuites)) then
                if (allocated(test_name)) then
                    write(error_unit, fmt) "Suite:", testsuites(is)%name
                    call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
                    if (stat < 0) then
                        error stop 1
                    end if
                else
                    write(error_unit, fmt) "Testing:", testsuites(is)%name
                    call run_testsuite(testsuites(is)%collect, error_unit, stat)
                end if
            else
                write(error_unit, fmt) "Available testsuites"
                do is = 1, size(testsuites)
                    write(error_unit, fmt) "-", testsuites(is)%name
                end do
                error stop 1
            end if
        else
            do is = 1, size(testsuites)
                write(error_unit, fmt) "Testing:", testsuites(is)%name
                call run_testsuite(testsuites(is)%collect, error_unit, stat)
            end do
        end if

        if (stat > 0) then
            write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
            error stop 1
        end if
    end subroutine run_tests

end program test_main
