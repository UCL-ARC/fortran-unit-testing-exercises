!> Module for testing the subroutine game_of_life::check_for_steady_state
module testdrive_check_for_steady_state_test
    use game_of_life_mod, only : check_for_steady_state
    use testdrive, only : new_unittest, unittest_type, error_type, check

    implicit none

    private
    public :: check_for_steady_state_test_suite

    !> Type to bundle inputs and expected outputs of game_of_life::check_for_steady_state
    type :: check_for_steady_state_test_params
        integer, dimension(:,:), allocatable :: current_board, new_board
        logical :: expected_steady_state
    end type check_for_steady_state_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life::check_for_steady_state subroutine
    subroutine check_for_steady_state_test_suite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_check_for_steady_state_all_zeros", test_check_for_steady_state_all_zeros), &
            new_unittest("test_check_for_steady_state_all_ones", test_check_for_steady_state_all_ones), &
            new_unittest("test_check_for_steady_state_up_to_ten_ones", test_check_for_steady_state_up_to_ten_ones), &
            new_unittest("test_check_for_steady_state_all_zeros_all_ones", test_check_for_steady_state_all_zeros_all_ones), &
            new_unittest("test_check_for_steady_state_all_ones_all_zeros", test_check_for_steady_state_all_ones_all_zeros), &
            new_unittest("test_check_for_steady_state_up_to_ten_differences", test_check_for_steady_state_up_to_ten_differences) &
        ]
    end subroutine check_for_steady_state_test_suite

    !> Test matching boards with all zeros are in steady state
    subroutine test_check_for_steady_state_all_zeros(error)
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: inputs

        call populate_random_boards(inputs%current_board, inputs%new_board, 0, .true.)
        inputs%expected_steady_state = .true.

        call check_if_steady_state(error, inputs)
    end subroutine test_check_for_steady_state_all_zeros

    !> Test matching boards with all ones are in steady state
    subroutine test_check_for_steady_state_all_ones(error)
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: inputs

        call populate_random_boards(inputs%current_board, inputs%new_board, 1, .true.)
        inputs%expected_steady_state = .true.

        call check_if_steady_state(error, inputs)

        deallocate(inputs%current_board)
        deallocate(inputs%new_board)
    end subroutine test_check_for_steady_state_all_ones

    !> Test matching boards with up to 10 ones are in steady state
    subroutine test_check_for_steady_state_up_to_ten_ones(error)
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: inputs

        call populate_random_boards(inputs%current_board, inputs%new_board, 10, .true.)
        inputs%expected_steady_state = .true.

        call check_if_steady_state(error, inputs)
    end subroutine test_check_for_steady_state_up_to_ten_ones

    !> Test mismatched boards with all zeros and all ones are not in steady state
    subroutine test_check_for_steady_state_all_zeros_all_ones(error)
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: inputs

        call populate_random_boards(inputs%current_board, inputs%new_board, 0, .false.)
        inputs%expected_steady_state = .false.

        call check_if_steady_state(error, inputs)
    end subroutine test_check_for_steady_state_all_zeros_all_ones

    !> Test mismatched boards with all ones and all zeros are not in steady state
    subroutine test_check_for_steady_state_all_ones_all_zeros(error)
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: inputs

        call populate_random_boards(inputs%new_board, inputs%current_board, 0, .false.)
        inputs%expected_steady_state = .false.

        call check_if_steady_state(error, inputs)
    end subroutine test_check_for_steady_state_all_ones_all_zeros

    !> Test mismatched boards with with up to ten differences are not in steady state
    subroutine test_check_for_steady_state_up_to_ten_differences(error)
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: inputs

        call populate_random_boards(inputs%current_board, inputs%new_board, 10, .false.)
        inputs%expected_steady_state = .false.

        call check_if_steady_state(error, inputs)
    end subroutine test_check_for_steady_state_up_to_ten_differences

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::check_for_steady_state subroutine
    subroutine check_if_steady_state(error, inputs)
        type(error_type), allocatable, intent(out) :: error
        type(check_for_steady_state_test_params), intent(in) :: inputs

        logical :: actual_steady_state
        character(len=80) :: failure_message

        call check_for_steady_state(inputs%current_board, inputs%new_board, actual_steady_state)

        write(failure_message,'(a,L,a,L)') "Expected steady state status to be ", &
            inputs%expected_steady_state, " but found ", actual_steady_state
        call check(error, inputs%expected_steady_state .eqv. actual_steady_state, failure_message)
        if (allocated(error)) return
    end subroutine check_if_steady_state

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Contructors
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine populate_random_boards(current_board, new_board, num_differences, matching)
        integer, dimension(:,:), allocatable, intent(inout) :: current_board, new_board
        integer, intent(in) :: num_differences
        logical, intent(in) :: matching

        integer :: nrow, ncol, row, col, rand_row, rand_col, new_board_val
        real :: rand_real
        nrow = 31
        ncol = 31

        ! Allocate arrays
        allocate(current_board(nrow, ncol))
        allocate(new_board(nrow, ncol))
        current_board = 0

        if (matching) then
            new_board = 0
        else
            new_board = 1
        end if

        ! For both boards, set to requested number of elements to the opposite value
        do row = 1, num_differences
            ! Get random coordinates for current
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            current_board(rand_row, rand_col) = 1

            if (.not. matching) then
                ! Get random coordinates for new
                call random_number(rand_real)
                rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
                call random_number(rand_real)
                rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

                new_board(rand_row, rand_col) = 0
            else
                new_board(rand_row, rand_col) = 1
            end if


        end do

    end subroutine populate_random_boards
end module testdrive_check_for_steady_state_test
