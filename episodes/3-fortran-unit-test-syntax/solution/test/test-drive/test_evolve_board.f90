!> Module for testing the subroutine game_of_life::evolve_board
module testdrive_evolve_board_test
    use game_of_life_mod, only : evolve_board
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: evolve_board_test_suite

    !> Type to bundle inputs and expected outputs of game_of_life::evolve_board
    type :: evolve_board_test_params
        !> A board representing the current board before the last evolution
        integer, dimension(:,:), allocatable :: current_board
        !> The expected board after the last evolution
        integer, dimension(:,:), allocatable :: expected_new_board
        !> A description of the test to be outputted for logging
        character(len=100) :: description
    end type evolve_board_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life::evolve_board subroutine
    subroutine evolve_board_test_suite(testsuite)
        !> An array of test cases which together define the test suite. Must of type unittest_type for testdrive to pick it up
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_evolve_board_all_zeros", test_evolve_board_all_zeros), &
            new_unittest("test_evolve_board_complex_steady_state", test_evolve_board_complex_steady_state), &
            new_unittest("test_evolve_board_one_non_zero_element", test_evolve_board_one_non_zero_element), &
            new_unittest("test_evolve_board_complex_non_steady_state", test_evolve_board_complex_non_steady_state) &
        ]

    end subroutine evolve_board_test_suite

    !> Test steady state is defined for a board of all zeros
    subroutine test_evolve_board_all_zeros(error)
        type(error_type), allocatable, intent(out) :: error

        type(evolve_board_test_params) :: params
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        params%description = "All zeros"
        allocate(params%current_board(nrow, ncol))
        allocate(params%expected_new_board(nrow, ncol))
        params%current_board = 0
        params%expected_new_board = 0

        call check_evolve_board(error, params)
    end subroutine test_evolve_board_all_zeros

    !> Test steady state is defined for a slightly more complex steady state structure
    !!       8  9 10 11 12
    !!      -- -- -- -- --
    !!   8 | 0  0  0  0  0
    !!   9 | 0  0  1  0  0
    !!  10 | 0  1  0  1  0
    !!  11 | 0  1  0  1  0
    !!  12 | 0  0  1  0  0
    !!  13 | 0  0  0  0  0
    !!
    subroutine test_evolve_board_complex_steady_state(error)
        type(error_type), allocatable, intent(out) :: error

        type(evolve_board_test_params) :: params
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        params%description = "Complex steady state"
        allocate(params%current_board(nrow, ncol))
        allocate(params%expected_new_board(nrow, ncol))
        params%current_board = 0
        params%expected_new_board = 0

        ! Input board
        params%current_board(9,9:11)  = [0,1,0]
        params%current_board(10,9:11) = [1,0,1]
        params%current_board(11,9:11) = [1,0,1]
        params%current_board(12,9:11) = [0,1,0]
        ! Expected output board
        params%expected_new_board(9,9:11)  = params%current_board(9,9:11)
        params%expected_new_board(10,9:11) = params%current_board(10,9:11)
        params%expected_new_board(11,9:11) = params%current_board(11,9:11)
        params%expected_new_board(12,9:11) = params%current_board(12,9:11)

        call check_evolve_board(error, params)
    end subroutine test_evolve_board_complex_steady_state

    !> Test steady state is not defined for a board with all zeros apart from a one non-zero element
    subroutine test_evolve_board_one_non_zero_element(error)
        type(error_type), allocatable, intent(out) :: error

        type(evolve_board_test_params) :: params
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        params%description = "One non-zero element"
        allocate(params%current_board(nrow, ncol))
        allocate(params%expected_new_board(nrow, ncol))
        params%current_board = 0
        params%expected_new_board = 0

        ! Input board
        params%current_board(10,9) = 1

        call check_evolve_board(error, params)
    end subroutine test_evolve_board_one_non_zero_element

    !> A slightly more complex non-steady state structure
    !!   Input board             Expected output board
    !!       8  9 10 11 12           8  9 10 11 12
    !!      -- -- -- -- --          -- -- -- -- --
    !!   8 | 0  0  0  0  0       8 | 0  0  0  0  0
    !!   9 | 0  0  1  0  0   \   9 | 0  1  1  1  0
    !!  10 | 0  1  1  1  0 ---\ 10 | 0  1  0  1  0
    !!  11 | 0  1  0  1  0 ---/ 11 | 0  1  0  1  0
    !!  12 | 0  0  1  0  0   /  12 | 0  0  1  0  0
    !!  13 | 0  0  0  0  0      13 | 0  0  0  0  0
    subroutine test_evolve_board_complex_non_steady_state(error)
        type(error_type), allocatable, intent(out) :: error

        type(evolve_board_test_params) :: params
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        params%description = "Complex non-steady state"
        allocate(params%current_board(nrow, ncol))
        allocate(params%expected_new_board(nrow, ncol))
        params%current_board = 0
        params%expected_new_board = 0

        ! Input board
        params%current_board(9,9:11)  = [0,1,0]
        params%current_board(10,9:11) = [1,1,1]
        params%current_board(11,9:11) = [1,0,1]
        params%current_board(12,9:11) = [0,1,0]
        ! Expected output board
        params%expected_new_board(9,9:11)  = [1,1,1]
        params%expected_new_board(10,9:11) = [1,0,1]
        params%expected_new_board(11,9:11) = [1,0,1]
        params%expected_new_board(12,9:11) = [0,1,0]

        call check_evolve_board(error, params)
    end subroutine test_evolve_board_complex_non_steady_state

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::evolve_board subroutine
    subroutine check_evolve_board(error, params)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error
        !> The current test parameters including inputs and expected outputs
        type(evolve_board_test_params), intent(in) :: params

        integer, dimension(:,:), allocatable :: actual_new_board
        integer :: nrow, ncol, row, col
        character(len=100) :: failure_message

        allocate(actual_new_board(size(params%current_board, 1), size(params%current_board, 2)))
        actual_new_board = params%current_board

        call evolve_board(params%current_board, actual_new_board)

        nrow = size(params%expected_new_board, 1)
        ncol = size(params%expected_new_board, 2)

        call check(error, nrow, size(actual_new_board, 1))
        if (allocated(error)) return

        call check(error, ncol, size(actual_new_board, 2))
        if (allocated(error)) return

        do col = 1, ncol
            do row = 1, nrow
                write(failure_message,'(a,a,i1,a,i1,a,i2,a,i2)') trim(params%description), ": Unexpected value for new_board(", &
                    row, ",", col, "), got ", actual_new_board(row, col), " expected ", params%expected_new_board(row, col)
                call check(error, params%expected_new_board(row, col), actual_new_board(row, col), failure_message)
                if (allocated(error)) return
            end do
        end do

        ! Catch test failure
        if (allocated(error)) return
    end subroutine check_evolve_board
end module testdrive_evolve_board_test
