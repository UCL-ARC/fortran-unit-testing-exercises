!> Module for testing the subroutine game_of_life::evolve_board
module testdrive_evolve_board_test
    use game_of_life_mod, only : evolve_board
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: testdrive_evolve_board_test_suite

    !> Type to bundle inputs and expected outputs of game_of_life::evolve_board
    type :: evolve_board_in_out_t
        integer, dimension(:,:), allocatable :: current_board
        integer, dimension(:,:), allocatable :: expected_new_board
    end type evolve_board_in_out_t

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life::evolve_board subroutine
    subroutine testdrive_evolve_board_test_suite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_evolve_board_all_zeros", test_evolve_board_all_zeros), &
            new_unittest("test_evolve_board_complex_steady_state", test_evolve_board_complex_steady_state), &
            new_unittest("test_evolve_board_one_non_zero_element", test_evolve_board_one_non_zero_element), &
            new_unittest("test_evolve_board_complex_non_steady_state", test_evolve_board_complex_non_steady_state) &
        ]

    end subroutine testdrive_evolve_board_test_suite

    !> Test steady state is defined for a board of all zeros
    subroutine test_evolve_board_all_zeros(error)
        type(error_type), allocatable, intent(out) :: error

        type(evolve_board_in_out_t) :: inputs
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        allocate(inputs%current_board(nrow, ncol))
        allocate(inputs%expected_new_board(nrow, ncol))
        inputs%current_board = 0
        inputs%expected_new_board = 0

        call check_evolve_board(error, inputs)

        deallocate(inputs%current_board)
        deallocate(inputs%expected_new_board)
    end subroutine test_evolve_board_all_zeros

    !> Test steady state is defined for a slightly more complex steady state sructure
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

        type(evolve_board_in_out_t) :: inputs
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        allocate(inputs%current_board(nrow, ncol))
        allocate(inputs%expected_new_board(nrow, ncol))
        inputs%current_board = 0
        inputs%expected_new_board = 0

        ! Input board
        inputs%current_board(9,9:11)  = [0,1,0]
        inputs%current_board(10,9:11) = [1,0,1]
        inputs%current_board(11,9:11) = [1,0,1]
        inputs%current_board(12,9:11) = [0,1,0]
        ! Expected output board
        inputs%expected_new_board(9,9:11)  = inputs%current_board(9,9:11)
        inputs%expected_new_board(10,9:11) = inputs%current_board(10,9:11)
        inputs%expected_new_board(11,9:11) = inputs%current_board(11,9:11)
        inputs%expected_new_board(12,9:11) = inputs%current_board(12,9:11)

        call check_evolve_board(error, inputs)

        deallocate(inputs%current_board)
        deallocate(inputs%expected_new_board)
    end subroutine test_evolve_board_complex_steady_state

    !> Test steady state is not defined for a board with all zeros apart from a one non-zero element
    subroutine test_evolve_board_one_non_zero_element(error)
        type(error_type), allocatable, intent(out) :: error

        type(evolve_board_in_out_t) :: inputs
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        allocate(inputs%current_board(nrow, ncol))
        allocate(inputs%expected_new_board(nrow, ncol))
        inputs%current_board = 0
        inputs%expected_new_board = 0

        ! Input board
        inputs%current_board(10,9) = 1

        call check_evolve_board(error, inputs)

        deallocate(inputs%current_board)
        deallocate(inputs%expected_new_board)
    end subroutine test_evolve_board_one_non_zero_element

    !> A slightly more complex non-steady state sructure
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

        type(evolve_board_in_out_t) :: inputs
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        allocate(inputs%current_board(nrow, ncol))
        allocate(inputs%expected_new_board(nrow, ncol))
        inputs%current_board = 0
        inputs%expected_new_board = 0

        ! Input board
        inputs%current_board(9,9:11)  = [0,1,0]
        inputs%current_board(10,9:11) = [1,1,1]
        inputs%current_board(11,9:11) = [1,0,1]
        inputs%current_board(12,9:11) = [0,1,0]
        ! Expected output board
        inputs%expected_new_board(9,9:11)  = [1,1,1]
        inputs%expected_new_board(10,9:11) = [1,0,1]
        inputs%expected_new_board(11,9:11) = [1,0,1]
        inputs%expected_new_board(12,9:11) = [0,1,0]

        call check_evolve_board(error, inputs)

        deallocate(inputs%current_board)
        deallocate(inputs%expected_new_board)
    end subroutine test_evolve_board_complex_non_steady_state

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::evolve_board subroutine
    subroutine check_evolve_board(error, input)
        type(error_type), allocatable, intent(out) :: error
        class(evolve_board_in_out_t), intent(in) :: input

        integer, dimension(:,:), allocatable :: actual_new_board
        integer :: nrow, ncol, row, col
        character(len=80) :: failure_message

        allocate(actual_new_board(size(input%current_board, 1), size(input%current_board, 2)))
        actual_new_board = input%current_board

        call evolve_board(input%current_board, actual_new_board)

        nrow = size(input%expected_new_board, 1)
        ncol = size(input%expected_new_board, 2)

        call check(error, nrow, size(actual_new_board, 1))
        if (allocated(error)) return

        call check(error, ncol, size(actual_new_board, 2))
        if (allocated(error)) return

        do col = 1, ncol
            do row = 1, nrow
                write(failure_message,'(a,i1,a,i1,a,i2,a,i2)') "Unexpected value for new_board(", row, ",", col, "), got ", &
                    actual_new_board(row, col), " expected ", input%expected_new_board(row, col)
                call check(error, input%expected_new_board(row, col), actual_new_board(row, col), failure_message)
                if (allocated(error)) return
            end do
        end do

        ! Catch test failure
        if (allocated(error)) return
    end subroutine check_evolve_board
end module testdrive_evolve_board_test
