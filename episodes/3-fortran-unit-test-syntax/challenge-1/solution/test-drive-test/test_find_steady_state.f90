!> Module for testing the subroutine game_of_life::find_steady_state
module testdrive_find_steady_state_test
    use game_of_life_mod, only : find_steady_state
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: find_steady_state_test_suite

    !> Type to bundle inputs and expected outputs of game_of_life::find_steady_state
    type :: find_steady_state_test_params
        integer, dimension(:,:), allocatable :: board

        logical :: expected_steady_state
        integer :: expected_generation_number
    end type find_steady_state_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life::find_steady_state subroutine
    subroutine find_steady_state_test_suite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_find_steady_state_exploder",test_find_steady_state_exploder) &
        ]

    end subroutine find_steady_state_test_suite

    !> Test steady state is reached in the expected number of generations for a 31x31 exploder
    subroutine test_find_steady_state_exploder(error)
        type(error_type), allocatable, intent(out) :: error

        type(find_steady_state_test_params) :: inputs

        !  Steady state should be reached after 17 iterations
        !       8  9 10 11 12
        !      -- -- -- -- --
        !   8 | 0  0  0  0  0
        !   9 | 0  0  1  0  0
        !  10 | 0  1  1  1  0
        !  11 | 0  1  0  1  0
        !  12 | 0  0  1  0  0
        !  13 | 0  0  0  0  0
        inputs%expected_steady_state = .true.
        inputs%expected_generation_number = 17
        allocate(inputs%board(31,31))
        inputs%board = 0
        inputs%board(9, 9:11) = [0,1,0]
        inputs%board(10,9:11) = [1,1,1]
        inputs%board(11,9:11) = [1,0,1]
        inputs%board(12,9:11) = [0,1,0]


        call check_find_steady_state(error, inputs)
    end subroutine test_find_steady_state_exploder

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::find_steady_state subroutine
    subroutine check_find_steady_state(error, inputs)
        type(error_type), allocatable, intent(out) :: error
        class(find_steady_state_test_params), intent(in) :: inputs

        logical :: actual_steady_state
        integer :: actual_generation_number
        integer, dimension(:,:), allocatable :: actual_board
        character(len=80) :: failure_message

        allocate(actual_board(size(inputs%board, 1), size(inputs%board, 2)))
        actual_board = inputs%board

        call find_steady_state(.false., actual_steady_state, actual_generation_number, actual_board)


        write(failure_message,'(a, i3, a, i3)') "Unexpected generation_number. Expected ", inputs%expected_generation_number, &
            " but found ", actual_generation_number
        call check(error, inputs%expected_generation_number, actual_generation_number, failure_message)
        if (allocated(error)) return

        call check(error, inputs%expected_steady_state, actual_steady_state, "Unexpected steady_state")
        if (allocated(error)) return
    end subroutine check_find_steady_state
end module testdrive_find_steady_state_test
