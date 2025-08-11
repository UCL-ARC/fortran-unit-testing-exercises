!> Module for testing the subroutine game_of_life::find_steady_state
module testdrive_find_steady_state_test
    use game_of_life_mod, only : find_steady_state
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: find_steady_state_test_suite

    !> Type to bundle inputs and expected outputs of game_of_life::find_steady_state
    type :: find_steady_state_test_params
        !> The initial starting board to be passed into find_steady_state
        integer, dimension(:,:), allocatable :: board
        !> The expected value of steady_state
        logical :: expected_steady_state
        !> The expected output generation number
        integer :: expected_generation_number
        !> A description of the test to be outputted for logging
        character(len=100) :: description
    end type find_steady_state_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life::find_steady_state subroutine
    subroutine find_steady_state_test_suite(testsuite)
        !> An array of test cases which together define the test suite. Must of type unittest_type for testdrive to pick it up
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_find_steady_state_exploder",test_find_steady_state_exploder) &
        ]

    end subroutine find_steady_state_test_suite

    !> Test steady state is reached in the expected number of generations for a 31x31 exploder
    subroutine test_find_steady_state_exploder(error)
        type(error_type), allocatable, intent(out) :: error

        type(find_steady_state_test_params) :: params

        !  Steady state should be reached after 17 iterations
        !       8  9 10 11 12
        !      -- -- -- -- --
        !   8 | 0  0  0  0  0
        !   9 | 0  0  1  0  0
        !  10 | 0  1  1  1  0
        !  11 | 0  1  0  1  0
        !  12 | 0  0  1  0  0
        !  13 | 0  0  0  0  0
        params%description = "Exploder initial state"
        params%expected_steady_state = .true.
        params%expected_generation_number = 17
        allocate(params%board(31,31))
        params%board = 0
        params%board(9, 9:11) = [0,1,0]
        params%board(10,9:11) = [1,1,1]
        params%board(11,9:11) = [1,0,1]
        params%board(12,9:11) = [0,1,0]


        call check_find_steady_state(error, params)
    end subroutine test_find_steady_state_exploder

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::find_steady_state subroutine
    subroutine check_find_steady_state(error, params)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error
        !> The current test parameters including inputs and expected outputs
        type(find_steady_state_test_params), intent(in) :: params

        logical :: actual_steady_state
        integer :: actual_generation_number
        integer, dimension(:,:), allocatable :: actual_board
        character(len=80) :: failure_message

        allocate(actual_board(size(params%board, 1), size(params%board, 2)))
        actual_board = params%board

        call find_steady_state(.false., actual_steady_state, actual_generation_number, actual_board)


        write(failure_message,'(a,a,i3,a,i3)') trim(params%description), ": Unexpected generation_number. Expected ", &
            params%expected_generation_number, " but found ", actual_generation_number
        call check(error, params%expected_generation_number, actual_generation_number, failure_message)
        if (allocated(error)) return

        write(failure_message, '(a,a,L,a,L)') trim(params%description), ": Unexpected steady_state. Expected ", &
            params%expected_steady_state, " but found ", actual_steady_state
        call check(error, params%expected_steady_state, actual_steady_state, failure_message)
        if (allocated(error)) return
    end subroutine check_find_steady_state
end module testdrive_find_steady_state_test
