!> Module for testing the subroutine game_of_life::check_for_steady_state
module testdrive_check_for_steady_state_test
    use game_of_life_mod, only : check_for_steady_state
    use testdrive, only : new_unittest, unittest_type, error_type, check

    implicit none

    private
    public :: check_for_steady_state_test_suite

    ! TASK: Define a parameter type `check_for_steady_state_test_params` to be used for testing game_of_life::check_for_steady_state
    !> Type to bundle inputs and expected outputs of game_of_life::check_for_steady_state
    type :: check_for_steady_state_test_params
        !> A board representing the current board before the last evolution
        integer, dimension(:,:), allocatable :: current_board
        !> A board representing the new board after the last evolution
        integer, dimension(:,:), allocatable :: new_board
        !> The value of steady state expected to be returned with the above boards as inputs
        logical :: expected_steady_state
        !> A description of the test to be outputted for logging
        character(len=100) :: description
    end type check_for_steady_state_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! TASK: Write the subroutine `check_for_steady_state_test_suite` which populates testsuites for testing
    !       game_of_life::check_for_steady_state including tests of the following scenarios.
    !
    !       1. Matching boards full of zeros are in steady state
    !       2. Matching boards full of ones are in steady state
    !       3. Matching boards with up to 10 ones are in steady state
    !       4. Mismatched boards with the first all zeros and the other all ones is not in steady state
    !       5. Mismatched boards with the first all zeros and the other all ones is not in steady state
    !       6. Mismatched boards with up to 10 differences is not in steady state
    !
    !> Test suite for the game_of_life::check_for_steady_state subroutine
    subroutine check_for_steady_state_test_suite(testsuite)
        !> An array of test cases which together define the test suite. Must of type unittest_type for testdrive to pick it up
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

    ! TASK: Write test subroutines for each of the above scenarios (1. is already provided)

    !> Test matching boards with all zeros are in steady state
    subroutine test_check_for_steady_state_all_zeros(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: params

        params%description = "Matching boards with all zeros"
        call populate_random_boards(params%current_board, params%new_board, 0, .true.)
        params%expected_steady_state = .true.

        call check_if_steady_state(error, params)
    end subroutine test_check_for_steady_state_all_zeros

    !> 2. Test matching boards with all ones are in steady state
    !> Test matching boards with all ones are in steady state
    subroutine test_check_for_steady_state_all_ones(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: params

        params%description = "Matching boards with all ones"
        call populate_random_boards(params%current_board, params%new_board, 1, .true.)
        params%expected_steady_state = .true.

        call check_if_steady_state(error, params)
    end subroutine test_check_for_steady_state_all_ones

    !> 3. Test matching boards with up to 10 ones are in steady state
    !> Test matching boards with up to 10 ones are in steady state
    subroutine test_check_for_steady_state_up_to_ten_ones(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: params

        params%description = "Matching boards with up to 10 ones"
        call populate_random_boards(params%current_board, params%new_board, 10, .true.)
        params%expected_steady_state = .true.

        call check_if_steady_state(error, params)
    end subroutine test_check_for_steady_state_up_to_ten_ones

    !> 4. Test mismatched boards with all zeros and all ones are not in steady state
    !> Test mismatched boards with all zeros and all ones are not in steady state
    subroutine test_check_for_steady_state_all_zeros_all_ones(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: params

        params%description = "Mismatched boards with all zeros and all ones"
        call populate_random_boards(params%current_board, params%new_board, 0, .false.)
        params%expected_steady_state = .false.

        call check_if_steady_state(error, params)
    end subroutine test_check_for_steady_state_all_zeros_all_ones

    !> 5. Test mismatched boards with all ones and all zeros are not in steady state
    !> Test mismatched boards with all ones and all zeros are not in steady state
    subroutine test_check_for_steady_state_all_ones_all_zeros(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: params

        params%description = "Mismatched boards with all ones and all zeros"
        call populate_random_boards(params%new_board, params%current_board, 0, .false.)
        params%expected_steady_state = .false.

        call check_if_steady_state(error, params)
    end subroutine test_check_for_steady_state_all_ones_all_zeros

    !> 6. Test mismatched boards with with up to ten differences are not in steady state
    !> Test mismatched boards with with up to ten differences are not in steady state
    subroutine test_check_for_steady_state_up_to_ten_differences(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(check_for_steady_state_test_params) :: params

        params%description = "Mismatched boards with up to 10 differences"
        call populate_random_boards(params%current_board, params%new_board, 10, .false.)
        params%expected_steady_state = .false.

        call check_if_steady_state(error, params)
    end subroutine test_check_for_steady_state_up_to_ten_differences

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::check_for_steady_state subroutine
    subroutine check_if_steady_state(error, params)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error
        !> The current test parameters including inputs and expected outputs
        type(check_for_steady_state_test_params), intent(in) :: params

        logical :: actual_steady_state
        character(len=100) :: failure_message

        call check_for_steady_state(params%current_board, params%new_board, actual_steady_state)

        write(failure_message,'(a,a,L,a,L)') trim(params%description), ": Expected steady state status to be ", &
            params%expected_steady_state, " but found ", actual_steady_state
        call check(error, params%expected_steady_state .eqv. actual_steady_state, failure_message)
        if (allocated(error)) return
    end subroutine check_if_steady_state

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Constructors
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> A convenience function to allow the populating of two boards with a random number of 1s and 0s
    !> The two boards can either be created matching or with with different locations for their 1s and 0s.
    subroutine populate_random_boards(board_1, board_2, num_differences, matching)
        !> One of the boards to be randomly populated
        integer, dimension(:,:), allocatable, intent(inout) :: board_1
        !> One of the baords to be randomly populated
        integer, dimension(:,:), allocatable, intent(inout) :: board_2
        !> The number of elements of board_1 to switch to 1.
        integer, intent(in) :: num_differences
        !> If true, board_1 and board_2 will match, otherwise board_2 will have it's ones and zeros inverted
        !> and will have a different random selection of elements set to 0, compared with board_1's 1s.
        logical, intent(in) :: matching

        integer :: nrow, ncol, row, col, rand_row, rand_col, new_board_val
        real :: rand_real
        nrow = 31
        ncol = 31

        ! Allocate arrays
        allocate(board_1(nrow, ncol))
        allocate(board_2(nrow, ncol))
        board_1 = 0

        if (matching) then
            board_2 = 0
        else
            board_2 = 1
        end if

        ! For both boards, set to requested number of elements to the opposite value
        do row = 1, num_differences
            ! Get random coordinates for current
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            board_1(rand_row, rand_col) = 1

            if (.not. matching) then
                ! Get random coordinates for new
                call random_number(rand_real)
                rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
                call random_number(rand_real)
                rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

                board_2(rand_row, rand_col) = 0
            else
                board_2(rand_row, rand_col) = 1
            end if
        end do
    end subroutine populate_random_boards
end module testdrive_check_for_steady_state_test
