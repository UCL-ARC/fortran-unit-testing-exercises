!> Module for testing the subroutine game_of_life::check_for_steady_state
module veggies_check_for_steady_state_test
    use game_of_life_mod, only : check_for_steady_state
    use veggies, only:            &
        assert_not,               &
        assert_that,              &
        describe,                 &
        example_t,                &
        fail,                     &
        input_t,                  &
        it,                       &
        result_t,                 &
        test_item_t
    implicit none

    private
    public :: check_for_steady_state_test_suite

    ! TASK: Define a parameter type `check_for_steady_state_test_params` to be used for testing game_of_life::check_for_steady_state
    !> Type to bundle inputs and expected outputs of game_of_life::check_for_steady_state
    type, extends(input_t) :: check_for_steady_state_test_params
        !> The current board to be inputted and compared to the new board
        integer, dimension(:,:), allocatable :: current_board
        !> The new board to be inputted and compared to the current board
        integer, dimension(:,:), allocatable :: new_board
        !> The expected stead state to be outputted
        logical :: expected_steady_state
    end type check_for_steady_state_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! TASK: Finish the function `check_for_steady_state_test_suite` which populates tests for testing
    !       game_of_life::check_for_steady_state including tests of the following scenarios.
    !
    !       1. Matching boards full of zeros are in steady state (already provided)
    !       2. Matching boards full of ones are in steady state
    !       3. Matching boards with up to 10 ones are in steady state
    !       4. Mismatched boards with the first all zeros and the other all ones is not in steady state
    !       5. Mismatched boards with the first all zeros and the other all ones is not in steady state
    !       6. Mismatched boards with up to 10 differences is not in steady state
    !
    !> Test suite for the game_of_life::check_for_steady_state subroutine
    function check_for_steady_state_test_suite() result(tests)
        !> The collection of tests which make up this test suite. Must be of type test_item_t to be picked up by veggies
        type(test_item_t) :: tests

        integer, dimension(:,:), allocatable :: test_current_board, test_new_board
        type(example_t) :: matching_boards_data(3), non_matching_boards_data(3)
        integer :: nrow, ncol

        nrow = 31
        ncol = 31

        ! Allocate arrays
        allocate(test_current_board(nrow, ncol))
        allocate(test_new_board(nrow, ncol))

        !> 1. Test matching boards with all zeros are in steady state
        call populate_random_boards(test_current_board, test_new_board, 0, .true.)
        matching_boards_data(1) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .true.))

        !> 2. Test matching boards with all ones are in steady state
        call populate_random_boards(test_current_board, test_new_board, nrow*ncol, .true.)
        matching_boards_data(2) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .true.))

        !> 3. Test matching boards with up to 10 ones are in steady state
        call populate_random_boards(test_current_board, test_new_board, 10, .true.)
        matching_boards_data(3) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .true.))

        !> 4. Test mismatched boards with all zeros and all ones are not in steady state
        call populate_random_boards(test_current_board, test_new_board, 0, .false.)
        non_matching_boards_data(1) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .false.))

        !> 5. Test mismatched boards with all ones and all zeros are not in steady state
        call populate_random_boards(test_current_board, test_new_board, nrow*ncol, .false.)
        non_matching_boards_data(2) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .false.))

        !> 6. Test mismatched boards with with up to ten differences are not in steady state
        call populate_random_boards(test_current_board, test_new_board, 10, .false.)
        non_matching_boards_data(3) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .false.))

        tests = describe( &
            "check_for_steady_state", &
            [ it( &
                "matching boards are in steady state", &
                matching_boards_data, &
                check_if_steady_state &
            ) &
            , it( &
                "non-matching boards are not in steady state", &
                non_matching_boards_data, &
                check_if_steady_state &
            )] &
        )
    end function check_for_steady_state_test_suite

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::check_for_steady_state subroutine
    function check_if_steady_state(input) result(result_)
        !> The current test case including inputs and expected outputs, must be of type input_t to be picked up by veggies
        class(input_t), intent(in) :: input
        !> the result of the current test case, must be of type result_t to be picked up by veggies
        type(result_t) :: result_

        logical :: actual_steady_state

        select type (input)
        type is (check_for_steady_state_test_params)
            call check_for_steady_state(input%current_board, input%new_board, actual_steady_state)

            if (input%expected_steady_state) then
                result_ = assert_that(actual_steady_state)
            else
                result_ = assert_not(actual_steady_state)
            end if

        class default
            result_ = fail("Didn't get check_for_steady_state_test_params")

        end select

    end function check_if_steady_state

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

        ! Initialise
        nrow = size(board_1, 1)
        ncol = size(board_1, 2)
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
end module veggies_check_for_steady_state_test
