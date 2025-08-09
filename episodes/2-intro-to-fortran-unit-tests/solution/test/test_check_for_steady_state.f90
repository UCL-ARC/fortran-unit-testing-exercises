!> Module for testing the subroutine game_of_life::check_for_steady_state
module check_for_steady_state_test
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

    !> Test suite for the game_of_life::check_for_steady_state subroutine
    function check_for_steady_state_test_suite() result(tests)
        type(test_item_t) :: tests

        integer, dimension(:,:), allocatable :: test_current_board, test_new_board
        type(example_t) :: matching_boards_data(3), non_matching_boards_data(3)
        integer :: nrow, ncol

        nrow = 31
        ncol = 31

        ! Allocate arrays
        allocate(test_current_board(nrow, ncol))
        allocate(test_new_board(nrow, ncol))

        ! Matching boards
        !  All zeros
        call populate_random_boards(test_current_board, test_new_board, 0, .true.)
        matching_boards_data(1) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .true.))
        !  All ones
        call populate_random_boards(test_current_board, test_new_board, nrow*ncol, .true.)
        matching_boards_data(2) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .true.))
        !  Up to 10 ones
        call populate_random_boards(test_current_board, test_new_board, 10, .true.)
        matching_boards_data(3) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .true.))

        ! Mismatched boards
        !  All ones vs all zeros
        call populate_random_boards(test_current_board, test_new_board, 0, .false.)
        non_matching_boards_data(1) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .false.))
        !  All zeros vs all ones
        call populate_random_boards(test_current_board, test_new_board, nrow*ncol, .false.)
        non_matching_boards_data(2) = example_t(check_for_steady_state_test_params(test_current_board, test_new_board, .false.))
        !  Up to 10 differences
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

        deallocate(test_current_board)
        deallocate(test_new_board)
    end function check_for_steady_state_test_suite

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::check_for_steady_state subroutine
    function check_if_steady_state(input) result(result_)
        class(input_t), intent(in) :: input
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

    subroutine populate_random_boards(current_board, new_board, num_differences, matching)
        integer, dimension(:,:), allocatable, intent(inout) :: current_board, new_board
        integer, intent(in) :: num_differences
        logical, intent(in) :: matching

        integer :: nrow, ncol, row, col, rand_row, rand_col, new_board_val
        real :: rand_real

        ! Initialise
        nrow = size(current_board, 1)
        ncol = size(current_board, 2)
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
end module check_for_steady_state_test
