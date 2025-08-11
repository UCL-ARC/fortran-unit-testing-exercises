!> Module for testing the subroutine game_of_life::find_steady_state
module find_steady_state_test
    use game_of_life_mod, only : find_steady_state
    use veggies, only:            &
        assert_equals,            &
        assert_that,              &
        assert_not,               &
        describe,                 &
        example_t,                &
        fail,                     &
        input_t,                  &
        it,                       &
        result_t,                 &
        test_item_t
    implicit none

    private
    public :: find_steady_state_test_suite

    !> Type to bundle inputs and expected outputs of game_of_life::find_steady_state
    type, extends(input_t) :: find_steady_state_test_params
        !> The initial starting board to be passed into find_steady_state
        integer, dimension(:,:), allocatable :: board
        !> The expected value of steady_state
        logical :: expected_steady_state
        !> The expected output generation number
        integer :: expected_generation_number
    end type find_steady_state_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life::find_steady_state subroutine
    function find_steady_state_test_suite() result(tests)
        !> The collection of tests which make up this test suite. Must be of type test_item_t to be picked up by veggies
        type(test_item_t) :: tests

        type(example_t) :: parameters(1)
        logical :: expected_steady_state
        integer :: expected_generation_number
        integer, dimension(:,:), allocatable :: board

        !  Steady state should be reached after 17 iterations
        !       8  9 10 11 12
        !      -- -- -- -- --
        !   8 | 0  0  0  0  0
        !   9 | 0  0  1  0  0
        !  10 | 0  1  1  1  0
        !  11 | 0  1  0  1  0
        !  12 | 0  0  1  0  0
        !  13 | 0  0  0  0  0
        expected_steady_state = .true.
        expected_generation_number = 17
        allocate(board(31,31))
        board = 0
        board(9, 9:11) = [0,1,0]
        board(10,9:11) = [1,1,1]
        board(11,9:11) = [1,0,1]
        board(12,9:11) = [0,1,0]

        parameters(1) = example_t(find_steady_state_test_params(board, expected_steady_state, expected_generation_number))

        tests = describe( &
            "find_steady_state", &
            [ it( &
                "a 31x31 board with an exploder starting state", &
                parameters, &
                check_find_steady_state &
            )] &
        )

    end function find_steady_state_test_suite

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::find_steady_state subroutine
    function check_find_steady_state(input) result(result_)
        !> The current test case including inputs and expected outputs, must be of type input_t to be picked up by veggies
        class(input_t), intent(in) :: input
        !> the result of the current test case, must be of type result_t to be picked up by veggies
        type(result_t) :: result_

        logical :: actual_steady_state
        integer :: actual_generation_number
        integer, dimension(:,:), allocatable :: actual_board

        select type (input)
        type is (find_steady_state_test_params)
            allocate(actual_board(size(input%board, 1), size(input%board, 2)))
            actual_board = input%board

            call find_steady_state(.false., actual_steady_state, actual_generation_number, actual_board)

            result_ = assert_equals(input%expected_generation_number, actual_generation_number, "Unexpected generation_number")
            if (input%expected_steady_state) then
                result_ = result_ .and. assert_that(actual_steady_state)
            else
                result_ = result_ .and. assert_not(actual_steady_state)
            end if

        class default
        result_ = fail("Didn't get find_steady_state_test_params")

    end select

    end function check_find_steady_state
end module find_steady_state_test
