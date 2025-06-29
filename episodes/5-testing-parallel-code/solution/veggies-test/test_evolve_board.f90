!> Module for testing the subroutine game_of_life::evolve_board
module veggies_evolve_board_test
    use game_of_life_mod, only : evolve_board
    use veggies, only:            &
        assert_equals,            &
        describe,                 &
        example_t,                &
        fail,                     &
        input_t,                  &
        it,                       &
        result_t,                 &
        test_item_t
    implicit none

    private
    public :: evolve_board_test_suite

    !> Type to bundle inputs and expected outputs of game_of_life::evolve_board
    type, extends(input_t) :: evolve_board_test_params
        integer, dimension(:,:), allocatable :: current_board
        integer, dimension(:,:), allocatable :: expected_new_board
    end type evolve_board_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life::evolve_board subroutine
    function evolve_board_test_suite() result(tests)
        type(test_item_t) :: tests

        integer, dimension(:,:), allocatable :: test_current_board, expected_new_board
        type(example_t) :: steady_state_boards_data(2), non_steady_state_boards_data(2)
        integer :: nrow, ncol

        nrow = 20
        ncol = 20

        allocate(test_current_board(nrow, ncol))
        allocate(expected_new_board(nrow, ncol))
        test_current_board = 0
        expected_new_board = 0

        ! Steady state boards
        !  All zeros
        steady_state_boards_data(1) = example_t(evolve_board_test_params(test_current_board, expected_new_board))

        !  A slightly more complex steady state sructure
        !       8  9 10 11 12
        !      -- -- -- -- --
        !   8 | 0  0  0  0  0
        !   9 | 0  0  1  0  0
        !  10 | 0  1  0  1  0
        !  11 | 0  1  0  1  0
        !  12 | 0  0  1  0  0
        !  13 | 0  0  0  0  0
        !
        !   Input board
        test_current_board(9,9:11)  = [0,1,0]
        test_current_board(10,9:11) = [1,0,1]
        test_current_board(11,9:11) = [1,0,1]
        test_current_board(12,9:11) = [0,1,0]
        !   Expected output board
        expected_new_board(9,9:11)  = test_current_board(9,9:11)
        expected_new_board(10,9:11) = test_current_board(10,9:11)
        expected_new_board(11,9:11) = test_current_board(11,9:11)
        expected_new_board(12,9:11) = test_current_board(12,9:11)
        steady_state_boards_data(2) = example_t(evolve_board_test_params(test_current_board, expected_new_board))
        !  Reset for next test
        test_current_board = 0
        expected_new_board = 0

        ! None-steady state boards
        !  One non-zero element
        !   Input board
        test_current_board(10,9) = 1
        non_steady_state_boards_data(1) = example_t(evolve_board_test_params(test_current_board, expected_new_board))
        !  Reset for next test
        test_current_board(10,9) = 0

        !  A slightly more complex non-steady state sructure
        !   Input board             Expected output board
        !       8  9 10 11 12           8  9 10 11 12
        !      -- -- -- -- --          -- -- -- -- --
        !   8 | 0  0  0  0  0       8 | 0  0  0  0  0
        !   9 | 0  0  1  0  0   \   9 | 0  1  1  1  0
        !  10 | 0  1  1  1  0 ---\ 10 | 0  1  0  1  0
        !  11 | 0  1  0  1  0 ---/ 11 | 0  1  0  1  0
        !  12 | 0  0  1  0  0   /  12 | 0  0  1  0  0
        !  13 | 0  0  0  0  0      13 | 0  0  0  0  0
        !
        !   Input board
        test_current_board(9,9:11)  = [0,1,0]
        test_current_board(10,9:11) = [1,1,1]
        test_current_board(11,9:11) = [1,0,1]
        test_current_board(12,9:11) = [0,1,0]
        !   Expected output board
        expected_new_board(9,9:11)  = [1,1,1]
        expected_new_board(10,9:11) = [1,0,1]
        expected_new_board(11,9:11) = [1,0,1]
        expected_new_board(12,9:11) = [0,1,0]
        non_steady_state_boards_data(2) = example_t(evolve_board_test_params(test_current_board, expected_new_board))

        tests = describe( &
            "evolve_board", &
            [ it( &
                "a board in steady state does not change", &
                steady_state_boards_data, &
                check_evolve_board &
            ) &
            , it( &
                "a board not in steady state will change", &
                non_steady_state_boards_data, &
                check_evolve_board &
            )] &
        )
    end function evolve_board_test_suite

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life::evolve_board subroutine
    function check_evolve_board(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer, dimension(:,:), allocatable ::current_board
        integer, dimension(:,:), allocatable ::actual_new_board

        select type (input)
        type is (evolve_board_test_params)
            allocate(current_board(size(input%current_board, 1), size(input%current_board, 2)))
            allocate(actual_new_board(size(input%current_board, 1), size(input%current_board, 2)))
            current_board = input%current_board
            actual_new_board = input%current_board

            !$omp parallel default(none) shared(current_board, actual_new_board)
            call evolve_board(current_board, actual_new_board)
            !$omp end parallel

            result_ = assert_equals(input%expected_new_board, actual_new_board)
        class default
            result_ = fail("Didn't get evolve_board_test_params")

        end select

    end function check_evolve_board
end module veggies_evolve_board_test
