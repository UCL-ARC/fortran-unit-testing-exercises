module game_of_life_sol_test
    use game_of_life_mod, only : check_for_steady_state, evolve_board
    use veggies, only:            &
        assert_that,              &
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
    public :: check_for_steady_state_tests, evolve_board_tests

    !> Type to bundle the boards of game_of_life_sol
    type, extends(input_t) :: input_boards_t
        integer, dimension(:,:), allocatable :: current_board, new_board
    end type input_boards_t

    !> Type to bundle inputs and expected outputs of game_of_life_sol::check_for_steady_state
    type, extends(input_t) :: check_for_steady_state_in_out_t
        type(input_boards_t) :: input_boards
        logical :: expected_steady_state
    end type check_for_steady_state_in_out_t
    interface check_for_steady_state_in_out_t
        module procedure check_for_steady_state_in_out_constructor
    end interface check_for_steady_state_in_out_t

    !> Type to bundle inputs and expected outputs of game_of_life_sol::evolve_board
    type, extends(input_t) :: check_evolve_board_in_out_t
        type(input_boards_t) :: input_boards
        integer, dimension(:,:), allocatable :: expected_new_board
    end type check_evolve_board_in_out_t
    interface check_evolve_board_in_out_t
        module procedure check_evolve_board_in_out_constructor
    end interface check_evolve_board_in_out_t

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life_sol::check_for_steady_state subroutine
    function check_for_steady_state_tests() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
            "check_for_steady_state", &
            [ it( &
                "matching boards are in steady state", &
                [ example_t(check_for_steady_state_in_out_t(setup_matching_boards(31, 31, 0), .true.)) &
                , example_t(check_for_steady_state_in_out_t(setup_matching_boards(31, 31, 10), .true.)) &
                , example_t(check_for_steady_state_in_out_t(setup_matching_boards(31, 31, 31*31), .true.)) &
                ], &
                check_if_steady_state &
            ) &
            , it( &
                "non-matching boards are not in steady state", &
                [ example_t(check_for_steady_state_in_out_t(setup_mismatched_boards(31, 31, 0), .false.)) &
                , example_t(check_for_steady_state_in_out_t(setup_mismatched_boards(31, 31, 10), .false.)) &
                , example_t(check_for_steady_state_in_out_t(setup_mismatched_boards(31, 31, 31*31), .false.)) &
                ], &
                check_if_steady_state &
            )] &
        )
    end function check_for_steady_state_tests

    !> Test suite for the game_of_life_sol::evolve_board subroutine
    function evolve_board_tests() result(tests)
        type(test_item_t) :: tests

        type(input_boards_t) :: test_boards
        type(example_t) :: steady_state_boards_data(2), non_steady_state_boards_data(1)

        test_boards = get_boards_of_zeros(20, 20)

        ! Steady state boards
        !  All zeros
        steady_state_boards_data(1) = example_t(check_evolve_board_in_out_t(test_boards, test_boards%new_board))

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
        test_boards%current_board(9,9:11)  = [0,1,0]
        test_boards%current_board(10,9:11) = [1,0,1]
        test_boards%current_board(11,9:11) = [1,0,1]
        test_boards%current_board(12,9:11) = [0,1,0]
        !   Expected output board
        test_boards%new_board(9,9:11)  = [0,1,0]
        test_boards%new_board(10,9:11) = [1,0,1]
        test_boards%new_board(11,9:11) = [1,0,1]
        test_boards%new_board(12,9:11) = [0,1,0]
        steady_state_boards_data(2) = example_t(check_evolve_board_in_out_t(test_boards, test_boards%new_board))
        !  Reset for next test
        test_boards%current_board = 0
        test_boards%new_board = 0

        ! None-steady state boards
        !  One non-zero element
        !   Input board
        test_boards%current_board(10,9) = 1
        non_steady_state_boards_data(1) = example_t(check_evolve_board_in_out_t(test_boards, test_boards%new_board))
        !  Reset for next test
        test_boards%current_board(10,9) = 0
        test_boards%new_board(10,9) = 0

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
        test_boards%current_board(9,9:11)  = [0,1,0]
        test_boards%current_board(10,9:11) = [1,1,1]
        test_boards%current_board(11,9:11) = [1,0,1]
        test_boards%current_board(12,9:11) = [0,1,0]
        !   Expected output board
        test_boards%new_board(9,9:11)  = [1,1,1]
        test_boards%new_board(10,9:11) = [1,0,1]
        test_boards%new_board(11,9:11) = [1,0,1]
        test_boards%new_board(12,9:11) = [0,1,0]
        steady_state_boards_data(2) = example_t(check_evolve_board_in_out_t(test_boards, test_boards%new_board))
        !  Reset for next test
        test_boards%current_board = 0
        test_boards%new_board = 0

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
    end function evolve_board_tests

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Check for the expected output of the game_of_life_sol::check_for_steady_state subroutine
    function check_if_steady_state(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        logical :: actual_steady_state

        select type (input)
        type is (check_for_steady_state_in_out_t)
            call check_for_steady_state(input%input_boards%current_board, input%input_boards%new_board, actual_steady_state)

            result_ = assert_that(input%expected_steady_state .eqv. actual_steady_state)

        class default
            result_ = fail("Didn't get check_for_steady_state_in_out_t")

        end select

    end function check_if_steady_state

    !> Check for the expected output of the game_of_life_sol::evolve_board subroutine
    function check_evolve_board(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer, dimension(:,:), allocatable ::actual_new_board

        select type (input)
        type is (check_evolve_board_in_out_t)
            allocate(actual_new_board(size(input%input_boards%current_board, 1), size(input%input_boards%current_board, 2)))

            call evolve_board(input%input_boards%current_board, actual_new_board)

            result_ = assert_equals(input%input_boards%new_board, actual_new_board)

            deallocate(actual_new_board)
        class default
            result_ = fail("Didn't get check_evolve_board_in_out_t")

        end select

    end function check_evolve_board

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Contructors
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function get_boards_of_zeros(nrow, ncol) result(input_boards)
        integer, intent(in) :: nrow, ncol
        type(input_boards_t) :: input_boards

        ! Initialise to all zeros
        allocate(input_boards%current_board(nrow, ncol))
        allocate(input_boards%new_board(nrow, ncol))
        input_boards%current_board = 0
        input_boards%new_board = 0
    end function get_boards_of_zeros

    function setup_matching_boards(nrow, ncol, num_ones) result(input_boards)
        integer, intent(in) :: nrow, ncol, num_ones
        type(input_boards_t) :: input_boards

        integer :: row, col, rand_row, rand_col
        real :: rand_real

        ! Initialise to all zeros
        input_boards = get_boards_of_zeros(nrow, ncol)

        ! For both boards, set to requested number of elements to 1
        do row = 1, num_ones
            ! Get random coordinates for both
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            input_boards%current_board(rand_row, rand_col) = 1
            input_boards%new_board(rand_row, rand_col) = 1
        end do

    end function setup_matching_boards

    function setup_mismatched_boards(nrow, ncol, num_ones) result(input_boards)
        integer, intent(in) :: nrow, ncol, num_ones
        type(input_boards_t) :: input_boards

        integer :: row, col, rand_row, rand_col
        real :: rand_real

        ! Initialise
        input_boards = get_boards_of_zeros(nrow, ncol)
        input_boards%new_board = 1

        ! For both boards, set to requested number of elements to 1
        do row = 1, num_ones
            ! Get random coordinates for current
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            input_boards%current_board(rand_row, rand_col) = 1

            ! Get random coordinates for new
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            input_boards%new_board(rand_row, rand_col) = 0
        end do

    end function setup_mismatched_boards

    function check_for_steady_state_in_out_constructor(input_boards, steady_state) result(check_for_steady_state_in_out)
        type(input_boards_t), intent(in) :: input_boards
        logical, intent(in) :: steady_state

        type(check_for_steady_state_in_out_t) :: check_for_steady_state_in_out

        check_for_steady_state_in_out%input_boards = input_boards
        check_for_steady_state_in_out%expected_steady_state = steady_state

    end function check_for_steady_state_in_out_constructor

    function check_evolve_board_in_out_constructor(input_boards, expected_new_board) result(check_evolve_board_in_out)
        type(input_boards_t), intent(in) :: input_boards
        integer, dimension(:,:), allocatable, intent(in) :: expected_new_board

        type(check_evolve_board_in_out_t) :: check_evolve_board_in_out

        check_evolve_board_in_out%input_boards = input_boards
        check_evolve_board_in_out%expected_new_board = expected_new_board
    end function check_evolve_board_in_out_constructor
end module game_of_life_sol_test
