module game_of_life_sol_test
    use game_of_life_mod, only : check_for_steady_state
    use veggies, only:            &
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
    public :: check_for_steady_state_tests

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

contains

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
                check_is_steady_state &
            )] &
        )
    end function check_for_steady_state_tests

    function check_is_steady_state(input) result(result_)
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
    end function check_is_steady_state

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Contructors
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function setup_matching_boards(nrow, ncol, num_ones) result(input_boards)
        integer, intent(in) :: nrow, ncol, num_ones
        type(input_boards_t) :: input_boards

        integer :: row, col, rand_row, rand_col
        real :: rand_real

        ! Initialise to all zeros
        allocate(input_boards%current_board(nrow, ncol))
        allocate(input_boards%new_board(nrow, ncol))
        input_boards%current_board = 0
        input_boards%new_board = 0

        ! For both boards, set to requested number of elements to 1
        do row = 1, num_ones
            ! Get random coordinates for 1
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            input_boards%current_board(rand_row, rand_col) = 1
            input_boards%new_board(rand_row, rand_col) = 1
        end do

    end function setup_matching_boards

    function check_for_steady_state_in_out_constructor(input_boards, steady_state) result(check_for_steady_state_in_out)
        type(input_boards_t), intent(in) :: input_boards
        logical, intent(in) :: steady_state

        type(check_for_steady_state_in_out_t) :: check_for_steady_state_in_out

        check_for_steady_state_in_out%input_boards = input_boards
        check_for_steady_state_in_out%expected_steady_state = steady_state

    end function check_for_steady_state_in_out_constructor
end module game_of_life_sol_test
