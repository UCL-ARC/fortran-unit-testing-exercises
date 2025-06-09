module game_of_life_sol_test
    use game_of_life_mod, only : check_for_steady_state
    use veggies, only:            &
        assert_that,              &
        fail,                     &
        given,                    &
        input_t,                  &
        result_t,                 &
        test_item_t,              &
        then__,                   &
        transformation_failure_t, &
        transformed_t,            &
        when
    implicit none

    private
    public :: game_of_life_sol_test_suite

    !> Type to bundle the inputs of game_of_life_sol::check_for_steady_state
    type, extends(input_t) :: input_boards_t
        integer, dimension(:,:), allocatable :: current_board, new_board
    end type input_boards_t

    !> Type to bundle the outputs of game_of_life_sol::check_for_steady_state
    type, extends(input_t) :: check_for_steady_state_output_t
        logical :: steady_state
    end type check_for_steady_state_output_t

contains

    function game_of_life_sol_test_suite() result(tests)
        type(test_item_t) :: tests

        tests = given( &
                    "we have populated a matching board and temp_board", &
                    setup_matching_boards(), &
                    [ when( &
                        "we check for steady_state", &
                        run_steady_sate_check, &
                        [ then__("steady_state will be reached", check_is_steady_sate) &
                        ]) &
                    ])
    end function game_of_life_sol_test_suite

    function setup_matching_boards() result(input_boards)
        type(input_boards_t) :: input_boards

        integer :: nrow, ncol, row, col, num_ones, rand_row, rand_col
        real :: rand_real

        nrow = 31
        ncol = 31

        ! Initialise to all zeros
        allocate(input_boards%current_board(nrow, ncol))
        allocate(input_boards%new_board(nrow, ncol))
        input_boards%current_board = 0
        input_boards%new_board = 0

        ! For both boards, set one or more elements to 1
        call random_number(rand_real)
        num_ones = 1 + FLOOR(nrow*ncol*rand_real) ! n=1 to n=nrow*ncol
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

    function run_steady_sate_check(input) result(output)
        class(input_t), intent(in) :: input
        type(transformed_t) :: output

        logical :: actual_steady_state

        select type (input)
        type is (input_boards_t)
            call check_for_steady_state(input%current_board, input%new_board, actual_steady_state)

            output = transformed_t(check_for_steady_state_output_t(actual_steady_state))

        class default
            output = transformed_t(transformation_failure_t(fail( &
                "Didn't get input_boards_t")))

        end select
    end function run_steady_sate_check

    function check_is_steady_sate(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (check_for_steady_state_output_t)
            result_ = assert_that(input%steady_state)
        class default
            result_ = fail("Didn't get check_for_steady_state_output_t")
        end select
    end function check_is_steady_sate
end module game_of_life_sol_test
