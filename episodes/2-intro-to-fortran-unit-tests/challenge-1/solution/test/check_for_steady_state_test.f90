module check_for_steady_state_test
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
    public :: check_for_steady_state_test_suite

    !> Type to bundle inputs and expected outputs of game_of_life::check_for_steady_state
    type, extends(input_t) :: check_for_steady_state_in_out_t
        integer, dimension(:,:), allocatable :: current_board, new_board
        logical :: expected_steady_state
    end type check_for_steady_state_in_out_t
    interface check_for_steady_state_in_out_t
        module procedure check_for_steady_state_in_out_constructor
    end interface check_for_steady_state_in_out_t

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
        call setup_matching_boards(test_current_board, test_new_board, 0)
        matching_boards_data(1) = example_t(check_for_steady_state_in_out_t(test_current_board, test_new_board, .true.))
        !  All ones
        call setup_matching_boards(test_current_board, test_new_board, nrow*ncol)
        matching_boards_data(2) = example_t(check_for_steady_state_in_out_t(test_current_board, test_new_board, .true.))
        !  Up to 10 ones
        call setup_matching_boards(test_current_board, test_new_board, 10)
        matching_boards_data(3) = example_t(check_for_steady_state_in_out_t(test_current_board, test_new_board, .true.))

        ! Mismatched boards
        !  All ones vs all zeros
        call setup_mismatched_boards(test_current_board, test_new_board, 0)
        non_matching_boards_data(1) = example_t(check_for_steady_state_in_out_t(test_current_board, test_new_board, .false.))
        !  All zeros vs all ones
        call setup_mismatched_boards(test_current_board, test_new_board, nrow*ncol)
        non_matching_boards_data(2) = example_t(check_for_steady_state_in_out_t(test_current_board, test_new_board, .false.))
        !  Up to 10 differences
        call setup_mismatched_boards(test_current_board, test_new_board, 10)
        non_matching_boards_data(3) = example_t(check_for_steady_state_in_out_t(test_current_board, test_new_board, .false.))

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
        type is (check_for_steady_state_in_out_t)
            call check_for_steady_state(input%current_board, input%new_board, actual_steady_state)

            result_ = assert_that(input%expected_steady_state .eqv. actual_steady_state)

        class default
            result_ = fail("Didn't get check_for_steady_state_in_out_t")

        end select

    end function check_if_steady_state

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Contructors
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine setup_matching_boards(current_board, new_board, num_ones)
        integer, dimension(:,:), allocatable, intent(inout) :: current_board, new_board
        integer, intent(in) :: num_ones

        integer :: nrow, ncol, row, col, rand_row, rand_col
        real :: rand_real

        ! Initialise to all zeros
        nrow = size(current_board, 1)
        ncol = size(current_board, 2)
        current_board = 0
        new_board = 0

        ! For both boards, set to requested number of elements to 1
        do row = 1, num_ones
            ! Get random coordinates for both
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            current_board(rand_row, rand_col) = 1
            new_board(rand_row, rand_col) = 1
        end do

    end subroutine setup_matching_boards

    subroutine setup_mismatched_boards(current_board, new_board, num_differences)
        integer, dimension(:,:), allocatable, intent(inout) :: current_board, new_board
        integer, intent(in) :: num_differences

        integer :: nrow, ncol, row, col, rand_row, rand_col
        real :: rand_real

        ! Initialise
        nrow = size(current_board, 1)
        ncol = size(current_board, 2)
        current_board = 0
        new_board = 1

        ! For both boards, set to requested number of elements to the opposite value
        do row = 1, num_differences
            ! Get random coordinates for current
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            current_board(rand_row, rand_col) = 1

            ! Get random coordinates for new
            call random_number(rand_real)
            rand_row = 1 + FLOOR(nrow*rand_real) ! n=1 to n=nrow
            call random_number(rand_real)
            rand_col = 1 + FLOOR(ncol*rand_real) ! n=1 to n=ncol

            new_board(rand_row, rand_col) = 0
        end do

    end subroutine setup_mismatched_boards

    function check_for_steady_state_in_out_constructor(current_board, new_board, steady_state) result(check_for_steady_state_in_out)
        integer, dimension(:,:), allocatable, intent(in) :: current_board, new_board
        logical, intent(in) :: steady_state

        type(check_for_steady_state_in_out_t) :: check_for_steady_state_in_out

        check_for_steady_state_in_out%current_board = current_board
        check_for_steady_state_in_out%new_board = new_board
        check_for_steady_state_in_out%expected_steady_state = steady_state

    end function check_for_steady_state_in_out_constructor
end module check_for_steady_state_test
