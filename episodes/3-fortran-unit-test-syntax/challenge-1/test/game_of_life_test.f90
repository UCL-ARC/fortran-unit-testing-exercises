module game_of_life_test
    use game_of_life_mod, only : check_for_steady_state, evolve_board, read_model_from_file
    use veggies, only:            &
        assert_that,              &
        assert_equals,            &
        describe,                 &
        example_t,                &
        fail,                     &
        input_t,                  &
        it,                       &
        result_t,                 &
        test_item_t, assert_not
    implicit none

    private
    public :: check_for_steady_state_tests, evolve_board_tests, read_model_from_file_tests

    !> Type to bundle inputs and expected outputs of game_of_life::check_for_steady_state
    type, extends(input_t) :: check_for_steady_state_in_out_t
        integer, dimension(:,:), allocatable :: current_board, new_board
        logical :: expected_steady_state
    end type check_for_steady_state_in_out_t
    interface check_for_steady_state_in_out_t
        module procedure check_for_steady_state_in_out_constructor
    end interface check_for_steady_state_in_out_t

    !> Type to bundle inputs and expected outputs of game_of_life::evolve_board
    type, extends(input_t) :: evolve_board_in_out_t
        integer, dimension(:,:), allocatable :: current_board, expected_new_board
    end type evolve_board_in_out_t
    interface evolve_board_in_out_t
        module procedure evolve_board_in_out_constructor
    end interface evolve_board_in_out_t

    !> Type to bundle inputs and expected outputs of game_of_life::read_model_from_file
    type, extends(input_t) :: read_model_from_file_in_out_t
        character(len=:), allocatable :: input_fname
        integer :: max_nrow
        integer :: max_ncol
        integer, dimension(:,:), allocatable :: expected_board
        character(len=:), allocatable :: expected_io_error_message
    end type read_model_from_file_in_out_t
    interface read_model_from_file_in_out_t
        module procedure read_model_from_file_in_out_constructor
    end interface read_model_from_file_in_out_t

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Test suite for the game_of_life::check_for_steady_state subroutine
    function check_for_steady_state_tests() result(tests)
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
    end function check_for_steady_state_tests

    !> Test suite for the game_of_life::evolve_board subroutine
    function evolve_board_tests() result(tests)
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
        steady_state_boards_data(1) = example_t(evolve_board_in_out_t(test_current_board, expected_new_board))

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
        steady_state_boards_data(2) = example_t(evolve_board_in_out_t(test_current_board, expected_new_board))
        !  Reset for next test
        test_current_board = 0
        expected_new_board = 0

        ! None-steady state boards
        !  One non-zero element
        !   Input board
        test_current_board(10,9) = 1
        non_steady_state_boards_data(1) = example_t(evolve_board_in_out_t(test_current_board, expected_new_board))
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
        non_steady_state_boards_data(2) = example_t(evolve_board_in_out_t(test_current_board, expected_new_board))
        !  Reset for next test
        test_current_board = 0
        expected_new_board = 0

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

        deallocate(test_current_board)
        deallocate(expected_new_board)
    end function evolve_board_tests

    function read_model_from_file_tests() result(tests)
        type(test_item_t) :: tests

        integer, dimension(:,:), allocatable :: test_board
        character(len=:), allocatable :: test_io_error_message
        type(example_t) :: valid_model_file_data(1), invalid_model_file_data(5)

        allocate(test_board(31, 31))
        test_board = 0

        valid_model_file_data(1) = example_t( &
            read_model_from_file_in_out_t("test/models/zeros_31_31.dat", 100, 100, test_board, test_io_error_message) &
        )

        deallocate(test_board)

        allocate(character(100) :: test_io_error_message)

        test_io_error_message = "nrow must be a positive integer less than     10 found     31"
        invalid_model_file_data(1) = example_t( &
            read_model_from_file_in_out_t("test/models/zeros_31_31.dat", 10, 100, test_board, test_io_error_message) &
        )

        test_io_error_message = "ncol must be a positive integer less than     10 found     31"
        invalid_model_file_data(2) = example_t( &
            read_model_from_file_in_out_t("test/models/zeros_31_31.dat", 100, 10, test_board, test_io_error_message) &
        )

        test_io_error_message = "nrow must be a positive integer less than    100 found    -10"
        invalid_model_file_data(3) = example_t( &
            read_model_from_file_in_out_t("test/models/empty_-10_10.dat", 100, 100, test_board, test_io_error_message) &
        )

        test_io_error_message = "ncol must be a positive integer less than    100 found    -10"
        invalid_model_file_data(4) = example_t( &
            read_model_from_file_in_out_t("test/models/empty_10_-10.dat", 100, 100, test_board, test_io_error_message) &
        )

        test_io_error_message = " *** Error when opening does/not/exist.dat"
        invalid_model_file_data(5) = example_t( &
            read_model_from_file_in_out_t("does/not/exist.dat", 100, 100, test_board, test_io_error_message) &
        )

        deallocate(test_io_error_message)

        tests = describe( &
            "read_model_from_file", &
            [ it( &
                "a valid model file is loaded successfully", &
                valid_model_file_data, &
                check_read_model_from_file &
            ), &
            it( &
                "a invalid model file is loaded unsuccessfully", &
                invalid_model_file_data, &
                check_read_model_from_file_with_invalid_model &
            )] &
        )
    end function read_model_from_file_tests

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

    !> Check for the expected output of the game_of_life::evolve_board subroutine
    function check_evolve_board(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer, dimension(:,:), allocatable ::actual_new_board

        select type (input)
        type is (evolve_board_in_out_t)
            allocate(actual_new_board(size(input%current_board, 1), size(input%current_board, 2)))
            actual_new_board = input%current_board

            call evolve_board(input%current_board, actual_new_board)

            result_ = assert_equals(input%expected_new_board, actual_new_board)

            deallocate(actual_new_board)
        class default
            result_ = fail("Didn't get evolve_board_in_out_t")

        end select

    end function check_evolve_board

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    function check_read_model_from_file(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message

        select type (input)
        type is (read_model_from_file_in_out_t)
            call read_model_from_file(input%input_fname, input%max_nrow, input%max_ncol, actual_board, actual_io_error_message)

            result_ = assert_equals(input%expected_board, actual_board) .and. &
                      assert_not(allocated(actual_io_error_message))

            if (allocated(actual_board)) then
                deallocate(actual_board)
            end if
        class default
            result_ = fail("Didn't get read_model_from_file_in_out_t")

        end select

    end function check_read_model_from_file

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    function check_read_model_from_file_with_invalid_model(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message

        select type (input)
        type is (read_model_from_file_in_out_t)
            call read_model_from_file(input%input_fname, input%max_nrow, input%max_ncol, actual_board, actual_io_error_message)

            result_ = assert_not(allocated(actual_board)) .and. &
                      assert_that(allocated(actual_io_error_message)) .and. &
                      assert_equals(trim(input%expected_io_error_message), trim(actual_io_error_message))

        class default
            result_ = fail("Didn't get read_model_from_file_in_out_t")

        end select

    end function check_read_model_from_file_with_invalid_model

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

    function evolve_board_in_out_constructor(current_board, expected_new_board) result(evolve_board_in_out)
        integer, dimension(:,:), allocatable, intent(in) :: current_board, expected_new_board

        type(evolve_board_in_out_t) :: evolve_board_in_out

        evolve_board_in_out%current_board = current_board
        evolve_board_in_out%expected_new_board = expected_new_board
    end function evolve_board_in_out_constructor

    function read_model_from_file_in_out_constructor( &
                input_fname, max_nrow, max_ncol, expected_board, expected_io_error_message) &
                result(read_model_from_file_in_out)
        character(len=:), allocatable, intent(in) :: input_fname
        integer, intent(in) :: max_nrow
        integer, intent(in) :: max_ncol
        integer, dimension(:,:), allocatable, intent(in) :: expected_board
        character(len=:), allocatable, intent(in) :: expected_io_error_message

        type(read_model_from_file_in_out_t) :: read_model_from_file_in_out

        read_model_from_file_in_out%input_fname = input_fname
        read_model_from_file_in_out%max_nrow = max_nrow
        read_model_from_file_in_out%max_ncol = max_ncol
        read_model_from_file_in_out%expected_board = expected_board
        read_model_from_file_in_out%expected_io_error_message = expected_io_error_message
    end function read_model_from_file_in_out_constructor
end module game_of_life_test
