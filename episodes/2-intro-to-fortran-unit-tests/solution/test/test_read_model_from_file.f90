module read_model_from_file_test
    use game_of_life_mod, only : read_model_from_file
    use veggies, only:            &
        assert_that,              &
        assert_equals,            &
        describe,                 &
        example_t,                &
        fail,                     &
        input_t,                  &
        it,                       &
        result_t,                 &
        test_item_t,              &
        assert_not
    implicit none

    private
    public :: read_model_from_file_test_suite

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

    !> Test suite for the game_of_life::read_model_from_file subroutine
    function read_model_from_file_test_suite() result(tests)
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
    end function read_model_from_file_test_suite

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    ! Constructors
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
end module read_model_from_file_test
