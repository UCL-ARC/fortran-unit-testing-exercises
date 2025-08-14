module veggies_read_model_from_file_test
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

    ! TASK: Define a parameter type `read_model_from_file_test_params` to be used for testing game_of_life::read_model_from_file
    !> Type to bundle inputs and expected outputs of game_of_life::read_model_from_file
    type, extends(input_t) :: read_model_from_file_test_params
        !> The file to test reading
        character(len=:), allocatable :: input_fname
        !> The maximum number of rows to set for this test
        integer :: max_nrow
        !> The maximum number of column to set for this test
        integer :: max_ncol
        !> The expected board to be read from the input file
        integer, dimension(:,:), allocatable :: expected_board
        !> The expected error message to be populated if an error occurs
        character(len=:), allocatable :: expected_io_error_message
    end type read_model_from_file_test_params

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! TASK: Write the function `read_model_from_file_test_suite` which populates testsuites for testing
    !       game_of_life::read_model_from_file including tests of the following scenarios.
    !
    !       1. Reading a valid model (i.e. test/models/zeros_31_31.dat) populates the board as expected
    !       2. Reading a model (test/models/zeros_31_31.dat) with nrow more than nrow_max populates the error message es expected
    !       3. Reading a model (test/models/zeros_31_31.dat) with ncol more than ncol_max populates the error message es expected
    !       4. Reading a model (test/models/empty_-10_10.dat) with nrow less than 1 populates the error message es expected
    !       5. Reading a model (test/models/empty_10_-10.dat) with ncol less than 1 populates the error message es expected
    !       6. Reading a model which doesn't exist populates the error message es expected
    !
    !> Test suite for the game_of_life::read_model_from_file subroutine
    function read_model_from_file_test_suite() result(tests)
        !> The collection of tests which make up this test suite. Must be of type test_item_t to be picked up by veggies
        type(test_item_t) :: tests

        integer, dimension(:,:), allocatable :: test_board
        character(len=:), allocatable :: test_io_error_message
        type(example_t) :: valid_model_file_data(1), invalid_model_file_data(5)

        allocate(test_board(31, 31))
        test_board = 0

        valid_model_file_data(1) = example_t( &
            read_model_from_file_test_params("test/models/zeros_31_31.dat", 100, 100, test_board, test_io_error_message) &
        )

        deallocate(test_board)

        allocate(character(100) :: test_io_error_message)

        test_io_error_message = "nrow must be a positive integer less than     10 found     31"
        invalid_model_file_data(1) = example_t( &
            read_model_from_file_test_params("test/models/zeros_31_31.dat", 10, 100, null(), test_io_error_message) &
        )

        test_io_error_message = "ncol must be a positive integer less than     10 found     31"
        invalid_model_file_data(2) = example_t( &
            read_model_from_file_test_params("test/models/zeros_31_31.dat", 100, 10, null(), test_io_error_message) &
        )

        test_io_error_message = "nrow must be a positive integer less than    100 found    -10"
        invalid_model_file_data(3) = example_t( &
            read_model_from_file_test_params("test/models/empty_-10_10.dat", 100, 100, null(), test_io_error_message) &
        )

        test_io_error_message = "ncol must be a positive integer less than    100 found    -10"
        invalid_model_file_data(4) = example_t( &
            read_model_from_file_test_params("test/models/empty_10_-10.dat", 100, 100, null(), test_io_error_message) &
        )

        test_io_error_message = " *** Error when opening does/not/exist.dat"
        invalid_model_file_data(5) = example_t( &
            read_model_from_file_test_params("does/not/exist.dat", 100, 100, null(), test_io_error_message) &
        )

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

    ! TASK: Finish the functions below such that they call game_of_life::read_model_from_file and assert
    !       that, for valid model files, the resultant board is what we expect and, for invalid models, we get
    !       the appropriate error message.

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    function check_read_model_from_file(input) result(result_)
        !> The current test case including inputs and expected outputs, must be of type input_t to be picked up by veggies
        class(input_t), intent(in) :: input
        !> the result of the current test case, must be of type result_t to be picked up by veggies
        type(result_t) :: result_

        integer, dimension(:,:), allocatable :: actual_board
        character(len=:), allocatable :: actual_io_error_message

        select type (input)
        type is (read_model_from_file_test_params)
            call read_model_from_file(input%input_fname, input%max_nrow, input%max_ncol, actual_board, actual_io_error_message)

            result_ = assert_equals(input%expected_board, actual_board) .and. &
                      assert_not(allocated(actual_io_error_message))
        class default
            result_ = fail("Didn't get read_model_from_file_test_params")

        end select

    end function check_read_model_from_file

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    function check_read_model_from_file_with_invalid_model(input) result(result_)
        !> The current test case including inputs and expected outputs, must be of type input_t to be picked up by veggies
        class(input_t), intent(in) :: input
        !> the result of the current test case, must be of type result_t to be picked up by veggies
        type(result_t) :: result_

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message

        select type (input)
        type is (read_model_from_file_test_params)
            call read_model_from_file(input%input_fname, input%max_nrow, input%max_ncol, actual_board, actual_io_error_message)

            result_ = assert_not(allocated(actual_board)) .and. &
                      assert_that(allocated(actual_io_error_message)) .and. &
                      assert_equals(trim(input%expected_io_error_message), trim(actual_io_error_message))

        class default
            result_ = fail("Didn't get read_model_from_file_test_params")

        end select

    end function check_read_model_from_file_with_invalid_model
end module veggies_read_model_from_file_test
