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
    ! Your changes here...

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
    ! Your changes here...

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

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message

        ! Your changes here...
    end function check_read_model_from_file

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    function check_read_model_from_file_with_invalid_model(input) result(result_)
        !> The current test case including inputs and expected outputs, must be of type input_t to be picked up by veggies
        class(input_t), intent(in) :: input
        !> the result of the current test case, must be of type result_t to be picked up by veggies
        type(result_t) :: result_

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message

        ! Your changes here...
    end function check_read_model_from_file_with_invalid_model
end module veggies_read_model_from_file_test
