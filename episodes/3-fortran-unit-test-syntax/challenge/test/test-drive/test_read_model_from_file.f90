module testdrive_read_model_from_file_test
    use game_of_life_mod, only : read_model_from_file
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: read_model_from_file_test_suite

    ! TASK: Define a parameter type `read_model_from_file_test_params` to be used for testing game_of_life::read_model_from_file
    ! Your changes here...

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Test Suites
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! TASK: Write the subroutine `read_model_from_file_test_suite` which populates testsuites for testing
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

    ! TASK: Write test subroutines for each of the above scenarios

    !> 1. Reading a valid model (i.e. test/models/zeros_31_31.dat) populates the board as expected
    ! Your changes here...

    !> 2. Reading a model (test/models/zeros_31_31.dat) with nrow more than nrow_max populates the error message es expected
    ! Your changes here...

    !> 3. Reading a model (test/models/zeros_31_31.dat) with ncol more than ncol_max populates the error message es expected
    ! Your changes here...

    !> 4. Reading a model (test/models/empty_-10_10.dat) with nrow less than 1 populates the error message es expected
    ! Your changes here...

    !> 5. Reading a model (test/models/empty_10_-10.dat) with ncol less than 1 populates the error message es expected
    ! Your changes here...

    !> 6. Reading a model which doesn't exist populates the error message es expected
    ! Your changes here...


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! TASK: Finish the subroutines below such that they calls game_of_life::read_model_from_file and asserts
    !       that, for valid model files, the resultant board is what we expect and, for invalid models, we get
    !       the appropriate error message. Ensure any failure message makes it clear what has failed.

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    subroutine check_read_model_from_file_valid_model(error, inputs)
        type(error_type), allocatable, intent(out) :: error
        class(read_model_from_file_test_params), intent(in) :: inputs

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message
        integer :: row, col, nrow, ncol
        character(len=80) :: failure_message

        call read_model_from_file(inputs%input_fname, inputs%max_nrow, inputs%max_ncol, actual_board, actual_io_error_message)

        ! Your changes here...
    end subroutine check_read_model_from_file_valid_model

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    subroutine check_read_model_from_file_invalid_model(error, inputs)
        type(error_type), allocatable, intent(out) :: error
        class(read_model_from_file_test_params), intent(in) :: inputs

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message
        integer :: row, col, nrow, ncol
        character(len=80) :: failure_message

        call read_model_from_file(inputs%input_fname, inputs%max_nrow, inputs%max_ncol, actual_board, actual_io_error_message)

        ! Your changes here...
    end subroutine check_read_model_from_file_invalid_model
end module testdrive_read_model_from_file_test
