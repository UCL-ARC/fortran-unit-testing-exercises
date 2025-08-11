module testdrive_read_model_from_file_test
    use game_of_life_mod, only : read_model_from_file
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: read_model_from_file_test_suite

    ! TASK: Define a parameter type `read_model_from_file_test_params` to be used for testing game_of_life::read_model_from_file
    !> Type to bundle inputs and expected outputs of game_of_life::read_model_from_file
    type :: read_model_from_file_test_params
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
        !> A description of the test to be outputted for logging
        character(len=100) :: description
    end type read_model_from_file_test_params

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
    !> Test suite for the game_of_life::read_model_from_file subroutine
    subroutine read_model_from_file_test_suite(testsuite)
        !> An array of test cases which together define the test suite. Must of type unittest_type for testdrive to pick it up
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_read_model_from_file_zeros_31_31", test_read_model_from_file_zeros_31_31), &
            new_unittest( &
                "test_read_model_from_file_zeros_31_31_low_max_nrow", test_read_model_from_file_zeros_31_31_low_max_nrow &
            ), &
            new_unittest( &
                "test_read_model_from_file_zeros_31_31_low_max_ncol", test_read_model_from_file_zeros_31_31_low_max_ncol &
            ), &
            new_unittest("test_read_model_from_file_empty_minus10_10", test_read_model_from_file_empty_minus10_10), &
            new_unittest("test_read_model_from_file_empty_10_minus10", test_read_model_from_file_empty_10_minus10), &
            new_unittest("test_read_model_from_file_non_existent_dat_file", test_read_model_from_file_non_existent_dat_file) &
        ]

    end subroutine read_model_from_file_test_suite

    ! TASK: Write test subroutines for each of the above scenarios

    !> 1. Reading a valid model (i.e. test/models/zeros_31_31.dat) populates the board as expected
    subroutine test_read_model_from_file_zeros_31_31(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: params

        params%description = "Valid 31x31 model"
        params%input_fname = "test/models/zeros_31_31.dat"
        params%max_nrow = 100
        params%max_ncol = 100
        allocate(params%expected_board(31, 31))
        params%expected_board = 0

        call check_read_model_from_file_valid_model(error, params)
    end subroutine test_read_model_from_file_zeros_31_31

    !> 2. Reading a model (test/models/zeros_31_31.dat) with nrow more than nrow_max populates the error message es expected
    subroutine test_read_model_from_file_zeros_31_31_low_max_nrow(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: params

        params%description = "Invalid model with nrow too large"
        params%input_fname = "test/models/zeros_31_31.dat"
        params%max_nrow = 10
        params%max_ncol = 100
        params%expected_io_error_message = "nrow must be a positive integer less than     10 found     31"

        call check_read_model_from_file_invalid_model(error, params)
    end subroutine test_read_model_from_file_zeros_31_31_low_max_nrow

    !> 3. Reading a model (test/models/zeros_31_31.dat) with ncol more than ncol_max populates the error message es expected
    subroutine test_read_model_from_file_zeros_31_31_low_max_ncol(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: params

        params%description = "Invalid model with ncol too large"
        params%input_fname = "test/models/zeros_31_31.dat"
        params%max_nrow = 100
        params%max_ncol = 10
        params%expected_io_error_message = "ncol must be a positive integer less than     10 found     31"

        call check_read_model_from_file_invalid_model(error, params)
    end subroutine test_read_model_from_file_zeros_31_31_low_max_ncol

    !> 4. Reading a model (test/models/empty_-10_10.dat) with nrow less than 1 populates the error message es expected
    subroutine test_read_model_from_file_empty_minus10_10(error)
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: params

        params%description = "Invalid model with nrow less than zero"
        params%input_fname = "test/models/empty_-10_10.dat"
        params%max_nrow = 100
        params%max_ncol = 100
        params%expected_io_error_message = "nrow must be a positive integer less than    100 found    -10"

        call check_read_model_from_file_invalid_model(error, params)
    end subroutine test_read_model_from_file_empty_minus10_10

    !> 5. Reading a model (test/models/empty_10_-10.dat) with ncol less than 1 populates the error message es expected
    subroutine test_read_model_from_file_empty_10_minus10(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: params

        params%description = "Invalid model with ncol less than zero"
        params%input_fname = "test/models/empty_10_-10.dat"
        params%max_nrow = 100
        params%max_ncol = 100
        params%expected_io_error_message = "ncol must be a positive integer less than    100 found    -10"

        call check_read_model_from_file_invalid_model(error, params)
    end subroutine test_read_model_from_file_empty_10_minus10

    !> 6. Reading a model which doesn't exist populates the error message es expected
    subroutine test_read_model_from_file_non_existent_dat_file(error)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: params

        params%description = "Model which does not exist"
        params%input_fname = "does/not/exist.dat"
        params%max_nrow = 100
        params%max_ncol = 100
        params%expected_io_error_message = " *** Error when opening does/not/exist.dat"

        call check_read_model_from_file_invalid_model(error, params)
    end subroutine test_read_model_from_file_non_existent_dat_file

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Assertion functions
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! TASK: Finish the subroutines below such that they calls game_of_life::read_model_from_file and asserts
    !       that, for valid model files, the resultant board is what we expect and, for invalid models, we get
    !       the appropriate error message. Ensure any failure message makes it clear what has failed.

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    subroutine check_read_model_from_file_valid_model(error, params)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error
        !> The current test parameters including inputs and expected outputs
        type(read_model_from_file_test_params), intent(in) :: params

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message
        integer :: row, col, nrow, ncol
        character(len=100) :: failure_message

        call read_model_from_file(params%input_fname, params%max_nrow, params%max_ncol, actual_board, actual_io_error_message)

        ! Verify error message is not allocated
        write(failure_message, '(a,a,a,a)') trim(params%description), &
            ": Expected actual error message to not be allocate but found '", trim(actual_io_error_message), "'"
        call check(error, .not. allocated(actual_io_error_message), failure_message)
        if (allocated(error)) return

        ! Verify board read from file
        nrow = size(params%expected_board, 1)
        ncol = size(params%expected_board, 2)

        write(failure_message, '(a,a,i3,a,i3)') trim(params%description), ": unexpected number of rows in actual board. Expected", &
            nrow, " but found ", size(actual_board, 1)
        call check(error, nrow, size(actual_board, 1), failure_message)
        if (allocated(error)) return

        write(failure_message, '(a,a,i3,a,i3)') trim(params%description), &
            ": unexpected number of columns in actual board. Expected", ncol, " but found ", size(actual_board, 2)
        call check(error, ncol, size(actual_board, 2), failure_message)
        if (allocated(error)) return

        do col = 1, ncol
            do row = 1, nrow
                write(failure_message,'(a,i1,a,i1,a,i2,a,i2)') "Unexpected value for board(", row, ",", col, "), got ", &
                    actual_board(row, col), " expected ", params%expected_board(row, col)
                call check(error, params%expected_board(row, col), actual_board(row, col), failure_message)
                if (allocated(error)) return
            end do
        end do
    end subroutine check_read_model_from_file_valid_model

    !> Check for the expected output of the game_of_life::read_model_from_file subroutine
    subroutine check_read_model_from_file_invalid_model(error, params)
        !> An error to track the status of assertions throughout the test i.e. if allocated this error indicates an assertion &
        !> failed. Must be of type error_type to be picked up by testdrive
        type(error_type), allocatable, intent(out) :: error
        !> The current test parameters including inputs and expected outputs
        type(read_model_from_file_test_params), intent(in) :: params

        integer, dimension(:,:), allocatable ::actual_board
        character(len=:), allocatable :: actual_io_error_message
        integer :: row, col, nrow, ncol
        character(len=250) :: failure_message

        call read_model_from_file(params%input_fname, params%max_nrow, params%max_ncol, actual_board, actual_io_error_message)

        ! Verify error message
        write(failure_message, '(a,a)') trim(params%description), ": Expected actual error message to be allocate"
        call check(error, allocated(actual_io_error_message), failure_message)
        if (allocated(error)) return

        write(failure_message, '(a,a,a,a,a,a)') trim(params%description), ": Expected error message '", &
            trim(params%expected_io_error_message), "' but found '", trim(actual_io_error_message), "'"
        call check(error, params%expected_io_error_message, actual_io_error_message, failure_message)
        if (allocated(error)) return

        write(failure_message, '(a,a)') trim(params%description), ": Expected actual board to not be allocated"
        call check(error, .not. allocated(actual_board), failure_message)
        if (allocated(error)) return
    end subroutine check_read_model_from_file_invalid_model
end module testdrive_read_model_from_file_test
