module testdrive_read_model_from_file_test
    use game_of_life_mod, only : read_model_from_file
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: read_model_from_file_test_suite

    ! TASK: Define a parameter type `read_model_from_file_test_params` to be used for testing game_of_life::read_model_from_file
    !> Type to bundle inputs and expected outputs of game_of_life::read_model_from_file
    type :: read_model_from_file_test_params
        character(len=:), allocatable :: input_fname
        integer :: max_nrow
        integer :: max_ncol
        integer, dimension(:,:), allocatable :: expected_board
        character(len=:), allocatable :: expected_io_error_message
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
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: inputs

        inputs%input_fname = "test/models/zeros_31_31.dat"
        inputs%max_nrow = 100
        inputs%max_ncol = 100
        allocate(inputs%expected_board(31, 31))
        inputs%expected_board = 0

        call check_read_model_from_file_valid_model(error, inputs)
    end subroutine test_read_model_from_file_zeros_31_31

    !> 2. Reading a model (test/models/zeros_31_31.dat) with nrow more than nrow_max populates the error message es expected
    subroutine test_read_model_from_file_zeros_31_31_low_max_nrow(error)
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: inputs

        inputs%input_fname = "test/models/zeros_31_31.dat"
        inputs%max_nrow = 10
        inputs%max_ncol = 100
        inputs%expected_io_error_message = "nrow must be a positive integer less than     10 found     31"

        call check_read_model_from_file_invalid_model(error, inputs)
    end subroutine test_read_model_from_file_zeros_31_31_low_max_nrow

    !> 3. Reading a model (test/models/zeros_31_31.dat) with ncol more than ncol_max populates the error message es expected
    subroutine test_read_model_from_file_zeros_31_31_low_max_ncol(error)
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: inputs

        inputs%input_fname = "test/models/zeros_31_31.dat"
        inputs%max_nrow = 100
        inputs%max_ncol = 10
        inputs%expected_io_error_message = "ncol must be a positive integer less than     10 found     31"

        call check_read_model_from_file_invalid_model(error, inputs)
    end subroutine test_read_model_from_file_zeros_31_31_low_max_ncol

    !> 4. Reading a model (test/models/empty_-10_10.dat) with nrow less than 1 populates the error message es expected
    subroutine test_read_model_from_file_empty_minus10_10(error)
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: inputs

        inputs%input_fname = "test/models/empty_-10_10.dat"
        inputs%max_nrow = 100
        inputs%max_ncol = 100
        inputs%expected_io_error_message = "nrow must be a positive integer less than    100 found    -10"

        call check_read_model_from_file_invalid_model(error, inputs)
    end subroutine test_read_model_from_file_empty_minus10_10

    !> 5. Reading a model (test/models/empty_10_-10.dat) with ncol less than 1 populates the error message es expected
    subroutine test_read_model_from_file_empty_10_minus10(error)
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: inputs

        inputs%input_fname = "test/models/empty_10_-10.dat"
        inputs%max_nrow = 100
        inputs%max_ncol = 100
        inputs%expected_io_error_message = "ncol must be a positive integer less than    100 found    -10"

        call check_read_model_from_file_invalid_model(error, inputs)
    end subroutine test_read_model_from_file_empty_10_minus10

    !> 6. Reading a model which doesn't exist populates the error message es expected
    subroutine test_read_model_from_file_non_existent_dat_file(error)
        type(error_type), allocatable, intent(out) :: error

        type(read_model_from_file_test_params) :: inputs

        inputs%input_fname = "does/not/exist.dat"
        inputs%max_nrow = 100
        inputs%max_ncol = 100
        inputs%expected_io_error_message = " *** Error when opening does/not/exist.dat"

        call check_read_model_from_file_invalid_model(error, inputs)
    end subroutine test_read_model_from_file_non_existent_dat_file

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

        ! Verify error message is not allocated
        call check(error, .not. allocated(actual_io_error_message), "Expected actual error message to not be allocate")
        if (allocated(error)) return

        ! Verify board read from file
        nrow = size(inputs%expected_board, 1)
        ncol = size(inputs%expected_board, 2)

        call check(error, nrow, size(actual_board, 1), "unexpected number of rows in actual board")
        if (allocated(error)) return

        call check(error, ncol, size(actual_board, 2), "unexpected number of columns in actual board")
        if (allocated(error)) return

        do col = 1, ncol
            do row = 1, nrow
                write(failure_message,'(a,i1,a,i1,a,i2,a,i2)') "Unexpected value for board(", row, ",", col, "), got ", &
                    actual_board(row, col), " expected ", inputs%expected_board(row, col)
                call check(error, inputs%expected_board(row, col), actual_board(row, col), failure_message)
                if (allocated(error)) return
            end do
        end do
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

        ! Verify error message
        call check(error, allocated(actual_io_error_message), "Expected actual error message to be allocate")
        if (allocated(error)) return

        call check(error, inputs%expected_io_error_message, actual_io_error_message, &
            "Expected error message: '"//inputs%expected_io_error_message//"' but found '"//actual_io_error_message//"'")
        if (allocated(error)) return

        call check(error, .not. allocated(actual_board), "Expected actual board to not nbe allocated")
        if (allocated(error)) return
    end subroutine check_read_model_from_file_invalid_model
end module testdrive_read_model_from_file_test
