module testdrive_test_transpose
    use testdrive, only : error_type, unittest_type, new_unittest, check
    use matrix_transforms, only : transpose
    implicit none

    private
    public :: test_transpose_testsuite

    type :: test_transpose_params
        real, dimension(:,:), allocatable :: input_matrix
        real, dimension(:,:), allocatable :: expected_output_matrix
    end type test_transpose_params

contains

    subroutine test_transpose_testsuite(testsuite)
        !> An array of test cases which together define the test suite. Must of type unittest_type for testdrive to pick it up
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite =[ &
            new_unittest("3x3 identity matrix", test_transpose_3x3_identity), &
            new_unittest("3x3 asymmetric matrix", test_transpose_3x3_asymmetric) &
        ]
    end subroutine test_transpose_testsuite

    subroutine test_transpose_3x3_identity(error)
        type(error_type), allocatable, intent(out) :: error

        type(test_transpose_params) :: params

        allocate(params%input_matrix(3,3))
        allocate(params%expected_output_matrix(3,3))

        params%input_matrix(1,:) = [1,0,0]
        params%input_matrix(2,:) = [0,1,0]
        params%input_matrix(3,:) = [0,0,1]

        params%expected_output_matrix = params%input_matrix

        call check_transpose(error, params)
    end subroutine test_transpose_3x3_identity

    subroutine test_transpose_3x3_asymmetric(error)
        type(error_type), allocatable, intent(out) :: error

        type(test_transpose_params) :: params

        allocate(params%input_matrix(3,3))
        allocate(params%expected_output_matrix(3,3))

        params%input_matrix(1,:) = [1,4,0]
        params%input_matrix(2,:) = [6,1,0]
        params%input_matrix(3,:) = [0,3,1]

        params%expected_output_matrix(1,:) = [1,6,0]
        params%expected_output_matrix(2,:) = [4,1,3]
        params%expected_output_matrix(3,:) = [0,0,1]

        call check_transpose(error, params)
    end subroutine test_transpose_3x3_asymmetric

    subroutine check_transpose(error, params)
        type(error_type), allocatable, intent(out) :: error
        type(test_transpose_params), intent(in) :: params

        real, dimension(:,:), allocatable :: actual_output
        integer :: nrow, ncol, row, col
        character(len=80) :: failure_message

        nrow = size(params%input_matrix, 1)
        ncol = size(params%input_matrix, 2)

        allocate(actual_output(nrow, ncol))

        actual_output = params%input_matrix

        call transpose(actual_output)

        do row = 1, nrow
            do col = 1, ncol
                write(failure_message,'(a,i1,a,i1,a,F3.1,a,F3.1)') "Unexpected value for output(", row, ",", col, "), got ", &
                    actual_output(row, col), " expected ", params%expected_output_matrix(row, col)
                call check(error, params%expected_output_matrix(row, col), actual_output(row, col), failure_message)
                if (allocated(error)) return
            end do
        end do
    end subroutine check_transpose

end module testdrive_test_transpose
