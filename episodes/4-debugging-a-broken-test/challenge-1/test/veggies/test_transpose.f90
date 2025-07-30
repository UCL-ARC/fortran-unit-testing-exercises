module veggies_test_transpose
    use veggies, only : &
        assert_equals_within_absolute, &
        describe, &
        example_t, &
        fail, &
        input_t, &
        it, &
        result_t, &
        test_item_t
    use matrix_transforms, only : transpose
    implicit none

    private
    public :: test_transpose_testsuite

    type, extends(input_t) :: test_transpose_params
        real, dimension(:,:), allocatable :: input_matrix
        real, dimension(:,:), allocatable :: expected_output_matrix
    end type test_transpose_params

contains

    function test_transpose_testsuite() result(tests)
        type(test_item_t) :: tests

        type(example_t) :: test_data(2)

        real, dimension(3,3) :: input_matrix, expected_output_matrix

        input_matrix(1,:) = [1,0,0]
        input_matrix(2,:) = [0,1,0]
        input_matrix(3,:) = [0,0,1]

        expected_output_matrix = input_matrix

        test_data(1) = example_t(test_transpose_params(input_matrix, expected_output_matrix))

        input_matrix(1,:) = [1,4,0]
        input_matrix(2,:) = [6,1,0]
        input_matrix(3,:) = [0,3,1]

        expected_output_matrix(1,:) = [1,6,0]
        expected_output_matrix(2,:) = [4,1,3]
        expected_output_matrix(3,:) = [0,0,1]

        test_data(2) = example_t(test_transpose_params(input_matrix, expected_output_matrix))

        tests = describe( &
            "transpose", &
            [ it( &
                "a matrix is transposed as expected", &
                test_data, &
                check_transpose &
            )] &
        )
    end function test_transpose_testsuite

    function check_transpose(params) result(result_)
        class(input_t), intent(in) :: params
        type(result_t) :: result_

        real, dimension(:,:), allocatable :: actual_output_matrix

        select type (params)
        type is (test_transpose_params)
            allocate(actual_output_matrix(size(params%input_matrix, 1), size(params%input_matrix, 2)))
            actual_output_matrix = params%input_matrix

            call transpose(actual_output_matrix)

            result_ = assert_equals_within_absolute(params%expected_output_matrix, actual_output_matrix, 1e-5)
        class default
            result_ = fail("Didn't get test_transpose_params")
        end select
    end function check_transpose
end module veggies_test_transpose
