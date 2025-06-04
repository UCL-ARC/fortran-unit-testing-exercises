module maths_test
    use maths, only : double, factorial
    use veggies, only : assert_equals, describe, input_t, it, result_t, test_item_t
    implicit none

    private

    public :: maths_test_suite

contains

    function maths_test_suite() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                    "maths", &
                    [ it( &
                        "works as expected", &
                        test_maths) &
                    ])
    end function maths_test_suite

    !> A unit test for the maths module.
    !!
    !! @returns result_ - The result of the test (pass or fail) of type result_t
    function test_maths() result(result_)
        implicit none

        type(result_t) :: result_

        integer :: actual_value, expected_value, input

        ! Given we have an input of 2
        input = 2
        expected_value = 24

        ! When we apply our maths operations
        actual_value = double(input)
        actual_value = factorial(actual_value)

        ! Then we expect the actual_value to match the expected_value
        result_ = assert_equals(actual_value, expected_value)
    end function test_maths

end module maths_test
