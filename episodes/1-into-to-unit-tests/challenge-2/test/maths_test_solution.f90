module maths_test_solution
    use maths, only : double, factorial
    use veggies, only: &
            assert_equals, &
            describe, &
            example_t, &
            fail, &
            input_t, &
            it, &
            result_t, &
            test_item_t
    implicit none
    private

    public :: maths_test_suite

    !! @class test_parameters_t
    !!
    !! @brief A class to store test parameters
    type, extends(input_t) :: test_parameters_t
        integer :: input_value, expected_value
    end type test_parameters_t

    interface test_parameters_t
        module procedure test_parameters_constructor
    end interface test_parameters_t

contains

    !> A constructor for the test_parameters_t type
    pure function test_parameters_constructor(input_value, expected_value) result(test_parameters)
        integer, intent(in) :: input_value, expected_value
        type(test_parameters_t) :: test_parameters

        test_parameters%input_value = input_value
        test_parameters%expected_value = expected_value
    end function test_parameters_constructor

    !> The test suite for the maths module tests
    function maths_test_suite() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                    "maths", &
                    [ it( &
                        "double works as expected", &
                        [ example_t(test_parameters_t(2, 4)) &
                        , example_t(test_parameters_t(0, 0)) &
                        , example_t(test_parameters_t(-1, -2)) &
                        , example_t(test_parameters_t(5000, 10000)) &
                        ], &
                        test_double) &
                    , it( &
                        "factorial works as expected", &
                        [ example_t(test_parameters_t(4, 24)) &
                        , example_t(test_parameters_t(0, 1)) &
                        , example_t(test_parameters_t(10, 3.6288e6)) &
                        ], &
                        test_factorial) &
                    ])
    end function maths_test_suite

    !> A unit test for the maths::double function.
    !!
    !! @param inputs - An instance of the test_parameters_t containing function
    !!                 inputs and expected outputs.
    !!
    !! @returns result_ - The result of the test (pass or fail) of type result_t
    function test_double(input) result(result_)
        implicit none

        ! Setting the values of actual/expected_value outside of the test allows many different inputs to be tested using the same code.
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer :: actual_value

        select type (input)
        type is (test_parameters_t)
            ! When we double our input
            actual_value = double(input%input_value)

            ! Then we expect the actual_value to match the expected_value
            result_ = assert_equals(input%expected_value, actual_value)
        class default
            result_ = fail("Didn't get test_parameters_t")
        end select

    end function test_double

    !> A unit test for the maths::factorial function.
    !!
    !! @param inputs - An instance of the test_parameters_t containing function
    !!                 inputs and expected outputs.
    !!
    !! @returns result_ - The result of the test (pass or fail) of type result_t
    function test_factorial(input) result(result_)
        implicit none

        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer :: actual_value

        select type (input)
        type is (test_parameters_t)
            ! When we get the factorial of our input
            actual_value = factorial(input%input_value)

            ! Then we expect the actual_value to match the expected_value
            result_ = assert_equals(input%expected_value, actual_value)
        class default
            result_ = fail("Didn't get test_parameters_t")
        end select
    end function test_factorial

end module maths_test_solution