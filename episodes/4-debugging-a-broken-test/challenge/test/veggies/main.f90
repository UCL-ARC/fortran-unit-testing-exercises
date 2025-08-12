!> Main program for running unit tests using the veggies framework
program test_main
    use veggies, only : test_item_t, test_that, run_tests
    use veggies_test_transpose, only : test_transpose_testsuite

    implicit none

    ! Run the test suite and stop with an error code if any test fails
    if (.not.run()) stop 1

contains
    !> Run all tests within this project
    function run() result(passed)
        !> Logical flag indicating whether all tests passed
        logical :: passed

        ! The collection of all test items to be executed
        type(test_item_t) :: tests
        ! Array of individual test items to be included in the test suite
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = test_transpose_testsuite()

        ! Group the individual tests into a single test suite
        tests = test_that(individual_tests)

        ! Run the test suite and return whether all tests passed
        passed = run_tests(tests)
    end function run

end program test_main
