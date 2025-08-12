!> Main program for running unit tests using the veggies framework
program test_main
    use veggies, only : test_item_t, test_that, run_tests

    use veggies_evolve_board_test, only : evolve_board_test_suite
    use veggies_check_for_steady_state_test, only : check_for_steady_state_test_suite
    use veggies_read_model_from_file_test, only : read_model_from_file_test_suite
    use veggies_find_steady_state_test, only : find_steady_state_test_suite

    implicit none

    !> Run the test suite and stop with an error code if any test fails
    if (.not.run()) stop 1

contains
    !> Run all tests within this project
    function run() result(passed)
        !> Logical flag indicating whether all tests passed
        logical :: passed

        !> The collection of all test items to be executed
        type(test_item_t) :: tests
        !> Array of individual test items to be included in the test suite
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = evolve_board_test_suite()
        individual_tests(2) = check_for_steady_state_test_suite()
        individual_tests(3) = find_steady_state_test_suite()
        individual_tests(4) = read_model_from_file_test_suite()

        ! Group the individual tests into a single test suite
        tests = test_that(individual_tests)

        ! Run the test suite and return whether all tests passed
        passed = run_tests(tests)
    end function run

end program test_main
