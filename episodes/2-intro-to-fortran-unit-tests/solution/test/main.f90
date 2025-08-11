program test_main
    use veggies, only : test_item_t, test_that, run_tests

    use check_for_steady_state_test, only : check_for_steady_state_test_suite
    use evolve_board_test, only : evolve_board_test_suite
    use find_steady_state_test, only : find_steady_state_test_suite
    use read_model_from_file_test, only : read_model_from_file_test_suite

    implicit none

    if (.not.run()) stop 1

contains
    !> Run all tests within this project
    function run() result(passed)
        !> true if all tests have passed successfully
        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(4)

        individual_tests(1) = check_for_steady_state_test_suite()
        individual_tests(2) = evolve_board_test_suite()
        individual_tests(3) = find_steady_state_test_suite()
        individual_tests(4) = read_model_from_file_test_suite()

        tests = test_that(individual_tests)

        passed = run_tests(tests)
    end function run
end program test_main
