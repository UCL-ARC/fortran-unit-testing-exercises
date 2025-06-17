program test_main
    use veggies, only : test_item_t, test_that, run_tests

    use veggies_evolve_board_test, only : evolve_board_test_suite
    use veggies_check_for_steady_state_test, only : check_for_steady_state_test_suite

    implicit none

    if (.not.run()) stop 1

contains
    function run() result(passed)
        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = evolve_board_test_suite()
        individual_tests(2) = check_for_steady_state_test_suite()

        tests = test_that(individual_tests)

        passed = run_tests(tests)
    end function run
end program test_main
