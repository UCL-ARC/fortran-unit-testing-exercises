program test_main
    use veggies, only : test_item_t, test_that, run_tests

    use game_of_life_sol_test, only : game_of_life_sol_test_suite

    implicit none

    if (.not.run()) stop 1

contains
    function run() result(passed)
        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = game_of_life_sol_test_suite()

        tests = test_that(individual_tests)

        passed = run_tests(tests)
    end function run
end program test_main
