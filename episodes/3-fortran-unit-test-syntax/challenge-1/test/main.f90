program test_main
    use veggies, only : test_item_t, test_that, run_tests

    use game_of_life_test, only : check_for_steady_state_tests, evolve_board_tests, read_model_from_file_tests

    implicit none

    if (.not.run()) stop 1

contains
    function run() result(passed)
        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = check_for_steady_state_tests()
        individual_tests(2) = evolve_board_tests()
        individual_tests(3) = read_model_from_file_tests()

        tests = test_that(individual_tests)

        passed = run_tests(tests)
    end function run
end program test_main
