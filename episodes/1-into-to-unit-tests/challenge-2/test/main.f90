program test_main
    use veggies, only : test_item_t, test_that, run_tests

    ! If you wish to run the provided solution replace maths_test with maths_test_solution in the line below
    use maths_test, only : maths_test_suite
    implicit none

    if (.not.run()) stop 1

contains
    !> Runs the entire veggies test suite
    function run() result(passed)
        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = maths_test_suite()

        tests = test_that(individual_tests)

        passed = run_tests(tests)
    end function run
end program test_main
