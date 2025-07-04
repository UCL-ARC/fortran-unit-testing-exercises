program test_main
    use veggies, only : test_item_t, test_that, run_tests

    use veggies_test_transpose, only : test_transpose_testsuite

    implicit none

    if (.not.run()) stop 1

contains
    function run() result(passed)
        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = test_transpose_testsuite()

        tests = test_that(individual_tests)

        passed = run_tests(tests)
    end function run
end program test_main
