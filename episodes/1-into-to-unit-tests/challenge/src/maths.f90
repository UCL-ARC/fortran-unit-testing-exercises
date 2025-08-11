module maths
    implicit none

    private

    public :: double, factorial

contains

    !> Doubles whatever input it is provided
    function double(input) result(output)
        !> The input value to be doubled
        integer, intent(in) :: input
        !> The result of doubling the input
        integer :: output

        output = input * 2
    end function double

    !> Calculates the factorial of the provided input
    function factorial(input) result(output)
        !> The input value whose factorial should be calculated
        integer, intent(in) :: input
        !> The result of the factorial of the input
        integer :: output

        integer :: i

        output = 1
        if (input > 0) then
            do i = 2, input
                output = output * i
            end do
        end if
    end function factorial
end module maths
