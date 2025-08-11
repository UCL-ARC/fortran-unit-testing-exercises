module maths
    implicit none

    private

    public :: double, factorial

contains
    !> Returns double the input value passed in
    function double(input) result(output)
        !> The input value to be doubled
        integer, intent(in) :: input
        integer :: output

        output = input * 2
    end function double

    !> Returns the factorial of the input value n (!n)
    function factorial(n) result(output)
        !> The value who's factorial will be returned
        integer, intent(in) :: n
        integer :: output, i

        output = 1
        if (n > 0) then
            do i = 2, n
                output = output * i
            end do
        end if
    end function factorial
end module maths
