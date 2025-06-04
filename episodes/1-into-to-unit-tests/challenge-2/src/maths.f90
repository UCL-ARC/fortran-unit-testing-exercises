module maths
    implicit none

    private

    public :: double, factorial

contains

    function double(input) result(output)
        integer, intent(in) :: input
        integer :: output

        output = input * 2
    end function double

    function factorial(n) result(output)
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