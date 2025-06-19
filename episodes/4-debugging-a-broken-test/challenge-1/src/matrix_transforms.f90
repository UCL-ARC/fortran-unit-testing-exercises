module matrix_transforms
    implicit none

    private
    public transpose
contains

    subroutine transpose(matrix)
        real, dimension(:,:), allocatable, intent(inout) :: matrix

        real, dimension(:,:), allocatable :: temp_matrix
        integer :: nrow, ncol, row, col

        nrow = size(matrix, 1)
        ncol = size(matrix, 2)

        allocate(temp_matrix(nrow, ncol))

        temp_matrix = matrix

        do row = 1, nrow
            do col = 1, ncol
                matrix(row, col) = temp_matrix(row, col)
            end do
        end do
    end subroutine transpose
end module matrix_transforms