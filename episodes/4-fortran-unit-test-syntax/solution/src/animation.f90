module animation
    implicit none
    public

contains

    !> Output the current board to the terminal
    subroutine draw_board(board)
        !> The current state of the board
        integer, dimension(:,:), allocatable, intent(in) :: board

        integer :: row, col, nrow, ncol
        character(:), allocatable :: output

        nrow = size(board, 1)
        ncol = size(board, 2)

        allocate(character(nrow) :: output)

        ! Clear the terminal screen
        call system("clear")

        do row=1, nrow
            output = ""
            do col=1, ncol
                if (board(row,col) == 1) then
                    output = trim(output)//"#"
                else
                    output = trim(output)//"."
                endif
            enddo
            print *, output
        enddo
    end subroutine draw_board

end module animation
