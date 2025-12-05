module game_of_life
    use animation, only : draw_board
    implicit none
    public

contains

    !> Find the steady state of the Game of Life board
    subroutine find_steady_state(steady_state, generation_number, input_board)
        !> Whether the board has reached a steady state
        logical, intent(out) :: steady_state
        !> The number of generations that have been processed
        integer, intent(out) :: generation_number
        !> The starting state of the board
        integer, dimension(:,:), allocatable, intent(in) :: input_board

        integer, dimension(:,:), allocatable :: current_board, new_board
        integer, parameter :: max_generations = 100

        !! Animation args
        integer, dimension(8) :: date_time_values
        integer :: mod_ms_step
        integer, parameter :: ms_per_step = 250

        allocate(current_board(size(input_board,1), size(input_board, 2)))
        allocate(new_board(size(input_board,1), size(input_board, 2)))
        current_board = input_board
        new_board = 0

        ! Clear the terminal screen
        call system ("clear")

        ! Iterate until we reach a steady state
        steady_state = .false.
        generation_number = 0
        mod_ms_step = 0
        do while(.not. steady_state .and. generation_number < max_generations)
            ! Advance the simulation in the steps of the requested number of milliseconds
            call date_and_time(VALUES=date_time_values)
            mod_ms_step = mod(date_time_values(8), ms_per_step)

            if (mod_ms_step == 0) then
                call evolve_board(current_board, new_board)
                call check_for_steady_state(steady_state, current_board, new_board)
                current_board = new_board
                call draw_board(current_board)

                generation_number = generation_number + 1
            end if

        end do
    end subroutine find_steady_state

    !> Evolve the board into the state of the next iteration
    subroutine evolve_board(current_board, new_board)
        !> The current state of the board
        integer, dimension(:,:), allocatable, intent(in) :: current_board
        !> The new state of the board
        integer, dimension(:,:), allocatable, intent(inout) :: new_board

        integer :: row, column, sum, nrow, ncol

        nrow = size(current_board, 1)
        ncol = size(current_board, 2)

        do row=2, nrow-1
            do column=2, ncol-1
                sum = 0
                sum = current_board(row, column-1)   &
                    + current_board(row+1, column-1) &
                    + current_board(row+1, column)   &
                    + current_board(row+1, column+1) &
                    + current_board(row, column+1)   &
                    + current_board(row-1, column+1) &
                    + current_board(row-1, column)   &
                    + current_board(row-1, column-1)
                if(current_board(row, column)==1 .and. sum<=1) then
                    new_board(row, column) = 0
                elseif(current_board(row, column)==1 .and. sum<=3) then
                    new_board(row, column) = 1
                elseif(current_board(row, column)==1 .and. sum>=4)then
                    new_board(row, column) = 0
                elseif(current_board(row, column)==0 .and. sum==3)then
                    new_board(row, column) = 1
                endif
            enddo
        enddo

        return
    end subroutine evolve_board

    !> Check if we have reached steady state, i.e. current and new board match
    subroutine check_for_steady_state(steady_state, current_board, new_board)
        !> Whether the board has reached a steady state
        logical, intent(out) :: steady_state
        !> The current state of the board
        integer, dimension(:,:), allocatable, intent(in) :: current_board
        !> The new state of the board
        integer, dimension(:,:), allocatable, intent(inout) :: new_board

        integer :: row, column, nrow, ncol

        nrow = size(current_board, 1)
        ncol = size(current_board, 2)

        do row=1, nrow
            do column=1, ncol
                if (.not. current_board(row, column) == new_board(row, column)) then
                    steady_state = .false.
                    return
                end if
            end do
        end do
        steady_state = .true.
    end subroutine check_for_steady_state

end module game_of_life
