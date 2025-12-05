! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life

    implicit none

    !! Board args
    integer, parameter :: max_generations = 100, max_nrows = 100, max_ncols = 100
    integer :: nrow, ncol
    integer :: row, generation_number
    integer, dimension(:,:), allocatable :: current_board, new_board

    !! Animation args
    integer, parameter :: ms_per_step = 250
    integer, dimension(8) :: date_time_values
    integer :: mod_ms_step
    logical :: steady_state = .false.

    !! CLI args
    integer                       :: argl
    character(len=:), allocatable :: cli_arg_temp_store, input_filename

    !! File IO args
    character(len=80) :: text_to_discard
    integer :: input_file_io
    integer :: iostat

    ! Get current_board file path from command line
    if (command_argument_count() == 1) then
        call get_command_argument(1, length=argl)
        allocate(character(argl) :: input_filename)
        call get_command_argument(1, input_filename)
    else
        write(*,'(A)') "Error: Invalid input"
        call get_command_argument(0, length=argl)
        allocate(character(argl) :: cli_arg_temp_store)
        call get_command_argument(0, cli_arg_temp_store)
        write(*,'(A,A,A)') "Usage: ", cli_arg_temp_store, " <input_file_name>"
        deallocate(cli_arg_temp_store)
        stop
    end if

    ! Open input file
    open(unit=input_file_io,   &
         file=input_filename, &
         status='old',  &
         IOSTAT=iostat)

    if( iostat /= 0) then
        write(*,'(a)') ' *** Error when opening '//input_filename
        stop 1
    end if

    ! Read in current_board from file
    read(input_file_io,'(a)') text_to_discard ! Skip first line
    read(input_file_io,*) nrow, ncol

    ! Verify the number of rows read from the file
    if (nrow < 1 .or. nrow > max_nrows) then
        write (*,'(a,i6,a,i6)') "nrow must be a positive integer less than ", max_nrows, " found ", nrow
        stop 1
    end if

    ! Verify the number of columns read from the file
    if (ncol < 1 .or. ncol > max_ncols) then
        write (*,'(a,i6,a,i6)') "ncol must be a positive integer less than ", max_ncols, " found ", ncol
        stop 1
    end if

    allocate(current_board(nrow, ncol))
    allocate(new_board(nrow, ncol))

    read(input_file_io,'(a)') text_to_discard ! Skip next line
    ! Populate the boards starting state
    do row = 1, nrow
        read(input_file_io,*) current_board(row, :)
    end do

    close(input_file_io)

    new_board = 0
    generation_number = 0

    ! Clear the terminal screen
    call system ("clear")

    ! Iterate until we reach a steady state
    do while(.not. steady_state .and. generation_number < max_generations)
        ! Advance the simulation in the steps of the requested number of milliseconds
        call date_and_time(VALUES=date_time_values)
        mod_ms_step = mod(date_time_values(8), ms_per_step)

        if (mod_ms_step == 0) then
            call run_next_iteration()

            generation_number = generation_number + 1
        end if

    end do

    if (steady_state) then
        write(*,'(a,i6,a)') "Reached steady after ", generation_number, " generations"
    else
        write(*,'(a,i6,a)') "Did NOT Reach steady after ", generation_number, " generations"
    end if

    deallocate(current_board)
    deallocate(new_board)

contains

    !> Evolve the board into the state of the next iteration
    subroutine run_next_iteration()
        integer :: row, column, sum
        character(nrow) :: output

        ! Clear the terminal screen
        call system("clear")

        ! Draw the current board
        do row=1, nrow
            output = ""
            do column=1, ncol
                if (current_board(row, column) == 1) then
                    output = trim(output)//"#"
                else
                    output = trim(output)//"."
                endif
            enddo
            print *, output
        enddo

        ! Calculate the new board
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

        ! Check for steady state
        steady_state = .true.
        do row=1, nrow
            do column=1, ncol
                if (.not. current_board(row, column) == new_board(row, column)) then
                    steady_state = .false.
                    exit
                end if
            end do
            if (.not. steady_state) exit
        end do

        current_board = new_board

        return
    end subroutine run_next_iteration

end program game_of_life
