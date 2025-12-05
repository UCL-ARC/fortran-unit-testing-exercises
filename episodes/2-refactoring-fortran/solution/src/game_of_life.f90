! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life

    implicit none

    integer, dimension(:,:), allocatable :: starting_board
    integer :: generation_number
    logical :: steady_state = .false.

    !! CLI args
    character(len=:), allocatable :: executable_name, input_filename

    ! Get current_board file path from command line
    if (command_argument_count() == 1) then
        call read_cli_arg(1, input_filename)
    else
        write(*,'(A)') "Error: Invalid input"
        call read_cli_arg(0, executable_name)
        write(*,'(A,A,A)') "Usage: ", executable_name, " <input_file_name>"
        stop
    end if

    call read_model_from_file(input_filename, starting_board)

    call find_steady_state(steady_state, generation_number, starting_board)

    if (steady_state) then
        write(*,'(a,i6,a)') "Reached steady after ", generation_number, " generations"
    else
        write(*,'(a,i6,a)') "Did NOT Reach steady after ", generation_number, " generations"
    end if

contains

    !> Read a cli arg at a given index and return it as a string (character array)
    subroutine read_cli_arg(arg_index, arg)
        !> The index of the cli arg to try and read
        integer, intent(in) :: arg_index
        !> The string into which to store the cli arg
        character(len=:), allocatable, intent(out) :: arg

        integer                       :: argl
        character(len=:), allocatable :: cli_arg_temp_store

        call get_command_argument(arg_index, length=argl)
        allocate(character(argl) :: cli_arg_temp_store)
        call get_command_argument(arg_index, cli_arg_temp_store)
        arg = trim(cli_arg_temp_store)
    end subroutine read_cli_arg

    !> Populate the board from a provided file
    subroutine read_model_from_file(input_filename, board)
        character(len=:), allocatable, intent(in) :: input_filename
        integer, dimension(:,:), allocatable, intent(out) :: board

        !> A flag to indicate if reading the file was successful
        character(len=:), allocatable :: io_error_message

        ! Board definition args
        integer :: row, nrow, ncol
        integer, parameter :: max_nrows = 100, max_ncols = 100

        ! File IO args
        integer :: input_file_io, iostat
        character(len=80) :: text_to_discard

        input_file_io = 1111

        ! Open input file
        open(unit=input_file_io,   &
            file=input_filename, &
            status='old',  &
            IOSTAT=iostat)

        if( iostat == 0) then
            ! Read in board from file
            read(input_file_io,'(a)') text_to_discard ! Skip first line
            read(input_file_io,*) nrow, ncol

            ! Verify the number of rows and columns read from the file
            if (nrow < 1 .or. nrow > max_nrows) then
                allocate(character(100) :: io_error_message)
                write (io_error_message,'(a,i6,a,i6)') "nrow must be a positive integer less than ", max_nrows, " found ", nrow
            elseif (ncol < 1 .or. ncol > max_ncols) then
                allocate(character(100) :: io_error_message)
                write (io_error_message,'(a,i6,a,i6)') "ncol must be a positive integer less than ", max_ncols, " found ", ncol
            end if
        else
            allocate(character(100) :: io_error_message)
            write(io_error_message,'(a)') ' *** Error when opening '//input_filename
        endif

        if (.not. allocated(io_error_message)) then

            allocate(board(nrow, ncol))

            read(input_file_io,'(a)') text_to_discard ! Skip next line
            ! Populate the boards starting state
            do row = 1, nrow
                read(input_file_io,*) board(row, :)
            end do

        end if

        close(input_file_io)

        if (allocated(io_error_message)) then
            write (*,*) io_error_message
            deallocate(io_error_message)
            stop
        end if
    end subroutine read_model_from_file

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

    !> Output the current board to the terminal
    subroutine draw_board(board)
        !> The current state of the board
        integer, dimension(:,:), allocatable, intent(in) :: board

        integer :: row, column, nrow, ncol
        character(:), allocatable :: output

        nrow = size(board, 1)
        ncol = size(board, 2)

        allocate(character(nrow) :: output)

        ! Clear the terminal screen
        call system("clear")

        do row=1, nrow
            output = ""
            do column=1, ncol
                if (board(row, column) == 1) then
                    output = trim(output)//"#"
                else
                    output = trim(output)//"."
                endif
            enddo
            print *, output
        enddo
    end subroutine draw_board

end program game_of_life