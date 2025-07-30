! FIX_1: Moving these procedures into a separate file allows them to be used within a test file
module game_of_life_mod
    implicit none

    public

contains

    ! FIX2: Pass parameters into procedures instead of relying on global state to isolate tests from one another.
    !> Evolve the board into the state of the next iteration
    subroutine evolve_board(current_board, new_board)
        !> The board as it currently is before this iteration
        integer, dimension(:,:), allocatable, intent(in) :: current_board
        !> The board into which the new state will be stored after this iteration
        integer, dimension(:,:), allocatable, intent(inout) :: new_board

        integer :: row, col, num_rows, num_cols, sum

        num_rows = size(current_board, 1)
        num_cols = size(current_board, 2)

        do row=2, num_rows-1
            do col=2, num_cols-1
                sum = 0
                sum = current_board(row, col-1)   &
                    + current_board(row+1, col-1) &
                    + current_board(row+1, col)   &
                    + current_board(row+1, col+1) &
                    + current_board(row, col+1)   &
                    + current_board(row-1, col+1) &
                    + current_board(row-1, col)   &
                    + current_board(row-1, col-1)
                if(current_board(row,col)==1 .and. sum<=1) then
                    new_board(row,col) = 0
                elseif(current_board(row,col)==1 .and. sum<=3) then
                    new_board(row,col) = 1
                elseif(current_board(row,col)==1 .and. sum>=4)then
                    new_board(row,col) = 0
                elseif(current_board(row,col)==0 .and. sum==3)then
                    new_board(row,col) = 1
                endif
            enddo
        enddo
    end subroutine evolve_board

    ! FIX2: Pass parameters into procedures instead of relying on global state to isolate tests from one another.
    !> Check if we have reached steady state, i.e. current and new board match
    subroutine check_for_steady_state(current_board, new_board, steady_state)
        !> The board as it currently is before this iteration
        integer, dimension(:,:), allocatable, intent(in) :: current_board
        !> The board into which the new state has been stored after this iteration
        integer, dimension(:,:), allocatable, intent(in) :: new_board
        !> Logical to indicate whether current and new board match
        logical, intent(out) :: steady_state

        integer :: row, col, num_rows, num_cols

        num_rows = size(current_board, 1)
        num_cols = size(current_board, 2)

        do row=1, num_rows
            do col=1, num_cols
                if (.not. current_board(row, col) == new_board(row, col)) then
                    steady_state = .false.
                    return
                end if
            end do
        end do

        steady_state = .true.
    end subroutine check_for_steady_state

    ! FIX2: Pass parameters into procedures instead of relying on global state to isolate tests from one another.
    !> Output the current board to the terminal
    subroutine draw_board(current_board)
        !> The board as it currently is for this iteration
        integer, dimension(:,:), allocatable, intent(in) :: current_board

        integer :: row, col, num_rows, num_cols
        character(len=:), allocatable :: output

        call system("clear")

        num_rows = size(current_board, 1)
        num_cols = size(current_board, 2)

        allocate(character(num_rows) :: output)

        do row=1, num_rows
            output = ""
            do col=1, num_cols
                if (current_board(row,col) == 1) then
                    output = trim(output)//"#"
                else
                    output = trim(output)//"."
                endif
            enddo
            print *, output
        enddo

        deallocate(output)
    end subroutine draw_board

    ! FIX3: Extract the file IO into a module procedure to allow it to be tested.
    !> Populate the a board from the provided file
    subroutine read_model_from_file(input_fname, max_nrow, max_ncol, board, io_error_message)
        !> The name of the file to read in the board
        character(len=:), allocatable, intent(in) :: input_fname
        !> The maximum allowed number of rows
        integer, intent(in) :: max_nrow
        !> The maximum allowed number of columns
        integer, intent(in) :: max_ncol
        !> The board to be populated
        integer, dimension(:,:), allocatable, intent(out) :: board
        !> A flag to indicate if reading the file was successful
        character(len=:), allocatable,intent(out) :: io_error_message

        ! Board definition args
        integer :: nrow, ncol, row

        ! File IO args
        integer :: input_file_io, iostat
        character(len=80) :: text_to_discard

        input_file_io = 1111

        ! Open input file
        open(unit=input_file_io,   &
            file=input_fname, &
            status='old',  &
            IOSTAT=iostat)

        if( iostat == 0) then
            ! Read in board from file
            read(input_file_io,'(a)') text_to_discard ! Skip first line
            read(input_file_io,*) nrow, ncol

            ! Verify the date_time_values read from the file
            if (nrow < 1 .or. nrow > max_nrow) then
                allocate(character(100) :: io_error_message)
                write (io_error_message,'(a,i6,a,i6)') "nrow must be a positive integer less than ", max_nrow, " found ", nrow
            elseif (ncol < 1 .or. ncol > max_ncol) then
                allocate(character(100) :: io_error_message)
                write (io_error_message,'(a,i6,a,i6)') "ncol must be a positive integer less than ", max_ncol, " found ", ncol
            end if
        else
            allocate(character(100) :: io_error_message)
            write(io_error_message,'(a)') ' *** Error when opening '//input_fname
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
    end subroutine read_model_from_file

end module game_of_life_mod
