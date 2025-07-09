! Q1_FIX_1: Moving these procedures into a separate file allows them to be used within a test file
module game_of_life_mod
    ! allow(C121)
    use mpi
    implicit none

    public

contains

    subroutine exchange_boundaries(board, local_nrow, local_ncol, cart_comm, nbr_north, nbr_south, nbr_east, nbr_west)
        implicit none
        !> The number of rows in the local board
        integer, intent(in) :: local_nrow
        !> The number of columns in the local board
        integer, intent(in) :: local_ncol
        !> The ID of the cartesian communicator containing the ranks
        integer, intent(in) :: cart_comm
        !> The current ranks neighbor to the north
        integer, intent(in) :: nbr_north
        !> The current ranks neighbor to the south
        integer, intent(in) :: nbr_south
        !> The current ranks neighbor to the east
        integer, intent(in) :: nbr_east
        !> The current ranks neighbor to the west
        integer, intent(in) :: nbr_west
        !> The board to be exchanged
        integer, dimension(:,:), intent(inout) :: board

        integer :: ierr, rank, mpi_req

        call MPI_Comm_rank(cart_comm, rank, ierr)

        ! Vertical exchange
        if (nbr_north >= 0) then
            ! Send top row north
            call MPI_ISEND(board(2,2:local_ncol), local_ncol, MPI_INTEGER, nbr_north, 0, cart_comm, mpi_req, ierr)

            ! Receive top row from the north
            CALL MPI_RECV(board(1,2:local_ncol), local_ncol, MPI_INTEGER, nbr_north, 1, cart_comm, MPI_STATUS_IGNORE, ierr)
        endif
        if (nbr_south >= 0) then
            ! Send the bottom row south
            call MPI_ISEND(board(local_nrow+1,2:local_ncol), local_ncol, MPI_INTEGER, nbr_south, 1, cart_comm, mpi_req, ierr)

            ! Receive the bottom row from the south
            CALL MPI_RECV( &
                board(local_nrow+2,2:local_ncol), local_ncol, MPI_INTEGER, nbr_south, 0, cart_comm, MPI_STATUS_IGNORE, ierr)
        endif

        ! Horizontal exchange
        if (nbr_west >= 0) then
            ! Send the left column to the west
            call MPI_ISEND(board(2:local_nrow,2), local_nrow, MPI_INTEGER, nbr_west, 2, cart_comm, mpi_req, ierr)

            ! Receive the left columns from the west
            CALL MPI_RECV(board(2:local_nrow,1), local_nrow, MPI_INTEGER, nbr_west, 3, cart_comm, MPI_STATUS_IGNORE, ierr)
        endif
        if (nbr_east >= 0) then
            ! Send the right column east
            call MPI_ISEND(board(2:local_nrow,local_ncol+1), local_nrow, MPI_INTEGER, nbr_east, 3, cart_comm, mpi_req, ierr)

            ! Receive the right column from the east
            CALL MPI_RECV( &
                board(2:local_nrow,local_ncol+2), local_nrow, MPI_INTEGER, nbr_east, 2, cart_comm, MPI_STATUS_IGNORE, ierr)
        endif
    end subroutine exchange_boundaries

    !> Evolve the board into the state of the next iteration
    subroutine evolve_board(current_board, new_board, nrow, ncol)
        !> The board as it currently is before this iteration
        integer, dimension(:,:), allocatable, intent(in) :: current_board
        !> The board into which the new state will be stored after this iteration
        integer, dimension(:,:), allocatable, intent(inout) :: new_board
        !> The number of row elements over which to evolve the board
        integer, intent(in) :: nrow
        !> The number of column elements over which to evolve the board
        integer, intent(in) :: ncol

        integer :: row, col, sum, num_threads, thread_id

        !$omp parallel do default(none) private(row, col, sum) shared(nrow, ncol, current_board, new_board)
        do row=3, nrow-1
            do col=3, ncol-1

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
        !$omp end parallel do
    end subroutine evolve_board

    !> Check if we have reached steady state, i.e. current and new board match
    subroutine check_for_steady_state(current_board, new_board, steady_state, nrow, ncol)
        !> The board as it currently is before this iteration
        integer, dimension(:,:), intent(in) :: current_board
        !> The board into which the new state has been stored after this iteration
        integer, dimension(:,:), intent(in) :: new_board
        !> Logical to indicate whether current and new board match
        logical, intent(out) :: steady_state
        !> The number of row elements over which to check for steady state
        integer, intent(in) :: nrow
        !> The number of column elements over which to check for steady state
        integer, intent(in) :: ncol

        integer :: row, col

        steady_state = .true.
        !$omp parallel do reduction(.and.: steady_state)
        do row = 2, nrow+1
            do col = 2, ncol+1
                if (current_board(row, col) /= new_board(row, col)) then
                    steady_state = .false.
                end if
            end do
        end do
        !$omp end parallel do
    end subroutine check_for_steady_state

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

    ! Q1_FIX3: Extract the file IO into a module procedure to allow it to be tested.
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
        real :: rnd_num
        character(len=80) :: text_to_discard

        call random_number(rnd_num)
        input_file_io = floor(rnd_num * 1000.0)

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
