! Q1_FIX_1: Moving these procedures into a separate file allows them to be used within a test file
module game_of_life_mod
    ! allow(C121)
    use mpi
    implicit none

    public

contains

    subroutine get_local_grid_info(communicator, rank, dims, global_ny, global_nx, ny_per_rank, nx_per_rank, coords, neighbours, &
                                y_start, x_start, local_ny, local_nx)
        integer, intent(in) :: communicator
        integer, intent(in) :: rank, dims(2), global_ny, global_nx
        integer, intent(out) :: ny_per_rank, nx_per_rank, coords(2), neighbours(4), y_start, x_start, local_ny, local_nx

        integer :: mpierr, num_ranks_y, num_ranks_x

        call MPI_Cart_coords(communicator, rank, 2, coords, mpierr)
        call MPI_Cart_shift(communicator, 0, 1, neighbours(1), neighbours(3), mpierr)
        call MPI_Cart_shift(communicator, 1, 1, neighbours(2), neighbours(4), mpierr)

        ny_per_rank = global_ny / dims(1)
        nx_per_rank = global_nx / dims(2)

        y_start = coords(1)*ny_per_rank + 1
        x_start = coords(2)*nx_per_rank + 1

        num_ranks_y = dims(1)
        num_ranks_x = dims(2)

        ! Add remainders if on the top or right of the grid
        local_ny = ny_per_rank
        local_nx = nx_per_rank
        if (neighbours(3) == MPI_PROC_NULL) local_ny = local_ny + modulo(global_ny, ny_per_rank)
        if (neighbours(4) == MPI_PROC_NULL) local_nx = local_nx + modulo(global_nx, nx_per_rank)
    end subroutine get_local_grid_info

    subroutine exchange_boundaries(board, local_nx, local_ny, cart_comm, neighbours)
        implicit none
        !> The number of xs in the local board
        integer, intent(in) :: local_nx
        !> The number of yumns in the local board
        integer, intent(in) :: local_ny
        !> The ID of the cartesian communicator containing the ranks
        integer, intent(in) :: cart_comm
        !> The current ranks neighbor to the north
        integer, intent(in) :: neighbours(4)
        !> The board to be exchanged
        integer, dimension(:,:), intent(inout) :: board

        integer :: ierr, rank, mpi_req

        call MPI_Comm_rank(cart_comm, rank, ierr)

        ! Vertical exchange
        if (neighbours(3) >= 0) then
            ! Send top x north
            call MPI_ISEND(board(2:local_nx+1,local_ny+1), local_nx, MPI_INTEGER, neighbours(3), 0, cart_comm, mpi_req, ierr)

            ! Receive top x from the north
            CALL MPI_RECV( &
                board(2:local_nx+1,local_ny+2), local_nx, MPI_INTEGER, neighbours(3), 1, cart_comm, MPI_STATUS_IGNORE, ierr)
        endif
        if (neighbours(1) >= 0) then
            ! Send the bottom x south
            call MPI_ISEND(board(2:local_nx+1,2), local_nx, MPI_INTEGER, neighbours(1), 1, cart_comm, mpi_req, ierr)

            ! Receive the bottom x from the south
            CALL MPI_RECV( &
                board(2:local_nx+1,1), local_nx, MPI_INTEGER, neighbours(1), 0, cart_comm, MPI_STATUS_IGNORE, ierr)
        endif

        ! Horizontal exchange
        if (neighbours(2) >= 0) then
            ! Send the left to the west
            call MPI_ISEND(board(2,2:local_ny+1), local_ny, MPI_INTEGER, neighbours(2), 2, cart_comm, mpi_req, ierr)

            ! Receive the left from the west
            CALL MPI_RECV(board(1,2:local_ny+1), local_ny, MPI_INTEGER, neighbours(2), 3, cart_comm, MPI_STATUS_IGNORE, ierr)
        endif
        if (neighbours(4) >= 0) then
            ! Send the right east
            call MPI_ISEND(board(local_nx+1,2:local_ny+1), local_ny, MPI_INTEGER, neighbours(4), 3, cart_comm, mpi_req, ierr)

            ! Receive the right from the east
            CALL MPI_RECV( &
                board(local_nx+2,2:local_ny+1), local_ny, MPI_INTEGER, neighbours(4), 2, cart_comm, MPI_STATUS_IGNORE, ierr)
        endif
    end subroutine exchange_boundaries

    !> Evolve the board into the state of the next iteration
    subroutine evolve_board(current_board, new_board, nx, ny)
        !> The board as it currently is before this iteration
        integer, dimension(:,:), allocatable, intent(in) :: current_board
        !> The board into which the new state will be stored after this iteration
        integer, dimension(:,:), allocatable, intent(inout) :: new_board
        !> The number of x elements over which to evolve the board
        integer, intent(in) :: nx
        !> The number of yumn elements over which to evolve the board
        integer, intent(in) :: ny

        integer :: x, y, sum, num_threads, thread_id

        !$omp parallel do default(none) private(x, y, sum) shared(nx, ny, current_board, new_board)
        do y=2, ny+1
            do x=2, nx+1

                sum = 0
                sum = current_board(x, y-1)   &
                    + current_board(x+1, y-1) &
                    + current_board(x+1, y)   &
                    + current_board(x+1, y+1) &
                    + current_board(x, y+1)   &
                    + current_board(x-1, y+1) &
                    + current_board(x-1, y)   &
                    + current_board(x-1, y-1)
                if(current_board(x,y)==1 .and. sum<=1) then
                    new_board(x,y) = 0
                elseif(current_board(x,y)==1 .and. sum<=3) then
                    new_board(x,y) = 1
                elseif(current_board(x,y)==1 .and. sum>=4)then
                    new_board(x,y) = 0
                elseif(current_board(x,y)==0 .and. sum==3)then
                    new_board(x,y) = 1
                endif
            enddo
        enddo
        !$omp end parallel do
    end subroutine evolve_board

    !> Check if we have reached steady state, i.e. current and new board match
    subroutine check_for_steady_state(current_board, new_board, steady_state, nx, ny)
        !> The board as it currently is before this iteration
        integer, dimension(:,:), intent(in) :: current_board
        !> The board into which the new state has been stored after this iteration
        integer, dimension(:,:), intent(in) :: new_board
        !> Logical to indicate whether current and new board match
        logical, intent(out) :: steady_state
        !> The number of x elements over which to check for steady state
        integer, intent(in) :: nx
        !> The number of yumn elements over which to check for steady state
        integer, intent(in) :: ny

        integer :: x, y

        steady_state = .true.
        !$omp parallel do reduction(.and.: steady_state)
        do x = 2, nx+1
        do y = 2, ny+1
                if (current_board(x, y) /= new_board(x, y)) then
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

        integer :: x, y, num_xs, num_ys
        character(len=:), allocatable :: output

        call system("clear")

        num_xs = size(current_board, 1)
        num_ys = size(current_board, 2)

        allocate(character(num_xs) :: output)

        do x=1, num_xs
            output = ""
            do y=1, num_ys
                if (current_board(x,y) == 1) then
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
    subroutine read_model_from_file(input_fname, max_nx, max_ny, board, io_error_message)
        !> The name of the file to read in the board
        character(len=:), allocatable, intent(in) :: input_fname
        !> The maximum allowed number of xs
        integer, intent(in) :: max_nx
        !> The maximum allowed number of yumns
        integer, intent(in) :: max_ny
        !> The board to be populated
        integer, dimension(:,:), allocatable, intent(out) :: board
        !> A flag to indicate if reading the file was successful
        character(len=:), allocatable,intent(out) :: io_error_message

        ! Board definition args
        integer :: nx, ny, x

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
            read(input_file_io,*) nx, ny

            ! Verify the date_time_values read from the file
            if (nx < 1 .or. nx > max_nx) then
                allocate(character(100) :: io_error_message)
                write (io_error_message,'(a,i6,a,i6)') "nx must be a positive integer less than ", max_nx, " found ", nx
            elseif (ny < 1 .or. ny > max_ny) then
                allocate(character(100) :: io_error_message)
                write (io_error_message,'(a,i6,a,i6)') "ny must be a positive integer less than ", max_ny, " found ", ny
            end if
        else
            allocate(character(100) :: io_error_message)
            write(io_error_message,'(a)') ' *** Error when opening '//input_fname
        endif

        if (.not. allocated(io_error_message)) then

            allocate(board(nx, ny))

            read(input_file_io,'(a)') text_to_discard ! Skip next line
            ! Populate the boards starting state
            do x = 1, nx
                read(input_file_io,*) board(x, :)
            end do

        end if

        close(input_file_io)
    end subroutine read_model_from_file

end module game_of_life_mod
