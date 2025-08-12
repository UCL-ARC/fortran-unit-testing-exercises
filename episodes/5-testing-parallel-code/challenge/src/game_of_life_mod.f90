module game_of_life_mod
    ! allow(C121)
    use mpi
    implicit none

    public

    ! Make UP, DOWN, LEFT, RIGHT parameters to define neighbour directions
    integer, parameter :: DOWN=1, LEFT=2, UP=3, RIGHT=4

    !> Type to represent the domain decomposition for parallel processing
    type :: DomainDecomposition
        !> The ID of the communicator for this domain
        integer :: communicator
        !> The dimensions of the MPI communicators Cartesian grid
        integer :: dims(2)
        !> The ranks of the neighbouring ranks - [down, left, up, right]
        integer :: neighbours(4)
    end type DomainDecomposition

contains

    !> Subroutine to find the steady state of the game of life
    subroutine find_steady_state(global_steady, generation_number, global_board, global_ny, global_nx, base_mpi_communicator, &
                                 nprocs)
        !> Logical flag indicating whether the global board has reached a steady state
        logical, intent(out) :: global_steady
        !> The number of generations required to reach the steady state
        integer, intent(out) :: generation_number
        !> The global board representing the current state of the game
        integer, dimension(:,:), allocatable, intent(inout) :: global_board
        !> The number of rows in the global board
        integer, intent(in) :: global_ny
        !> The number of columns in the global board
        integer, intent(in) :: global_nx
        !> The base MPI communicator for parallel processing
        integer, intent(in) :: base_mpi_communicator
        !> The total number of processes in the MPI communicator
        integer, intent(in) :: nprocs

        !! Board args
        integer, parameter :: max_generations = 100
        integer :: local_nx, local_ny, nx_per_rank, ny_per_rank
        integer, dimension(:,:), allocatable :: local_current, local_new
        logical :: local_steady
        integer :: x_start, y_start, x_end, y_end

        !! MPI args
        integer :: ierr, rank, mpi_req
        integer :: coords(2)
        logical :: periods(2)
        type(DomainDecomposition) :: domainDecomp

        !! MPI args for rank 0 only
        integer :: coords_i(2), neighbours_i(4), y_start_i, x_start_i, local_ny_i, local_nx_i

        !! Timing
        real :: start_time, end_time

        !! Misc
        integer :: i, j

        local_steady = .false.
        global_steady = .false.

        ! Create 2D Cartesian topology
        domainDecomp%dims = 0
        call MPI_Dims_create(nprocs, 2, domainDecomp%dims, ierr)   ! Automatically split into num_ranks_x x num_ranks_y grid
        periods = [ .false., .false. ]
        call MPI_Cart_create(base_mpi_communicator, 2, domainDecomp%dims, periods, .true., domainDecomp%communicator, ierr)
        call MPI_Comm_rank(domainDecomp%communicator, rank, ierr)

        call get_local_grid_info(domainDecomp, rank, global_ny, global_nx, ny_per_rank, nx_per_rank, coords, &
            y_start, x_start, local_ny, local_nx)

        allocate(local_current(local_nx+2, local_ny+2))
        allocate(local_new(local_nx+2, local_ny+2))
        local_current = 0
        local_new = 0

        ! Scatter global board
        if (rank == 0) then
            do i = 1, nprocs - 1
                call MPI_RECV(y_start_i, 1, MPI_INTEGER, i, i*100, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
                call MPI_RECV(x_start_i, 1, MPI_INTEGER, i, i*100 + 1, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
                call MPI_RECV(local_ny_i, 1, MPI_INTEGER, i, i*100 + 2, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
                call MPI_RECV(local_nx_i, 1, MPI_INTEGER, i, i*100 + 3, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)

                call MPI_Send(global_board(x_start_i:x_start_i+local_nx_i-1, y_start_i:y_start_i+local_ny_i-1), &
                    local_nx_i*local_ny_i, MPI_INTEGER, i, i*100 + 4, domainDecomp%communicator, ierr)
            end do

            local_current(2:local_nx+1, 2:local_ny+1) = global_board(1:local_nx, 1:local_ny)
        else
            call MPI_ISEND(y_start, 1, MPI_INTEGER, 0, rank*100, domainDecomp%communicator, mpi_req, ierr)
            call MPI_ISEND(x_start, 1, MPI_INTEGER, 0, rank*100 + 1, domainDecomp%communicator, mpi_req, ierr)
            call MPI_ISEND(local_ny, 1, MPI_INTEGER, 0, rank*100 + 2, domainDecomp%communicator, mpi_req, ierr)
            call MPI_ISEND(local_nx, 1, MPI_INTEGER, 0, rank*100 + 3, domainDecomp%communicator, mpi_req, ierr)

            call MPI_Recv(local_current(2:local_nx+1, 2:local_ny+1), local_nx*local_ny, MPI_INTEGER, &
                        0, rank*100 + 4, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif

        local_new = local_current

        call MPI_Barrier(domainDecomp%communicator, ierr)

        generation_number = 0
        local_steady = .false.

        do while (.not. local_steady .and. generation_number < max_generations)
            ! Exchange ghost cells with neighbors
            call exchange_boundaries(local_current, local_ny, local_nx, domainDecomp)

            ! Evolution
            call evolve_board(local_current, local_new)
            call check_for_steady_state(local_current, local_new, local_steady)

            call MPI_Allreduce(local_steady, global_steady, 1, MPI_LOGICAL, MPI_LAND, domainDecomp%communicator, ierr)
            local_steady = global_steady

            local_current = local_new

            generation_number = generation_number + 1
        end do
    end subroutine find_steady_state

    !> Subroutine to get local grid information for a rank
    subroutine get_local_grid_info(domainDecomp, rank, global_ny, global_nx, ny_per_rank, nx_per_rank, coords, &
                                y_start, x_start, local_ny, local_nx)
        !> The MPI communication domain decomposition object
        type(DomainDecomposition), intent(inout) :: domainDecomp
        !> The rank of the current process
        integer, intent(in) :: rank
        !> The number of rows in the global board
        integer, intent(in) :: global_ny
        !> The number of columns in the global board
        integer, intent(in) :: global_nx
        !> The number of rows per rank
        integer, intent(out) :: ny_per_rank
        !> The number of columns per rank
        integer, intent(out) :: nx_per_rank
        !> The coordinates of the current rank in the Cartesian grid
        integer, intent(out) :: coords(2)
        !> The starting row index for the local grid
        integer, intent(out) :: y_start
        !> The starting column index for the local grid
        integer, intent(out) :: x_start
        !> The number of rows in the local grid
        integer, intent(out) :: local_ny
        !> The number of columns in the local grid
        integer, intent(out) :: local_nx

        integer :: mpierr, num_ranks_y, num_ranks_x

        call MPI_Cart_coords(domainDecomp%communicator, rank, 2, coords, mpierr)
        call MPI_Cart_shift(domainDecomp%communicator, 0, 1, domainDecomp%neighbours(DOWN), domainDecomp%neighbours(UP), mpierr)
        call MPI_Cart_shift(domainDecomp%communicator, 1, 1, domainDecomp%neighbours(LEFT), domainDecomp%neighbours(RIGHT), mpierr)

        ny_per_rank = global_ny / domainDecomp%dims(1)
        nx_per_rank = global_nx / domainDecomp%dims(2)

        y_start = coords(1)*ny_per_rank + 1
        x_start = coords(2)*nx_per_rank + 1

        num_ranks_y = domainDecomp%dims(1)
        num_ranks_x = domainDecomp%dims(2)

        ! Add remainders if on the top or right of the grid
        local_ny = ny_per_rank
        local_nx = nx_per_rank
        if (domainDecomp%neighbours(UP) == MPI_PROC_NULL) local_ny = local_ny + modulo(global_ny, ny_per_rank)
        if (domainDecomp%neighbours(RIGHT) == MPI_PROC_NULL) local_nx = local_nx + modulo(global_nx, nx_per_rank)
    end subroutine get_local_grid_info

    !> Subroutine to exchange boundaries between neighboring ranks
    subroutine exchange_boundaries(board, local_ny, local_nx, domainDecomp)
        !> The board to be exchanged
        integer, dimension(:,:), intent(inout) :: board
        !> The number of rows in the local board
        integer, intent(in) :: local_ny
        !> The number of columns in the local board
        integer, intent(in) :: local_nx
        !> The domain decomposition object
        type(DomainDecomposition), intent(in) :: domainDecomp

        integer :: ierr, rank, mpi_req

        ! Vertical exchange
        if (domainDecomp%neighbours(UP) >= 0) then
            ! Send top halo up
            call MPI_ISEND(board(:,local_ny+1), local_nx+2, MPI_INTEGER, domainDecomp%neighbours(UP), 0, &
                domainDecomp%communicator, mpi_req, ierr)

            ! Receive top halo from the above rank
            CALL MPI_RECV(board(:,local_ny+2), local_nx+2, MPI_INTEGER, domainDecomp%neighbours(UP), 1, &
                domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif
        if (domainDecomp%neighbours(DOWN) >= 0) then
            ! Send the bottom halo down
            call MPI_ISEND(board(:,2), local_nx+2, MPI_INTEGER, domainDecomp%neighbours(DOWN), 1, &
                domainDecomp%communicator, mpi_req, ierr)

            ! Receive the bottom halo from the below rank
            CALL MPI_RECV(board(:,1), local_nx+2, MPI_INTEGER, domainDecomp%neighbours(DOWN), 0, &
                domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif

        ! Horizontal exchange
        if (domainDecomp%neighbours(LEFT) >= 0) then
            ! Send the left halo left
            call MPI_ISEND(board(2,:), local_ny+2, MPI_INTEGER, domainDecomp%neighbours(LEFT), 2, &
                domainDecomp%communicator, mpi_req, ierr)

            ! Receive the left halo from the left
            CALL MPI_RECV(board(1,:), local_ny+2, MPI_INTEGER, domainDecomp%neighbours(LEFT), 3, &
                domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif
        if (domainDecomp%neighbours(RIGHT) >= 0) then
            ! Send the right halo right
            call MPI_ISEND(board(local_nx+1,:), local_ny+2, MPI_INTEGER, domainDecomp%neighbours(RIGHT), 3, &
                domainDecomp%communicator, mpi_req, ierr)

            ! Receive the right halo from the right
            CALL MPI_RECV(board(local_nx+2,:), local_ny+2, MPI_INTEGER, domainDecomp%neighbours(RIGHT), 2, &
                domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif
    end subroutine exchange_boundaries

    !> Evolve the board into the state of the next iteration
    subroutine evolve_board(current_board, new_board)
        !> The board as it currently is before this iteration
        integer, dimension(:,:), allocatable, intent(in) :: current_board
        !> The board into which the new state will be stored after this iteration
        integer, dimension(:,:), allocatable, intent(inout) :: new_board

        integer :: nx, ny, x, y, sum

        nx = size(current_board, 1)
        ny = size(current_board, 2)

        !$omp parallel do default(none) private(x, y, sum) shared(nx, ny, current_board, new_board)
        do y = 2, ny-1
            do x = 2, nx-1
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
    subroutine check_for_steady_state(current_board, new_board, steady_state)
        !> The board as it currently is before this iteration
        integer, dimension(:,:), intent(in) :: current_board
        !> The board into which the new state has been stored after this iteration
        integer, dimension(:,:), intent(in) :: new_board
        !> Logical to indicate whether current and new board match
        logical, intent(out) :: steady_state

        integer :: nx, ny, x, y

        nx = size(current_board, 1)
        ny = size(current_board, 2)

        steady_state = .true.
        !$omp parallel do reduction(.and.: steady_state)
        do y = 2, ny-1
            do x = 2, nx-1
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

    !> Populate the a board from the provided file
    subroutine read_model_from_file(input_fname, max_nx, max_ny, board, io_error_message)
        !> The name of the file to read in the board
        character(len=:), allocatable, intent(in) :: input_fname
        !> The maximum allowed number of rows
        integer, intent(in) :: max_nx
        !> The maximum allowed number of columns
        integer, intent(in) :: max_ny
        !> The board to be populated
        integer, dimension(:,:), allocatable, intent(out) :: board
        !> A flag to indicate if reading the file was successful
        character(len=:), allocatable, intent(out) :: io_error_message

        ! Board definition args
        integer :: nx, ny, x

        ! File IO args
        integer :: input_file_io, iostat
        real :: rnd_num
        character(len=80) :: text_to_discard

        call random_number(rnd_num)
        input_file_io = floor(rnd_num * 1000.0)

        ! Open input file
        open(unit=input_file_io, file=input_fname, status='old', IOSTAT=iostat)

        if (iostat == 0) then
            read(input_file_io,'(a)') text_to_discard ! Skip first line
            read(input_file_io,*) nx, ny

            ! Verify the number of rows read from the file
            if (nx < 1 .or. nx > max_nx) then
                allocate(character(100) :: io_error_message)
                write(io_error_message,'(a,i6,a,i6)') "nx must be a positive integer less than ", max_nx, " found ", nx
            elseif (ny < 1 .or. ny > max_ny) then
                allocate(character(100) :: io_error_message)
                write(io_error_message,'(a,i6,a,i6)') "ny must be a positive integer less than ", max_ny, " found ", ny
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
