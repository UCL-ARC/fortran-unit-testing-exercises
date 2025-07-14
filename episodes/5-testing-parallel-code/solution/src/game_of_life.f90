! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life
    ! allow(C121)
    use mpi
    use game_of_life_mod, only : evolve_board, check_for_steady_state, read_model_from_file, exchange_boundaries, getLocalGridInfo
    implicit none

    !! Board args
    integer, parameter :: max_nx = 50000, max_ny = 50000, max_generations = 100
    integer :: generation_number, local_nx, local_ny, global_nx, global_ny, nx_per_rank, ny_per_rank
    integer, dimension(:,:), allocatable :: global_board, local_current, local_new
    logical :: local_steady = .false., global_steady = .false.

    !! CLI args
    integer :: argl
    character(len=:), allocatable :: cli_arg_temp_store, input_fname

    !! IO args
    character(len=:), allocatable :: io_error_message
    integer :: num_ranks_x, num_ranks_y, x_start, y_start, x_end, y_end

    !! Timing
    real :: start_time, end_time

    !! MPI args
    integer :: ierr, rank, nprocs
    integer :: dims(2), coords(2), cart_comm
    integer :: neighbours(4)
    logical :: periods(2)

    !! MPI args for rank 0 only
    integer :: coords_i(2), neighbours_i(4), y_start_i, x_start_i, local_ny_i, local_nx_i

    !! Misc
    integer :: i, j

    ! MPI Init
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)

    ! Read input on rank 0
    if (rank == 0) then
        if (command_argument_count() == 1) then
            call get_command_argument(1, length=argl)
            allocate(character(argl) :: input_fname)
            call get_command_argument(1, input_fname)
        else
            write(*,'(A)') "Error: Invalid input"
            call get_command_argument(0, length=argl)
            allocate(character(argl) :: cli_arg_temp_store)
            call get_command_argument(0, cli_arg_temp_store)
            write(*,'(A,A,A)') "Usage: ", cli_arg_temp_store, " <input_file_name>"
            deallocate(cli_arg_temp_store)
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
            stop
        end if

        call read_model_from_file(input_fname, max_nx, max_ny, global_board, io_error_message)

        if (allocated(io_error_message)) then
            write (*,*) io_error_message
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
            stop
        end if

        global_ny = size(global_board, 1)
        global_nx = size(global_board, 2)
    end if

    ! Broadcast global dimensions
    call MPI_Bcast(global_nx, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call MPI_Bcast(global_ny, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    ! Create 2D Cartesian topology
    dims = 0
    call MPI_Dims_create(nprocs, 2, dims, ierr)   ! Automatically split into num_ranks_x x num_ranks_y grid
    periods = [ .false., .false. ]
    call MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, .true., cart_comm, ierr)

    num_ranks_y = dims(1)
    num_ranks_x = dims(2)

    ! Shared local domain sizes
    ny_per_rank = global_ny / num_ranks_y
    nx_per_rank = global_nx / num_ranks_x

    call getLocalGridInfo(cart_comm, rank, dims, global_ny, global_nx, ny_per_rank, nx_per_rank, coords, neighbours, y_start, &
                          x_start, local_ny, local_nx)

    if (rank == 0) write(*,*) rank, "Ranks,", num_ranks_x, num_ranks_y, ny_per_rank, nx_per_rank, "grid values,", coords, ",", &
        neighbours, ",", y_start, ",", x_start, ",", local_ny, ",", local_nx

    ! call MPI_FINALIZE(ierr);
    ! stop

    allocate(local_current(local_nx+2, local_ny+2))
    allocate(local_new(local_nx+2, local_ny+2))
    local_current = 0
    local_new = 0

    ! Scatter global board
    if (rank == 0) then
        do i = 1, nprocs - 1
            call getLocalGridInfo(cart_comm, i, dims, global_ny, global_nx, ny_per_rank, nx_per_rank, coords_i, neighbours_i, &
                y_start_i, x_start_i, local_ny_i, local_nx_i)

            call MPI_Send(global_board(x_start_i:x_start_i+local_nx_i-1, y_start_i:y_start_i+local_ny_i-1), &
                local_nx_i*local_ny_i, MPI_INTEGER, i, 0, MPI_COMM_WORLD, ierr)
        end do

        local_current(2:local_nx+1, 2:local_ny+1) = global_board(1:local_nx, 1:local_ny)
    else
        call MPI_Recv(local_current(2:local_nx+1, 2:local_ny+1), local_nx*local_ny, MPI_INTEGER, &
                      0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
    endif

    call MPI_Barrier(cart_comm, ierr)
    start_time = MPI_Wtime()

    generation_number = 0
    local_steady = .false.

    do while (.not. local_steady .and. generation_number < max_generations)
        ! Exchange ghost cells with neighbors
        call exchange_boundaries(local_current, nx_per_rank, ny_per_rank, cart_comm, neighbours)

        ! Evolution
        call evolve_board(local_current, local_new, local_nx, local_ny)
        call check_for_steady_state(local_current, local_new, local_steady, local_nx, local_ny)

        call MPI_Allreduce(local_steady, global_steady, 1, MPI_LOGICAL, MPI_LAND, cart_comm, ierr)
        local_steady = global_steady

        local_current(2:local_nx+1, 2:local_ny+1) = local_new(2:local_nx+1, 2:local_ny+1)

        generation_number = generation_number + 1
    end do

    end_time = MPI_Wtime()

    if (rank == 0) then
        if (local_steady) then
            print *, "Reached steady state after ", generation_number, " generations. Time: ", end_time - start_time
        else
            print *, "Did NOT reach steady state after ", generation_number, " generations. Time: ", end_time - start_time
        end if
    end if

    call MPI_Finalize(ierr)
end program game_of_life
