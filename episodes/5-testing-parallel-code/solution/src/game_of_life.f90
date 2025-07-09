! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life
    ! allow(C121)
    use mpi
    use game_of_life_mod, only : evolve_board, check_for_steady_state, read_model_from_file, exchange_boundaries
    implicit none

    !! Board args
    integer, parameter :: max_nrow = 50000, max_ncol = 50000, max_generations = 100
    integer :: generation_number, local_nrow, local_ncol, global_nrow, global_ncol
    integer, dimension(:,:), allocatable :: global_board, local_current, local_new
    logical :: local_steady = .false., global_steady = .false.

    !! CLI args
    integer :: argl
    character(len=:), allocatable :: cli_arg_temp_store, input_fname

    !! IO args
    character(len=:), allocatable :: io_error_message
    integer :: num_ranks_x, num_ranks_y, row_start, col_start, row_end, col_end

    !! Timing
    real :: start_time, end_time

    !! MPI args
    integer :: ierr, rank, nprocs
    integer :: dims(2), coords(2), cart_comm
    integer :: nbr_north, nbr_south, nbr_east, nbr_west
    logical :: periods(2)

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

        call read_model_from_file(input_fname, max_nrow, max_ncol, global_board, io_error_message)

        if (allocated(io_error_message)) then
            write (*,*) io_error_message
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
            stop
        end if

        global_nrow = size(global_board, 1)
        global_ncol = size(global_board, 2)
    end if

    ! Broadcast global dimensions
    call MPI_Bcast(global_nrow, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call MPI_Bcast(global_ncol, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    ! Create 2D Cartesian topology
    dims = 0
    call MPI_Dims_create(nprocs, 2, dims, ierr)   ! Automatically split into num_ranks_x x num_ranks_y grid
    periods = [ .false., .false. ]
    call MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, .true., cart_comm, ierr)
    call MPI_Cart_coords(cart_comm, rank, 2, coords, ierr)
    call MPI_Cart_shift(cart_comm, 0, 1, nbr_north, nbr_south, ierr)
    call MPI_Cart_shift(cart_comm, 1, 1, nbr_west, nbr_east, ierr)

    num_ranks_x = dims(1)
    num_ranks_y = dims(2)

    ! Local domain sizes
    local_nrow = global_nrow / num_ranks_x
    local_ncol = global_ncol / num_ranks_y
    row_start = coords(1)*local_nrow + 1
    col_start = coords(2)*local_ncol + 1

    allocate(local_current(local_nrow+2, local_ncol+2))
    allocate(local_new(local_nrow+2, local_ncol+2))
    local_current = 0
    local_new = 0

    ! Scatter global board
    if (rank == 0) then
        do i = 1, nprocs - 1
            call MPI_Cart_coords(cart_comm, i, 2, coords, ierr)
            row_start = coords(1)*local_nrow + 1
            col_start = coords(2)*local_ncol + 1
            call MPI_Send(global_board(row_start:row_start+local_nrow-1, col_start:col_start+local_ncol-1), &
                          local_nrow*local_ncol, MPI_INTEGER, i, 0, MPI_COMM_WORLD, ierr)
        end do

        local_current(1:local_nrow, 1:local_ncol) = global_board(1:local_nrow, 1:local_ncol)
    else
        call MPI_Recv(local_current(2:local_nrow+1, 2:local_ncol+1), local_nrow*local_ncol, MPI_INTEGER, &
                      0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
    endif

    ! do j = 0, nprocs - 1
    !     if (j == rank) then
    !         do i = 1, local_nrow
    !             write(100+rank,*) local_current(i,:)
    !         end do
    !     end if
    ! end do

    call MPI_Barrier(cart_comm, ierr)
    start_time = MPI_Wtime()

    generation_number = 0
    local_steady = .false.

    do while (.not. local_steady .and. generation_number < max_generations)
        ! Exchange ghost cells with neighbors
        call exchange_boundaries(local_current, local_nrow, local_ncol, cart_comm, nbr_north, nbr_south, nbr_east, nbr_west)

        ! call MPI_Finalize(ierr)
        ! stop

        ! Evolution
        call evolve_board(local_current, local_new, local_nrow, local_ncol)
        call check_for_steady_state(local_current, local_new, local_steady, local_nrow, local_ncol)

        call MPI_Allreduce(local_steady, global_steady, 1, MPI_LOGICAL, MPI_LAND, cart_comm, ierr)
        local_steady = global_steady

        local_current(2:local_nrow, 2:local_ncol) = local_new(2:local_nrow, 2:local_ncol)

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
