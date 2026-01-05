module comms
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

    !> Subroutine to exchange boundaries between neighboring ranks
    subroutine exchange_boundaries(board, local_ncols, local_nrows, domainDecomp)
        !> The board to be exchanged
        integer, dimension(:,:), intent(inout) :: board
        !> The number of rows in the local board
        integer, intent(in) :: local_ncols
        !> The number of columns in the local board
        integer, intent(in) :: local_nrows
        !> The domain decomposition object
        type(DomainDecomposition), intent(in) :: domainDecomp

        integer :: ierr, rank, mpi_req

        ! Vertical exchange
        if (domainDecomp%neighbours(UP) >= 0) then
            ! Send top halo up
            call MPI_ISEND(board(:,local_ncols+1), local_nrows+2, MPI_INTEGER, domainDecomp%neighbours(UP), 0, &
                domainDecomp%communicator, mpi_req, ierr)

            ! Receive top halo from the above rank
            CALL MPI_RECV(board(:,local_ncols+2), local_nrows+2, MPI_INTEGER, domainDecomp%neighbours(UP), 1, &
                domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif
        if (domainDecomp%neighbours(DOWN) >= 0) then
            ! Send the bottom halo down
            call MPI_ISEND(board(:,2), local_nrows+2, MPI_INTEGER, domainDecomp%neighbours(DOWN), 1, &
                domainDecomp%communicator, mpi_req, ierr)

            ! Receive the bottom halo from the below rank
            CALL MPI_RECV(board(:,1), local_nrows+2, MPI_INTEGER, domainDecomp%neighbours(DOWN), 0, &
                domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif

        ! Horizontal exchange
        if (domainDecomp%neighbours(LEFT) >= 0) then
            ! Send the left halo left
            call MPI_ISEND(board(2,:), local_ncols+2, MPI_INTEGER, domainDecomp%neighbours(LEFT), 2, &
                domainDecomp%communicator, mpi_req, ierr)

            ! Receive the left halo from the left
            CALL MPI_RECV(board(1,:), local_ncols+2, MPI_INTEGER, domainDecomp%neighbours(LEFT), 3, &
                domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif
        if (domainDecomp%neighbours(RIGHT) >= 0) then
            ! Send the right halo right
            call MPI_ISEND(board(local_nrows+1,:), local_ncols+2, MPI_INTEGER, domainDecomp%neighbours(RIGHT), 3, &
                domainDecomp%communicator, mpi_req, ierr)

            ! Receive the right halo from the right
            CALL MPI_RECV(board(local_nrows+2,:), local_ncols+2, MPI_INTEGER, domainDecomp%neighbours(RIGHT), 2, &
                domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif
    end subroutine exchange_boundaries

    !> Subroutine to get local grid information for a rank
    subroutine get_local_grid_info(domainDecomp, rank, global_nrows, global_ncols, nrows_per_rank, ncols_per_rank, coords, &
                                row_start, col_start, local_nrows, local_ncols)
        !> The MPI communication domain decomposition object
        type(DomainDecomposition), intent(inout) :: domainDecomp
        !> The rank of the current process
        integer, intent(in) :: rank
        !> The number of columns in the global board
        integer, intent(in) :: global_nrows
        !> The number of rows in the global board
        integer, intent(in) :: global_ncols
        !> The number of columns per rank
        integer, intent(out) :: nrows_per_rank
        !> The number of rows per rank
        integer, intent(out) :: ncols_per_rank
        !> The coordinates of the current rank in the Cartesian grid
        integer, intent(out) :: coords(2)
        !> The starting column index for the local grid
        integer, intent(out) :: row_start
        !> The starting row index for the local grid
        integer, intent(out) :: col_start
        !> The number of columns in the local grid
        integer, intent(out) :: local_nrows
        !> The number of rows in the local grid
        integer, intent(out) :: local_ncols

        integer :: mpierr, num_ranks_col, num_ranks_row

        call MPI_Cart_coords(domainDecomp%communicator, rank, 2, coords, mpierr)
        call MPI_Cart_shift(domainDecomp%communicator, 0, 1, domainDecomp%neighbours(DOWN), domainDecomp%neighbours(UP), mpierr)
        call MPI_Cart_shift(domainDecomp%communicator, 1, 1, domainDecomp%neighbours(LEFT), domainDecomp%neighbours(RIGHT), mpierr)

        ncols_per_rank = global_ncols / domainDecomp%dims(1)
        nrows_per_rank = global_nrows / domainDecomp%dims(2)

        col_start = coords(1)*ncols_per_rank + 1
        row_start = coords(2)*nrows_per_rank + 1

        num_ranks_col = domainDecomp%dims(1)
        num_ranks_row = domainDecomp%dims(2)

        ! Add remainders if on the top or right of the grid
        local_ncols = ncols_per_rank
        local_nrows = nrows_per_rank
        if (domainDecomp%neighbours(UP) == MPI_PROC_NULL) local_ncols = local_ncols + modulo(global_ncols, ncols_per_rank)
        if (domainDecomp%neighbours(RIGHT) == MPI_PROC_NULL) local_nrows = local_nrows + modulo(global_nrows, nrows_per_rank)
    end subroutine get_local_grid_info

end module comms
