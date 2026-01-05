module game_of_life
    ! allow(C121)
    use mpi
    use comms, only : DomainDecomposition, DOWN, UP, LEFT, RIGHT, exchange_boundaries, get_local_grid_info
    use animation, only : draw_board
    implicit none
    public

contains

    !> Subroutine to find the steady state of the game of life
    subroutine find_steady_state(steady_state, generation_number, global_input_board, global_nrows, global_ncols, &
                                 base_mpi_communicator, nprocs)
        !> Logical flag indicating whether the global board has reached a steady state
        logical, intent(out) :: steady_state
        !> The number of generations required to reach the steady state
        integer, intent(out) :: generation_number
        !> The global board representing the current state of the game
        integer, dimension(:,:), allocatable, intent(in) :: global_input_board
        !> The number of columns in the global board
        integer, intent(in) :: global_nrows
        !> The number of rows in the global board
        integer, intent(in) :: global_ncols
        !> The base MPI communicator for parallel processing
        integer, intent(in) :: base_mpi_communicator
        !> The total number of processes in the MPI communicator
        integer, intent(in) :: nprocs

        !! Board args
        integer, parameter :: max_generations = 100
        integer :: local_nrows, local_ncols, nrows_per_rank, ncols_per_rank
        integer, dimension(:,:), allocatable :: global_board, local_current, local_new
        logical :: local_steady
        integer :: row_start, col_start

        !! MPI args
        integer :: ierr, rank, mpi_req
        integer :: coords(2)
        logical :: periods(2)
        type(DomainDecomposition) :: domainDecomp

        !! MPI args for rank 0 only
        integer :: coords_i(2), neighbours_i(4), row_start_i, col_start_i, local_nrows_i, local_ncols_i

        !! Timing
        real :: start_time, end_time

        !! Misc
        integer :: i, j

        local_steady = .false.
        steady_state = .false.

        ! Create 2D Cartesian topology
        domainDecomp%dims = 0
        call MPI_Dims_create(nprocs, 2, domainDecomp%dims, ierr)   ! Automatically split into num_ranks_row row num_ranks_col grid
        periods = [ .false., .false. ]
        call MPI_Cart_create(base_mpi_communicator, 2, domainDecomp%dims, periods, .true., domainDecomp%communicator, ierr)
        call MPI_Comm_rank(domainDecomp%communicator, rank, ierr)

        call get_local_grid_info(domainDecomp, rank, global_nrows, global_ncols, nrows_per_rank, ncols_per_rank, coords, &
            row_start, col_start, local_nrows, local_ncols)

        allocate(local_current(local_nrows+2, local_ncols+2))
        allocate(local_new(local_nrows+2, local_ncols+2))
        local_current = 0
        local_new = 0

        ! Scatter global board
        if (rank == 0) then
            allocate(global_board(size(global_input_board, 1), size(global_input_board, 2)))
            global_board = global_input_board
            do i = 1, nprocs - 1
                call MPI_RECV(col_start_i, 1, MPI_INTEGER, i, i*100, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
                call MPI_RECV(row_start_i, 1, MPI_INTEGER, i, i*100 + 1, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
                call MPI_RECV(local_ncols_i, 1, MPI_INTEGER, i, i*100 + 2, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
                call MPI_RECV(local_nrows_i, 1, MPI_INTEGER, i, i*100 + 3, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)

                call MPI_Send(global_board(row_start_i:row_start_i+local_nrows_i-1, col_start_i:col_start_i+local_ncols_i-1), &
                    local_nrows_i*local_ncols_i, MPI_INTEGER, i, i*100 + 4, domainDecomp%communicator, ierr)
            end do

            local_current(2:local_nrows+1, 2:local_ncols+1) = global_board(1:local_nrows, 1:local_ncols)
        else
            call MPI_ISEND(col_start, 1, MPI_INTEGER, 0, rank*100, domainDecomp%communicator, mpi_req, ierr)
            call MPI_ISEND(row_start, 1, MPI_INTEGER, 0, rank*100 + 1, domainDecomp%communicator, mpi_req, ierr)
            call MPI_ISEND(local_ncols, 1, MPI_INTEGER, 0, rank*100 + 2, domainDecomp%communicator, mpi_req, ierr)
            call MPI_ISEND(local_nrows, 1, MPI_INTEGER, 0, rank*100 + 3, domainDecomp%communicator, mpi_req, ierr)

            call MPI_Recv(local_current(2:local_nrows+1, 2:local_ncols+1), local_nrows*local_ncols, MPI_INTEGER, &
                        0, rank*100 + 4, domainDecomp%communicator, MPI_STATUS_IGNORE, ierr)
        endif

        local_new = local_current

        call MPI_Barrier(domainDecomp%communicator, ierr)

        generation_number = 0
        local_steady = .false.

        do while (.not. local_steady .and. generation_number < max_generations)
            ! Exchange ghost cells with neighbors
            call exchange_boundaries(local_current, local_ncols, local_nrows, domainDecomp)

            ! Evolution
            call evolve_board(local_current, local_new)
            call check_for_steady_state(local_steady, local_current, local_new)

            call MPI_Allreduce(local_steady, steady_state, 1, MPI_LOGICAL, MPI_LAND, domainDecomp%communicator, ierr)
            local_steady = steady_state

            local_current = local_new

            generation_number = generation_number + 1
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
    end subroutine evolve_board

    !> Check if we have reached steady state, i.e. current and new board match
    subroutine check_for_steady_state(steady_state, current_board, new_board)
        !> Logical to indicate whether current and new board match
        logical, intent(out) :: steady_state
        !> The board as it currently is before this iteration
        integer, dimension(:,:), intent(in) :: current_board
        !> The board into which the new state has been stored after this iteration
        integer, dimension(:,:), intent(in) :: new_board

        integer :: nrows, ncols, row, col

        nrows = size(current_board, 1)
        ncols = size(current_board, 2)

        steady_state = .true.
        do col = 2, ncols-1
            do row = 2, nrows-1
                if (current_board(row, col) /= new_board(row, col)) then
                    steady_state = .false.
                end if
            end do
        end do
    end subroutine check_for_steady_state

end module game_of_life
