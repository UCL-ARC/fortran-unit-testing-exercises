! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life
    ! allow(C121)
    use mpi
    use game_of_life_mod, only : read_model_from_file, find_steady_state
    implicit none

    !! Board args
    integer, parameter :: max_nx = 50000, max_ny = 50000
    integer :: generation_number, global_nx, global_ny
    integer, dimension(:,:), allocatable :: global_board
    logical :: local_steady = .false.

    !! CLI args
    integer :: argl
    character(len=:), allocatable :: cli_arg_temp_store, input_fname

    !! IO args
    character(len=:), allocatable :: io_error_message

    !! MPI args
    integer :: ierr, rank, nprocs
    logical :: error_found = .false.

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
            error_found = .true.
        end if

        if (.not. error_found) then
            call read_model_from_file(input_fname, max_nx, max_ny, global_board, io_error_message)

            if (allocated(io_error_message)) then
                write (*,*) io_error_message
                error_found = .true.
            end if

            if (.not. error_found) then
                global_ny = size(global_board, 1)
                global_nx = size(global_board, 2)
            end if
        end if
    end if

    ! Check for input errors
    call MPI_Bcast(error_found, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
    if (error_found) then
        call MPI_Finalize(ierr)
        stop
    end if

    ! Broadcast global dimensions
    call MPI_Bcast(global_nx, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call MPI_Bcast(global_ny, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    call find_steady_state(local_steady, generation_number, global_board, global_ny, global_nx, MPI_COMM_WORLD, nprocs)

    if (rank == 0) then
        if (local_steady) then
            write(*,'(a,i6,a)') "Reached steady after ", generation_number, " generations"
        else
            write(*,'(a,i6,a)') "Did NOT Reach steady after ", generation_number, " generations"
        end if
    end if

    call MPI_Finalize(ierr)
end program game_of_life
