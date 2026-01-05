! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program main
    ! allow(C121)
    use mpi
    use cli, only : read_cli_arg
    use game_of_life, only : check_for_steady_state, evolve_board, find_steady_state
    use io, only : read_model_from_file
    implicit none

    !! Board args
    integer, dimension(:,:), allocatable :: global_input_board
    integer :: generation_number, global_nrows, global_ncols
    logical :: local_steady = .false.

    !! CLI args
    character(len=:), allocatable :: executable_name, input_filename

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
        ! Get current_board file path from command line
        if (command_argument_count() == 1) then
            call read_cli_arg(1, input_filename)
        else
            write(*,'(A)') "Error: Invalid input"
            call read_cli_arg(0, executable_name)
            write(*,'(A,A,A)') "Usage: ", executable_name, " <input_file_name>"
            error_found = .true.
        end if

        if (.not. error_found) then
            call read_model_from_file(input_filename, global_input_board, io_error_message)

            if (allocated(io_error_message)) then
                write (*,*) io_error_message
                deallocate(io_error_message)
                error_found = .true.
            end if

            if (.not. error_found) then
                global_ncols = size(global_input_board, 1)
                global_nrows = size(global_input_board, 2)
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
    call MPI_Bcast(global_nrows, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call MPI_Bcast(global_ncols, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    call find_steady_state(local_steady, generation_number, global_input_board, global_ncols, global_nrows, MPI_COMM_WORLD, nprocs)

    ! if (rank == 0) then
    !     write(*,*) "Hello from rank 0", local_steady, generation_number
    ! end if

    if (rank == 0) then
        if (local_steady) then
            write(*,'(a,i6,a)') "Reached steady after ", generation_number, " generations"
        else
            write(*,'(a,i6,a)') "Did NOT Reach steady after ", generation_number, " generations"
        end if
    end if
    call MPI_Finalize(ierr)

end program main
