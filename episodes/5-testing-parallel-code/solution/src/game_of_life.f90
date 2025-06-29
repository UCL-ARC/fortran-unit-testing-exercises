! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life
    use game_of_life_mod, only : evolve_board, check_for_steady_state, read_model_from_file

    implicit none

    !! Board args
    integer, parameter :: max_nrow = 50000, max_ncol = 50000, max_generations = 100
    integer :: generation_number
    integer, dimension(:,:), allocatable :: current_board, new_board

    !! Animation args
    integer, dimension(8) :: date_time_values
    integer :: mod_ms_step, ms_per_step = 250
    logical :: steady_state = .false., run_with_animation = .true.

    !! CLI args
    integer                       :: argl, i
    character(len=:), allocatable :: cli_arg_temp_store, input_fname

    !! IO args
    character(len=:), allocatable :: io_error_message
    integer :: stat, num_threads

    !! Timing
    real :: start_time, end_time

    !! Get current_board file path from command line
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
        stop
    end if

    ! Q1_FIX3: Extract the file IO into a module procedure to allow it to be tested.
    call read_model_from_file(input_fname, max_nrow, max_ncol, current_board, io_error_message)

    if (allocated(io_error_message)) then
        write (*,*) io_error_message
        deallocate(io_error_message)
        stop
    end if

    allocate(new_board(size(current_board,1), size(current_board, 2)))
    new_board = 0

    generation_number = 0

    call cpu_time(start_time)
    ! Iterate until we reach a steady state
    do while(.not. steady_state .and. generation_number < max_generations)
        call evolve_board(current_board, new_board)
        call check_for_steady_state(current_board, new_board, steady_state)

        current_board = new_board
        generation_number = generation_number + 1
    end do
    call cpu_time(end_time)

    if (steady_state) then
        write(*,'(a,i6,a,F5.3)') "Reached steady after ", generation_number, " generations ", end_time - start_time
    else
        write(*,'(a,i6,a,F5.3)') "Did NOT Reach steady after ", generation_number, " generations ", end_time - start_time
    end if

    deallocate(current_board)
    deallocate(new_board)

end program game_of_life
