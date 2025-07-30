! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life
    use game_of_life_mod, only : evolve_board, check_for_steady_state, draw_board, read_model_from_file

    implicit none

    !! Board args
    integer, parameter :: max_nrow = 100, max_ncol = 100, max_generations = 100
    integer :: generation_number
    integer, dimension(:,:), allocatable :: current_board, new_board

    !! Animation args
    integer, dimension(8) :: date_time_values
    integer :: mod_ms_step, ms_per_step = 250
    logical :: steady_state = .false.

    !! CLI args
    integer                       :: argl
    character(len=:), allocatable :: cli_arg_temp_store, input_fname

    !! IO args
    character(len=:), allocatable :: io_error_message
    integer :: stat

    ! Get current_board file path from command line
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

    call read_model_from_file(input_fname, max_nrow, max_ncol, current_board, io_error_message)

    if (allocated(io_error_message)) then
        write (*,*) io_error_message
        deallocate(io_error_message)
        stop
    end if

    allocate(new_board(size(current_board,1), size(current_board, 2)))
    new_board = 0

    ! Clear the terminal screen
    call system ("clear")

    ! Iterate until we reach a steady state
    do while(.not. steady_state .and. generation_number < max_generations)
        ! Advance the simulation in the steps of the requested number of milliosecons
        call date_and_time(VALUES=date_time_values)
        mod_ms_step = mod(date_time_values(8), ms_per_step)

        if (mod_ms_step == 0) then
            call evolve_board(current_board, new_board)
            call check_for_steady_state(current_board, new_board, steady_state)
            current_board = new_board
            call draw_board(current_board)

            generation_number = generation_number + 1
        end if

    end do

    if (steady_state) then
        write(*,'(a,i6,a)') "Reached steady after ", generation_number, " generations"
    else
        write(*,'(a,i6,a)') "Did NOT Reach steady after ", generation_number, " generations"
    end if

    deallocate(current_board)
    deallocate(new_board)

end program game_of_life
