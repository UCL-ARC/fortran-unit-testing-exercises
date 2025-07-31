! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life
    use game_of_life_mod, only : evolve_board, check_for_steady_state, draw_board, read_model_from_file, find_steady_state

    implicit none

    !! Board args
    integer, parameter :: max_nrow = 100, max_ncol = 100
    integer :: generation_number
    integer, dimension(:,:), allocatable :: current_board

    !! Animation args
    logical :: steady_state = .false.

    !! CLI args
    integer                       :: argl
    character(len=:), allocatable :: cli_arg_temp_store, input_fname

    !! IO args
    character(len=:), allocatable :: io_error_message

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

    call find_steady_state(steady_state, generation_number, current_board)

    if (steady_state) then
        write(*,'(a,i6,a)') "Reached steady after ", generation_number, " generations"
    else
        write(*,'(a,i6,a)') "Did NOT Reach steady after ", generation_number, " generations"
    end if

    deallocate(current_board)

end program game_of_life
