! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program main
    use animation, only : animate
    use cli, only : read_cli_arg
    use game_of_life, only : check_for_steady_state, evolve_board, find_steady_state
    use io, only : read_model_from_file
    implicit none

    integer, dimension(:,:), allocatable :: starting_board
    integer :: generation_number
    logical :: steady_state = .false.

    !! CLI args
    character(len=:), allocatable :: executable_name, input_filename

    ! Get current_board file path from command line
    if (command_argument_count() == 1) then
        call read_cli_arg(1, input_filename)
    else
        write(*,'(A)') "Error: Invalid input"
        call read_cli_arg(0, executable_name)
        write(*,'(A,A,A)') "Usage: ", executable_name, " <input_file_name>"
        stop
    end if

    call read_model_from_file(input_filename, starting_board)

    call find_steady_state(steady_state, generation_number, starting_board, animate)

    if (steady_state) then
        write(*,'(a,i6,a)') "Reached steady after ", generation_number, " generations"
    else
        write(*,'(a,i6,a)') "Did NOT Reach steady after ", generation_number, " generations"
    end if

end program main
