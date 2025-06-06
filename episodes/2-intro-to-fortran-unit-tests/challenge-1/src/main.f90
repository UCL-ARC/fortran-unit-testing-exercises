! =======================================================
! Conway's game of life
!
! =======================================================
! Adapted from https://github.com/tuckerrc/game_of_life
! =======================================================
program game_of_life

    implicit none

    !! Board args
    integer, parameter :: max_nrow = 100, max_ncol = 100, max_generations = 100, sleep_seconds = 1
    integer :: nrow, ncol
    integer :: i, generation_number, sum, num_generations, steady_state_generation
    integer, dimension(:,:), allocatable :: board, temp_board

    !! Animation args
    integer, dimension(8) :: values
    integer :: mod_val, speed = 250, steady_state_counter = 0
    logical :: steady_state = .false.

    !! CLI args
    integer                       :: argl
    character(len=:), allocatable :: a, input_fname

    !! File IO args
    character(len=80) :: text
    integer :: input_file_io
    integer :: iostat

    ! Get board file path from command line
    if (command_argument_count() == 1) then
        call get_command_argument(1, length=argl)
        allocate(character(argl) :: input_fname)
        call get_command_argument(1, input_fname)
    else
        write(*,'(A)') "Error: Invalid input"
        call get_command_argument(0, length=argl)
        allocate(character(argl) :: a)
        call get_command_argument(0, a)
        write(*,'(A,A,A)') "Usage: ", a, " <input_file_name>"
        deallocate(a)
        stop
    end if

    ! Open input file
    open(unit=input_file_io,   &
         file=input_fname, &
         status='old',  &
         IOSTAT=iostat)

    if( iostat /= 0) then
        write(*,'(a)') ' *** Error when opening '//input_fname
        stop 1
    end if

    ! Read in board from file
    read(input_file_io,'(a)') text ! Skip first line
    read(input_file_io,*) nrow, ncol

    if (num_generations < 1 .or. num_generations > max_generations) then
        write (*,'(a,i6,a,i6)') "num_generations must be a positive integer less than ", max_generations, " found ", num_generations
        stop 1
    end if

    if (nrow < 1 .or. nrow > max_nrow) then
        write (*,'(a,i6,a,i6)') "nrow must be a positive integer less than ", max_nrow, " found ", nrow
        stop 1
    end if

    if (ncol < 1 .or. ncol > max_ncol) then
        write (*,'(a,i6,a,i6)') "ncol must be a positive integer less than ", max_ncol, " found ", ncol
        stop 1
    end if

    allocate(board(nrow, ncol))
    allocate(temp_board(nrow, ncol))

    read(input_file_io,'(a)') text ! Skip next line
    ! Populate the boards starting state
    do i = 1, nrow
        read(input_file_io,*) board(i, :)
    end do

    temp_board = 0

    call system ("clear")

    do while(.not. steady_state)
        call date_and_time(VALUES=values)
        mod_val = mod(values(8), speed)

        if (mod_val == 0) then
            call evolve_board()
            call check_for_steady_state()
            board = temp_board
            call draw_board()

            generation_number = generation_number + 1
        end if

    end do

    write(*,'(a,i6,a)') "Reached steady after ", generation_number, " generations"

    deallocate(board)
    deallocate(temp_board)

contains

    subroutine check_for_steady_state()
        logical :: equal
        integer :: i, j

        equal = .true.

        do i=1, nrow
            do j=1, ncol
                ! write(*,*) equal, board(i, j), prev_board(i, j)
                equal = board(i, j) == temp_board(i, j)
                if (.not. equal) then
                    steady_state_counter = 0
                    return
                end if
            end do
        end do
        if (steady_state_counter == 0) then
            steady_state_generation = generation_number
        end if

        steady_state_counter =  steady_state_counter + 1

        if (steady_state_counter > 1) then
            steady_state = .true.
            return
        end if
        steady_state = .false.
    end subroutine check_for_steady_state

    subroutine evolve_board()
        integer :: i, j
        do i=2, nrow-1
            do j=2, ncol-1
                sum = 0
                sum = board(i-1, j-1) + board(i, j-1) + board(i+1, j-1) !
                sum = sum + board(i-1, j) + board(i+1, j)
                sum = sum + board(i-1, j+1) + board(i, j+1) + board(i+1, j+1)
                if(board(i,j)==1 .and. sum<=1) then
                    temp_board(i,j) = 0
                elseif(board(i,j)==1 .and. sum<=3) then
                    temp_board(i,j) = 1
                elseif(board(i,j)==1 .and. sum>=4)then
                    temp_board(i,j) = 0
                elseif(board(i,j)==0 .and. sum==3)then
                    temp_board(i,j) = 1
                endif
            enddo
        enddo

        return
    end subroutine evolve_board

    subroutine draw_board()
        integer :: i, j
        character(nrow) :: output
        call system("clear")
        do i=1, nrow
            output = ""
            do j=1, ncol
                if (board(i,j) == 1) then
                    output = trim(output)//"#"
                else
                    output = trim(output)//"."
                endif
            enddo
            print *, output
        enddo
    end subroutine draw_board

end program game_of_life
