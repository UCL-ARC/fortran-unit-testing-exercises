module io
    implicit none
    public

contains

    !> Populate the board from a provided file
    subroutine read_model_from_file(input_filename, board, io_error_message)
        character(len=:), allocatable, intent(in) :: input_filename
        integer, dimension(:,:), allocatable, intent(out) :: board
        !> A flag to indicate if reading the file was successful
        character(len=:), allocatable, intent(inout) :: io_error_message

        ! Board definition args
        integer :: row, nrow, ncol
        integer, parameter :: max_nrows = 100, max_ncols = 100

        ! File IO args
        integer :: input_file_io, iostat
        character(len=80) :: text_to_discard

        input_file_io = 1111

        ! Open input file
        open(unit=input_file_io,   &
            file=input_filename, &
            status='old',  &
            IOSTAT=iostat)

        if( iostat == 0) then
            ! Read in board from file
            read(input_file_io,'(a)') text_to_discard ! Skip first line
            read(input_file_io,*) nrow, ncol

            ! Verify the number of rows and columns read from the file
            if (nrow < 1 .or. nrow > max_nrows) then
                allocate(character(100) :: io_error_message)
                write (io_error_message,'(a,i6,a,i6)') "nrow must be a positive integer less than ", max_nrows, " found ", nrow
            elseif (ncol < 1 .or. ncol > max_ncols) then
                allocate(character(100) :: io_error_message)
                write (io_error_message,'(a,i6,a,i6)') "ncol must be a positive integer less than ", max_ncols, " found ", ncol
            end if
        else
            allocate(character(100) :: io_error_message)
            write(io_error_message,'(a)') ' *** Error when opening '//input_filename
        endif

        if (.not. allocated(io_error_message)) then

            allocate(board(nrow, ncol))

            read(input_file_io,'(a)') text_to_discard ! Skip next line
            ! Populate the boards starting state
            do row = 1, nrow
                read(input_file_io,*) board(row, :)
            end do

        end if

        close(input_file_io)
    end subroutine read_model_from_file

end module io
