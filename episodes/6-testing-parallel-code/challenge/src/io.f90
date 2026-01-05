module io
    implicit none
    public

contains

    !> Populate the board from a provided file
    subroutine read_model_from_file(input_fname, board, io_error_message)
        !> The name of the file to read in the board
        character(len=:), allocatable, intent(in) :: input_fname
        !> The board to be populated
        integer, dimension(:,:), allocatable, intent(out) :: board
        !> A flag to indicate if reading the file was successful
        character(len=:), allocatable, intent(out) :: io_error_message

        ! Board definition args
        integer :: nrows, ncols, row
        integer, parameter :: max_nrows = 100, max_ncols = 100

        ! File IO args
        integer :: input_file_io, iostat
        real :: rnd_num
        character(len=80) :: text_to_discard

        call random_number(rnd_num)
        input_file_io = floor(rnd_num * 1000.0)

        ! Open input file
        open(unit=input_file_io, file=input_fname, status='old', IOSTAT=iostat)

        if (iostat == 0) then
            read(input_file_io,'(a)') text_to_discard ! Skip first line
            read(input_file_io,*) nrows, ncols

            ! Verify the number of rows read from the file
            if (nrows < 1 .or. nrows > max_nrows) then
                allocate(character(100) :: io_error_message)
                write(io_error_message,'(a,i6,a,i6)') "nrows must be a positive integer less than ", max_nrows, " found ", nrows
            elseif (ncols < 1 .or. ncols > max_ncols) then
                allocate(character(100) :: io_error_message)
                write(io_error_message,'(a,i6,a,i6)') "ncols must be a positive integer less than ", max_ncols, " found ", ncols
            end if
        else
            allocate(character(100) :: io_error_message)
            write(io_error_message,'(a)') ' *** Error when opening '//input_fname
        endif

        if (.not. allocated(io_error_message)) then

            allocate(board(nrows, ncols))

            read(input_file_io,'(a)') text_to_discard ! Skip next line
            ! Populate the boards starting state
            do row = 1, nrows
                read(input_file_io,*) board(row, :)
            end do

        end if

        close(input_file_io)
    end subroutine read_model_from_file

end module io
