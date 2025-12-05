module cli
    implicit none
    public

contains

    !> Read a cli arg at a given index and return it as a string (character array)
    subroutine read_cli_arg(arg_index, arg)
        !> The index of the cli arg to try and read
        integer, intent(in) :: arg_index
        !> The string into which to store the cli arg
        character(len=:), allocatable, intent(out) :: arg

        integer                       :: argl
        character(len=:), allocatable :: cli_arg_temp_store

        call get_command_argument(arg_index, length=argl)
        allocate(character(argl) :: cli_arg_temp_store)
        call get_command_argument(arg_index, cli_arg_temp_store)
        arg = trim(cli_arg_temp_store)
    end subroutine read_cli_arg

end module cli
