module netcdf_cxx_mod
    use iso_c_binding, only : c_int, c_ptr, c_null_ptr
    use f_c_string_t_mod, only : f_c_string_t
    use netcdf_cxx_i_mod, only : c_netcdfCreate, c_netcdfClose, c_netcdfAddGroup
    use netcdf, only : NF90_INT, NF90_REAL
    implicit none
    public

contains

    ! netcdfCreate:
    !   Creates a new NetCDF file or opens an existing file in a specified mode,
    !   using a Fortran string for the file path.
    !
    !   Arguments:
    !     - path (character(len=*), intent(in)): The file path as a Fortran string.
    !     - netcdfID (integer(c_int), intent(inout)): On input, it may contain an
    !       identifier to be updated; on output, it holds the file identifier
    !       for the created or opened NetCDF file.
    !     - fileMode (integer(c_int), intent(in), optional):
    !         File mode for creating or opening the NetCDF file. Defaults to 2
    !         (replace mode). Possible values are:
    !           - 0: Open an existing file in read-only mode.
    !           - 1: Open an existing file for writing.
    !           - 2: Create a new file, overwriting any existing file.
    !           - 3: Create a new file, failing if the file already exists.
    !
    !   Returns:
    !     - integer(c_int): A status code indicating success (0) or failure (non-zero).
    function netcdfCreate(path, netcdfID, fileMode)
        character(len = *), intent(in) :: path
        integer(c_int), intent(inout) :: netcdfID
        integer(c_int), intent(in), optional :: fileMode
        integer(c_int) :: netcdfCreate
        type(f_c_string_t) :: f_c_string_path
        type(c_ptr) :: c_path
        integer(c_int) :: mode
        ! Set the mode to the provided fileMode if present, otherwise default to 2
        if (present(fileMode)) then
            mode = fileMode
        else
            mode = 2
        end if
        c_path = f_c_string_path%to_c(path)
        netcdfCreate = c_netcdfCreate(c_path, netcdfID, mode)
    end function netcdfCreate

    ! netcdfClose:
    !   Closes a previously opened NetCDF file identified by its file identifier.
    !
    !   Arguments:
    !     - netcdfID (integer(c_int), intent(in), value): The identifier of the
    !       NetCDF file to close.
    !
    !   Returns:
    !     - integer(c_int): A status code indicating success (0) or failure (non-zero).
    function netcdfClose(netcdfID)
        integer(c_int), value, intent(in) :: netcdfID
        integer(c_int) :: netcdfClose
        netcdfClose = c_netcdfClose(netcdfID)
    end function netcdfClose

    ! netcdfAddGroup:
    !   Adds a new group to a NetCDF file under a specified parent group.
    !
    !   Arguments:
    !     - netcdfID (integer(c_int), intent(in), value):
    !       The identifier of the NetCDF file where the group will be added.
    !     - groupName (character(len=*), intent(in)):
    !       The name of the new group to be created within the specified parent group.
    !     - parentGroupName (character(len=*), intent(in), optional):
    !       The name of the parent group under which the new group will be added.
    !       If not provided, the new group will be created in the root group.
    !
    !   Returns:
    !     - integer(c_int): A status code indicating the outcome of the operation:
    !         - 0: Success.
    !         - Non-zero: Failure
    function netcdfAddGroup(netcdfID, groupName, parentGroupName)
        integer(c_int), value, intent(in) :: netcdfID
        character(len = *), intent(in), optional :: parentGroupName
        character(len = *), intent(in) :: groupName
        integer(c_int) :: netcdfAddGroup
        type(c_ptr) :: c_parentGroupName
        type(c_ptr) :: c_groupName
        type(f_c_string_t) :: f_c_string_parentGroupName
        type(f_c_string_t) :: f_c_string_groupName

        if (present(parentGroupName)) then
            c_parentGroupName = f_c_string_parentGroupName%to_c(parentGroupName)
        else
            c_parentGroupName = c_null_ptr
        end if
        c_groupName = f_c_string_groupName%to_c(groupName)

        netcdfAddGroup = c_netcdfAddGroup(netcdfID, c_parentGroupName, c_groupName)
    end function netcdfAddGroup

end module netcdf_cxx_mod
