module netcdf_cxx_i_mod
    use iso_c_binding, only : c_int, c_ptr
    implicit none
    public

    interface
        ! c_netcdfCreate:
        !   Creates a new NetCDF file or opens an existing file in a specified mode.
        !   The file path must be passed as a C-compatible null-terminated string.
        !   The resulting file identifier is returned in `netcdfID` for further operations.
        !
        !   Arguments:
        !     - path (type(c_ptr), intent(in), value): A C pointer to a null-terminated
        !       string representing the file path.
        !     - netcdfID (integer(c_int), intent(out)): Receives the file identifier
        !       for the created or opened NetCDF file.
        !     - fileMode (integer(c_int), intent(in)): File mode for creating the NetCDF file.
        !
        !   Returns:
        !     - integer(c_int): A status code indicating success (0) or failure (non-zero).
        function c_netcdfCreate(path, netcdfID, fileMode) &
                bind(C, name = "netcdfCreate")
            import :: c_int
            import :: c_ptr
            type(c_ptr), value, intent(in) :: path
            integer(c_int), intent(inout) :: netcdfID
            integer(c_int), value, intent(in) :: fileMode
            integer(c_int) :: c_netcdfCreate
        end function

        ! c_netcdfClose:
        !   Closes a previously opened NetCDF file identified by its file identifier.
        !
        !   Arguments:
        !     - netcdfID (integer(c_int), intent(in), value): The identifier of the
        !       NetCDF file to close.
        !
        !   Returns:
        !     - integer(c_int): A status code indicating success (0) or failure (non-zero).
        function c_netcdfClose(netcdfID) &
                bind(C, name = "netcdfClose")
            import :: c_int
            integer(c_int), value, intent(in) :: netcdfID
            integer(c_int) :: c_netcdfClose
        end function

        ! c_netcdfAddGroup:
        !   Adds a new group to a NetCDF file under a specified parent group.
        !
        !   Arguments:
        !     - netcdfID (integer(c_int), intent(in), value):
        !       The identifier of the NetCDF file where the group will be added.
        !     - parentGroupName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the name of the parent
        !       group under which the new group will be created. If targeting the root
        !       group, pass a c_null_ptr.
        !     - groupName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the name of the new group
        !       to be added under the parent group.
        !
        !   Returns:
        !     - integer(c_int): A status code indicating the outcome of the operation:
        !         - 0: Success.
        !         - Non-zero: Failure
        !
        !   Notes:
        !     - This function assumes `netcdfID` is valid and corresponds to an open NetCDF
        !       file managed by the internal file handling utilities.
        !     - The parent group must exist; otherwise, the operation will fail with an error.
        function c_netcdfAddGroup(&
                netcdfID, parentGroupName, groupName) &
                bind(C, name = "netcdfAddGroup")
            import :: c_int
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: parentGroupName
            type(c_ptr), value, intent(in) :: groupName
            integer(c_int) :: c_netcdfAddGroup
        end function c_netcdfAddGroup

        ! c_netcdfAddDim:
        !   Adds a new dimension to a NetCDF file
        !
        !   Arguments:
        !     - netcdfID (integer(c_int), intent(in), value):
        !       The identifier of the NetCDF file to which the dimension will be added.
        !     - groupName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the name of the group
        !       where the dimension will be created. If creating a global dimension, pass `c_null_ptr`.
        !     - dimName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the name of the new dimension.
        !     - len (integer(c_int), intent(in), value):
        !
        !   Returns:
        !     - integer(c_int): Status code indicating the result of the operation:
        !         - 0: Success.
        !         - Non-zero: Failure.
        !
        !   Notes:
        !     - The function assumes that `netcdfID` is valid and corresponds to an open NetCDF file.
        !     - `dimName` must be valid C pointers pointing to null-terminated strings.
        !   ```
        function c_netcdfAddDim(&
                netcdfID, groupName, dimName, len) &
                bind(C, name = "netcdfAddDim")
            import :: c_int
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: dimName
            integer(c_int), value, intent(in) :: len
            integer(c_int) :: c_netcdfAddDim
        end function c_netcdfAddDim

    end interface

end module netcdf_cxx_i_mod
