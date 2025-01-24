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
    end interface

end module netcdf_cxx_i_mod
