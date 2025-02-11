module netcdf_cxx_i_mod
    use iso_c_binding, only : c_int, c_ptr, c_float, c_long
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

        ! c_netcdfAddVar:
        !   Adds a new variable to a NetCDF file, specifying its name, type, and associated dimensions.
        !
        !   Arguments:
        !     - netcdfID (integer(c_int), intent(in), value):
        !       The identifier of the NetCDF file where the variable will be created.
        !     - groupName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the group name. If `c_null_ptr`,
        !       the variable is added as a global variable.
        !     - varName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the variable name.
        !     - netcdfDataType (integer(c_int), intent(in), value):
        !       The NetCDF data type of the variable (e.g., `NF90_INT`, `NF90_REAL`).
        !     - numDims (integer(c_int), intent(in), value):
        !       The number of dimensions associated with the variable.
        !     - dimNames (type(c_ptr), intent(in), value):
        !       A C pointer to an array of null-terminated strings representing the dimension names.
        !
        !   Returns:
        !     - integer(c_int): Status code indicating the result of the operation:
        !         - 0: Success.
        !         - Non-zero: Failure.
        !
        !   Notes:
        !     - This function assumes that `netcdfID` corresponds to a valid NetCDF file.
        !     - All strings must be null-terminated and passed as C pointers.
        function c_netcdfAddVar(&
                netcdfID, groupName, varName, netcdfDataType, numDims, dimNames) &
                bind(C, name = "netcdfAddVar")
            import :: c_int
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            integer(c_int), value, intent(in) :: netcdfDataType
            integer(c_int), value, intent(in) :: numDims
            type(c_ptr), value, intent(in) :: dimNames
            integer(c_int) :: c_netcdfAddVar
        end function c_netcdfAddVar

        ! c_netcdfPutVar:
        !   Writes integer data to a NetCDF variable in the specified group or as a global variable.
        !
        !   Arguments:
        !     - netcdfID (integer(c_int), intent(in), value):
        !       The identifier of the NetCDF file.
        !     - groupName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the group name. If `c_null_ptr`,
        !       the variable is assumed to be a global variable.
        !     - varName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the variable name.
        !     - values (type(c_ptr), intent(in), value):
        !       A C pointer to the array of integer data to be written.
        !
        !   Returns:
        !     - integer(c_int): Status code indicating the result of the operation:
        !         - 0: Success.
        !         - Non-zero: Failure.
        function c_netcdfPutVarInt(&
                netcdfID, groupName, varName, values) &
                bind(C, name = "netcdfPutVarInt")
            import :: c_int
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            type(c_ptr), value, intent(in) :: values
            integer(c_int) :: c_netcdfPutVarInt
        end function c_netcdfPutVarInt

        ! See documentation for `c_netcdfPutVarInt`.
        function c_netcdfPutVarInt64(&
                netcdfID, groupName, varName, values) &
                bind(C, name = "netcdfPutVarInt64")
            import :: c_int
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            type(c_ptr), value, intent(in) :: values
            integer(c_int) :: c_netcdfPutVarInt64
        end function c_netcdfPutVarInt64

        ! See documentation for `c_netcdfPutVarInt`.
        function c_netcdfPutVarReal(&
                netcdfID, groupName, varName, values) &
                bind(C, name = "netcdfPutVarReal")
            import :: c_int
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            type(c_ptr), value, intent(in) :: values
            integer(c_int) :: c_netcdfPutVarReal
        end function c_netcdfPutVarReal

        ! See documentation for `c_netcdfPutVarInt`.
        function c_netcdfPutVarString(&
                netcdfID, groupName, varName, values) &
                bind(C, name = "netcdfPutVarString")
            import :: c_int
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            type(c_ptr), value, intent(in) :: values
            integer(c_int) :: c_netcdfPutVarString
        end function c_netcdfPutVarString

        ! c_netcdfSetFillInt:
        !   Sets the fill mode and fill value for an integer NetCDF variable in the specified group
        !   or as a global variable.
        !
        !   Arguments:
        !     - netcdfID (integer(c_int), intent(in), value):
        !       The identifier of the NetCDF file.
        !     - groupName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the group name. If `c_null_ptr`,
        !       the variable is assumed to be a global variable.
        !     - varName (type(c_ptr), intent(in), value):
        !       A C pointer to a null-terminated string specifying the variable name.
        !     - fillMode (integer(c_int), intent(in), value):
        !       The fill mode flag, typically `NC_FILL` (enable fill) or `NC_NOFILL` (disable fill).
        !     - fillValue (integer(c_int), intent(in), value):
        !       The integer fill value to be used if fill mode is enabled.
        !
        !   Returns:
        !     - integer(c_int): Status code indicating the result of the operation:
        !         - 0: Success.
        !         - Non-zero: Failure.
        function c_netcdfSetFillInt(&
                netcdfID, groupName, varName, fillMode, fillValue) &
                bind(C, name = "netcdfSetFillInt")
            import :: c_int
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            integer(c_int), value, intent(in) :: fillMode
            integer(c_int), value, intent(in) :: fillValue
            integer(c_int) :: c_netcdfSetFillInt
        end function c_netcdfSetFillInt

        ! See documentation for `c_netcdfSetFillInt`.
        function c_netcdfSetFillInt64(&
                netcdfID, groupName, varName, fillMode, fillValue) &
                bind(C, name = "netcdfSetFillInt64")
            import :: c_int
            import :: c_long
            import :: c_ptr
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            integer(c_int), value, intent(in) :: fillMode
            integer(c_long), value, intent(in) :: fillValue
            integer(c_int) :: c_netcdfSetFillInt64
        end function c_netcdfSetFillInt64

        ! See documentation for `c_netcdfSetFillInt`.
        function c_netcdfSetFillReal(&
                netcdfID, groupName, varName, fillMode, fillValue) &
                bind(C, name = "netcdfSetFillReal")
            import :: c_int
            import :: c_ptr
            import :: c_float
            real(c_float), value, intent(in) :: fillValue
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            integer(c_int), value, intent(in) :: fillMode
            integer(c_int) :: c_netcdfSetFillReal
        end function c_netcdfSetFillReal

        ! See documentation for `c_netcdfSetFillInt`.
        function c_netcdfSetFillString(&
                netcdfID, groupName, varName, fillMode, fillValue) &
                bind(C, name = "netcdfSetFillString")
            import :: c_int
            import :: c_ptr
            type(c_ptr), value, intent(in) :: fillValue
            integer(c_int), value, intent(in) :: netcdfID
            type(c_ptr), value, intent(in) :: groupName
            type(c_ptr), value, intent(in) :: varName
            integer(c_int), value, intent(in) :: fillMode
            integer(c_int) :: c_netcdfSetFillString
        end function c_netcdfSetFillString

    end interface

end module netcdf_cxx_i_mod
