module netcdf_cxx_mod
    use iso_c_binding, only : c_int, c_ptr, c_null_ptr, c_loc, c_float, c_long, c_double
    use f_c_string_t_mod, only : f_c_string_t, check_f_c_string
    use f_c_string_array_t_mod, only : f_c_string_array_t, check_f_c_string_array
    use netcdf, only : nf90_char, nf90_string
    use netcdf_cxx_i_mod, only : c_netcdfCreate, c_netcdfClose, c_netcdfAddGroup, c_netcdfAddDim, &
            c_netcdfAddVar, c_netcdfPutVarInt, c_netcdfPutVarInt64, c_netcdfPutVarReal, c_netcdfPutVarDouble, c_netcdfPutVarString, &
            c_netcdfPutVarChar, c_netcdfSetFillInt, c_netcdfSetFillInt64, c_netcdfSetFillReal, c_netcdfSetFillString, &
            c_netcdfPutAttInt, c_netcdfPutAttString, c_netcdfPutAttIntArray, c_netcdfPutAttRealArray
    implicit none
    public

    interface netcdfPutAtt
        module procedure netcdfPutAtt
        module procedure netcdfPutAttArray
    end interface netcdfPutAtt

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
        integer :: status
        ! Set the mode to the provided fileMode if present, otherwise default to 2
        if (present(fileMode)) then
            mode = fileMode
        else
            mode = 2
        end if
        f_c_string_path = f_c_string_t(path)
        status = check_f_c_string(f_c_string_path%to_c())
        c_path = check_f_c_string(f_c_string_path%get_c_string())
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
        integer :: status
        type(c_ptr) :: c_parentGroupName
        type(c_ptr) :: c_groupName
        type(f_c_string_t) :: f_c_string_parentGroupName
        type(f_c_string_t) :: f_c_string_groupName

        if (present(parentGroupName)) then
            f_c_string_parentGroupName = f_c_string_t(parentGroupName)
            status = check_f_c_string(f_c_string_parentGroupName%to_c())
            c_parentGroupName = check_f_c_string(f_c_string_parentGroupName%get_c_string())
        else
            c_parentGroupName = c_null_ptr
        end if
        f_c_string_groupName = f_c_string_t(groupName)
        status = check_f_c_string(f_c_string_groupName%to_c())
        c_groupName = check_f_c_string(f_c_string_groupName%get_c_string())
        netcdfAddGroup = c_netcdfAddGroup(netcdfID, c_parentGroupName, c_groupName)
    end function netcdfAddGroup

    ! netcdfAddDim:
    !   Adds a new dimension to a NetCDF file, either in a specified group or as a global dimension.
    !
    ! Arguments:
    !   - netcdfID (integer(c_int), intent(in), value):
    !       Identifier of the NetCDF file.
    !   - dimName (character(len=*), intent(in)):
    !       Name of the new dimension.
    !   - len (integer(c_int), intent(in), value):
    !       Length of the dimension.
    !  - dimID (integer(c_int), intent(out)):
    !       Identifier of the new dimension.
    !   - groupName (character(len=*), intent(in), optional):
    !       Name of the target group. If absent, the dimension is added as a global dimension.
    !
    ! Returns:
    !    - integer(c_int): A status code indicating the outcome of the operation:
    !       - 0: Success.
    !       - Non-zero: Failure
    function netcdfAddDim(netcdfID, dimName, len, dimID, groupName)
        integer(c_int), value, intent(in) :: netcdfID
        character(len = *), intent(in) :: dimName
        integer(c_int), value, intent(in) :: len
        integer(c_int), intent(out) :: dimID
        character(len = *), optional, intent(in) :: groupName
        integer(c_int) :: netcdfAddDim
        type(c_ptr) :: c_groupName
        type(c_ptr) :: c_dimName
        type(f_c_string_t) :: f_c_string_groupName
        type(f_c_string_t) :: f_c_string_dimName
        integer(c_int) :: status

        if (present(groupName)) then
            f_c_string_groupName = f_c_string_t(groupName)
            status = check_f_c_string(f_c_string_groupName%to_c())
            c_groupName = check_f_c_string(f_c_string_groupName%get_c_string())
        else
            c_groupName = c_null_ptr
        end if
        f_c_string_dimName = f_c_string_t(dimName)
        status = check_f_c_string(f_c_string_dimName%to_c())
        c_dimName = check_f_c_string(f_c_string_dimName%get_c_string())

        netcdfAddDim = c_netcdfAddDim(netcdfID, c_groupName, c_dimName, len, dimID)
        dimID = dimID + 1
    end function netcdfAddDim

    ! netcdfAddVar:
    !   Adds a new variable to a NetCDF file, specifying its name, type, dimensions, and target group.
    !
    !   Arguments:
    !     - netcdfID (integer(c_int), intent(in), value):
    !       The identifier of the NetCDF file to which the variable will be added.
    !     - varName (character(len=*), intent(in)):
    !       The name of the new variable to be created.
    !     - netcdfDataType (integer(c_int), intent(in), value):
    !       The NetCDF data type of the variable (e.g., `NF90_INT`, `NF90_REAL`).
    !     - numDims (integer(c_int), intent(in), value):
    !       The number of dimensions associated with the variable.
    !     - dimNames (character(len=*), dimension(numDims), intent(in)):
    !       An array of dimension names that define the variable's shape.
    !     - groupName (character(len=*), intent(in), optional):
    !       The name of the group in which the variable will be created.
    !       If not provided, the variable will be added as a global variable.
    !     - fillValue (class(*), intent(in), optional):
    !       The fill value to be used for the variable.
    !
    !   Returns:
    !     - integer(c_int): A status code indicating the outcome of the operation:
    !         - 0: Success.
    !         - Non-zero: Failure.
    function netcdfAddVar(netcdfID, varName, netcdfDataType, numDims, dimNames, groupName, fillValue)
        integer(c_int), value, intent(in) :: netcdfID
        character(len = *), intent(in) :: varName
        integer(c_int), value, intent(in) :: netcdfDataType
        integer(c_int), value, intent(in) :: numDims
        character(len = *), dimension(numDims), intent(in) :: dimNames
        character(len = *), optional, intent(in) :: groupName
        class(*), intent(in), optional :: fillValue
        integer(c_int) :: netcdfAddVar
        integer :: status
        type(c_ptr) :: c_groupName
        type(c_ptr) :: c_varName
        type(c_ptr) :: c_dimNames
        type(f_c_string_t) :: f_c_string_groupName
        type(f_c_string_t) :: f_c_string_varName
        type(f_c_string_array_t) :: f_c_string_array_dimNames

        if (present(groupName)) then
            f_c_string_groupName = f_c_string_t(groupName)
            status = check_f_c_string(f_c_string_groupName%to_c())
            c_groupName = check_f_c_string(f_c_string_groupName%get_c_string())
        else
            c_groupName = c_null_ptr
        end if
        f_c_string_varName = f_c_string_t(varName)
        status = check_f_c_string(f_c_string_varName%to_c())
        c_varName = check_f_c_string(f_c_string_varName%get_c_string())
        f_c_string_array_dimNames = f_c_string_array_t(dimNames)
        status = check_f_c_string_array(f_c_string_array_dimNames%to_c())
        c_dimNames = check_f_c_string_array(f_c_string_array_dimNames%get_c_string_array())
        netcdfAddVar = c_netcdfAddVar(netcdfID, c_groupName, c_varName, &
                netcdfDataType, numDims, c_dimNames)
        if (present(fillValue)) then
            netcdfAddVar = netcdfSetFill(netcdfID, varName, 1, fillValue, groupName)
        end if
    end function netcdfAddVar

    ! netcdfPutVar:
    !   Writes data to a variable in a NetCDF file.
    !
    !   Arguments:
    !     - netcdfID (integer(c_int), intent(in), value):
    !       The identifier of the NetCDF file where the data will be written.
    !     - varName (character(len=*), intent(in)):
    !       The name of the variable to which data will be written.
    !     - values (class(*), dimension(:), intent(in)):
    !       The data to be written to the variable.
    !     - groupName (character(len=*), intent(in), optional):
    !       The name of the group containing the variable.
    !       If not provided, the variable is assumed to be a global variable.
    !
    !   Returns:
    !     - integer(c_int): A status code indicating the outcome of the operation:
    !         -  0: Success.
    !         - -1: NetCDF operation returned an error, but the error code was 0.
    !         - -2: Unsupported type passed for values.
    !         - Other nonzero values: Specific NetCDF error codes.
    function netcdfPutVar(netcdfID, varName, values, groupName, netcdfDataType)
        integer(c_int), value, intent(in) :: netcdfID
        character(len = *), intent(in) :: varName
        class(*), dimension(:), target, intent(in) :: values
        character(len = *), optional, intent(in) :: groupName
        integer(c_int), intent(in), optional :: netcdfDataType
        integer(c_int) :: netcdfPutVar
        integer :: status
        type(f_c_string_t) :: f_c_string_groupName
        type(f_c_string_t) :: f_c_string_varName
        type(c_ptr) :: c_groupName
        type(c_ptr) :: c_varName
        type(c_ptr) :: c_values
        type(f_c_string_array_t) :: f_c_string_array_values

        if (present(groupName)) then
            f_c_string_groupName = f_c_string_t(groupName)
            status = check_f_c_string(f_c_string_groupName%to_c())
            c_groupName = check_f_c_string(f_c_string_groupName%get_c_string())
        else
            c_groupName = c_null_ptr
        end if
        f_c_string_varName = f_c_string_t(varName)
        status = check_f_c_string(f_c_string_varName%to_c())
        c_varName = check_f_c_string(f_c_string_varName%get_c_string())

        select type (values)
        type is (integer(c_int))
            c_values = c_loc(values)
            netcdfPutVar = c_netcdfPutVarInt(netcdfID, c_groupName, &
                    c_varName, c_values)

        type is (integer(c_long))
            c_values = c_loc(values)
            netcdfPutVar = c_netcdfPutVarInt64(netcdfID, c_groupName, &
                    c_varName, c_values)

        type is (real(c_float))
            c_values = c_loc(values)
            netcdfPutVar = c_netcdfPutVarReal(netcdfID, c_groupName, &
                    c_varName, c_values)

        type is (real(c_double))
            c_values = c_loc(values)
            netcdfPutVar = c_netcdfPutVarDouble(netcdfID, c_groupName, &
                    c_varName, c_values)

        type is (character(len = *))
            f_c_string_array_values = f_c_string_array_t(values)
            status = check_f_c_string_array(f_c_string_array_values%to_c())
            c_values = check_f_c_string_array(f_c_string_array_values%get_c_string_array())
            if (present(netcdfDataType)) then
                if (netcdfDataType == nf90_char) then
                    netcdfPutVar = c_netcdfPutVarChar(netcdfID, c_groupName, &
                            c_varName, c_values)
                else
                    netcdfPutVar = c_netcdfPutVarString(netcdfID, c_groupName, &
                            c_varName, c_values)
                end if
            else
                netcdfPutVar = c_netcdfPutVarString(netcdfID, c_groupName, &
                        c_varName, c_values)
            end if
        class default
            netcdfPutVar = -2
        end select
    end function netcdfPutVar

    ! netcdfSetFill:
    !   Sets the fill mode and fill value for a variable in a NetCDF file.
    !
    !   Arguments:
    !     - netcdfID (integer(c_int), intent(in), value):
    !       The identifier of the NetCDF file containing the variable.
    !     - varName (character(len=*), intent(in)):
    !       The name of the variable for which the fill mode is set.
    !     - fillMode (integer(c_int), intent(in), value):
    !       The fill mode to be applied:
    !         - 0: Turn off fill mode (use uninitialized values).
    !         - 1: Turn on fill mode (use specified fill value).
    !     - fillValue (class(*), intent(in)):
    !       The fill value to be applied when fill mode is enabled.
    !       Must match the data type of the variable.
    !     - groupName (character(len=*), intent(in), optional):
    !       The name of the group containing the variable.
    !       If not provided, the variable is assumed to be a global variable.
    !
    !   Returns:
    !     - integer(c_int): A status code indicating the outcome of the operation:
    !         -  0: Success.
    !         - -1: NetCDF operation returned an error, but the error code was 0.
    !         - -2: Unsupported type passed for fillValue.
    !         - Other nonzero values: Specific NetCDF error codes.
    function netcdfSetFill(netcdfID, varName, fillMode, fillValue, groupName)
        integer(c_int), value, intent(in) :: netcdfID
        character(len = *), intent(in) :: varName
        integer(c_int), value, intent(in) :: fillMode
        class(*), target, intent(in) :: fillValue
        character(len = *), optional, intent(in) :: groupName
        integer(c_int) :: netcdfSetFill
        integer :: status
        type(f_c_string_t) :: f_c_string_groupName
        type(f_c_string_t) :: f_c_string_varName
        type(c_ptr) :: c_groupName
        type(c_ptr) :: c_varName
        type(f_c_string_t) :: f_c_string_fillValue
        type(c_ptr) :: c_fillValue

        if (present(groupName)) then
            f_c_string_groupName = f_c_string_t(groupName)
            status = check_f_c_string(f_c_string_groupName%to_c())
            c_groupName = check_f_c_string(f_c_string_groupName%get_c_string())
        else
            c_groupName = c_null_ptr
        end if
        f_c_string_varName = f_c_string_t(varName)
        status = check_f_c_string(f_c_string_varName%to_c())
        c_varName = check_f_c_string(f_c_string_varName%get_c_string())

        select type (fillValue)
        type is (integer(c_int))
            netcdfSetFill = c_netcdfSetFillInt(netcdfID, c_groupName, &
                    c_varName, fillMode, fillValue)

        type is (integer(c_long))
            netcdfSetFill = c_netcdfSetFillInt64(netcdfID, c_groupName, &
                    c_varName, fillMode, fillValue)

        type is (real(c_float))
            netcdfSetFill = c_netcdfSetFillReal(netcdfID, c_groupName, &
                    c_varName, fillMode, fillValue)

        type is (character(len = *))
            f_c_string_fillValue = f_c_string_t(fillValue)
            status = check_f_c_string(f_c_string_fillValue%to_c())
            c_fillValue = check_f_c_string(f_c_string_fillValue%get_c_string())
            netcdfSetFill = c_netcdfSetFillString(netcdfID, c_groupName, &
                    c_varName, fillMode, &
                    c_fillValue)
        class default
            netcdfSetFill = -2
        end select
    end function netcdfSetFill

    ! netcdfPutAtt:
    !   Writes an attribute to a NetCDF variable, group, or as a global attribute.
    !
    !   Arguments:
    !     - netcdfID (integer(c_int), intent(in), value):
    !       The identifier of the NetCDF file.
    !     - attName (character(len=*), intent(in)):
    !       The name of the attribute to be written.
    !     - attValue (class(*), intent(in)):
    !       The value of the attribute. Must be of a supported NetCDF type,
    !       such as integer(c_int) or character(len=*). Unsupported types will
    !       result in an error with status code -2.
    !     - varName (character(len=*), intent(in), optional):
    !       The name of the variable to which the attribute will be assigned.
    !       If not provided, the attribute is assigned to the group instead.
    !     - groupName (character(len=*), intent(in), optional):
    !       The name of the group containing the variable.
    !       If not provided, the attribute is written as a global attribute.
    !
    !   Returns:
    !     - integer(c_int): A status code indicating the outcome of the operation:
    !         -  0: Success.
    !         - -1: NetCDF operation returned an error, but the error code was 0.
    !         - -2: Unsupported type passed for attValue.
    !         - Other nonzero values: Specific NetCDF error codes.
    function netcdfPutAtt(netcdfID, attName, attValue, varName, groupName)
        integer(c_int), value, intent(in) :: netcdfID
        character(len = *), intent(in) :: attName
        class(*), target, intent(in) :: attValue
        character(len = *), optional, intent(in) :: varName
        character(len = *), optional, intent(in) :: groupName
        integer(c_int) :: netcdfPutAtt
        integer :: status
        type(f_c_string_t) :: f_c_string_attName
        type(f_c_string_t) :: f_c_string_groupName
        type(f_c_string_t) :: f_c_string_varName
        type(c_ptr) :: c_attName
        type(c_ptr) :: c_groupName
        type(c_ptr) :: c_varName
        type(c_ptr) :: c_attValue
        type(f_c_string_t) :: f_c_string_attValue

        if (present(groupName)) then
            f_c_string_groupName = f_c_string_t(groupName)
            status = check_f_c_string(f_c_string_groupName%to_c())
            c_groupName = check_f_c_string(f_c_string_groupName%get_c_string())
        else
            c_groupName = c_null_ptr
        end if
        if (present(varName)) then
            f_c_string_varName = f_c_string_t(varName)
            status = check_f_c_string(f_c_string_varName%to_c())
            c_varName = check_f_c_string(f_c_string_varName%get_c_string())
        else
            c_varName = c_null_ptr
        end if

        f_c_string_attName = f_c_string_t(attName)
        status = check_f_c_string(f_c_string_attName%to_c())
        c_attName = check_f_c_string(f_c_string_attName%get_c_string())

        select type (attValue)
        type is (integer(c_int))
            c_attValue = c_loc(attValue)
            netcdfPutAtt = c_netcdfPutAttInt(netcdfID, c_attName, c_attValue, c_varName, c_groupName)
        type is (character(len = *))
            f_c_string_attValue = f_c_string_t(attValue)
            status = check_f_c_string(f_c_string_attValue%to_c())
            c_attValue = check_f_c_string(f_c_string_attValue%get_c_string())
            netcdfPutAtt = c_netcdfPutAttString(netcdfID, c_attName, c_attValue, c_varName, c_groupName)
        class default
            netcdfPutAtt = -2
        end select
    end function netcdfPutAtt

    ! netcdfPutAttArray:
    !   Writes a 1D attribute to a NetCDF variable, group, or as a global attribute.
    !
    !   Arguments:
    !     - netcdfID (integer(c_int), intent(in), value):
    !       The identifier of the NetCDF file.
    !     - attName (character(len=*), intent(in)):
    !       The name of the attribute to be written.
    !     - attValue (class(*), dimension(:), intent(in)):
    !       The values of the attribute as a one-dimensional array. Must be of a
    !       supported NetCDF type, such as integer(c_int) or character(len=*).
    !       If the type is unsupported, the function will return an error
    !       status code of -2.
    !     - attLen (integer(c_int), intent(in), value):
    !       The length of the attribute array.
    !     - varName (character(len=*), intent(in), optional):
    !       The name of the variable to which the attribute will be assigned.
    !       If not provided, the attribute is assigned to the group instead.
    !     - groupName (character(len=*), intent(in), optional):
    !       The name of the group containing the variable.
    !       If not provided, the attribute is written as a global attribute.
    !
    !   Returns:
    !     - integer(c_int): A status code indicating the outcome of the operation:
    !         -  0: Success.
    !         - -1: NetCDF operation returned an error, but the error code was 0.
    !         - -2: Unsupported type passed for attValue.
    !         - Other nonzero values: Specific NetCDF error codes.
    function netcdfPutAttArray(netcdfID, attName, attValue, attLen, varName, groupName)
        integer(c_int), value, intent(in) :: netcdfID
        character(len = *), intent(in) :: attName
        class(*), target, intent(in) :: attValue(:)
        integer(c_int), intent(in), value :: attLen
        character(len = *), optional, intent(in) :: varName
        character(len = *), optional, intent(in) :: groupName
        integer(c_int) :: netcdfPutAttArray
        integer :: status
        type(f_c_string_t) :: f_c_string_attName
        type(f_c_string_t) :: f_c_string_groupName
        type(f_c_string_t) :: f_c_string_varName
        type(c_ptr) :: c_attName
        type(c_ptr) :: c_groupName
        type(c_ptr) :: c_varName
        type(c_ptr) :: c_attValue

        if (present(groupName)) then
            f_c_string_groupName = f_c_string_t(groupName)
            status = check_f_c_string(f_c_string_groupName%to_c())
            c_groupName = check_f_c_string(f_c_string_groupName%get_c_string())
        else
            c_groupName = c_null_ptr
        end if
        if (present(varName)) then
            f_c_string_varName = f_c_string_t(varName)
            status = check_f_c_string(f_c_string_varName%to_c())
            c_varName = check_f_c_string(f_c_string_varName%get_c_string())
        else
            c_varName = c_null_ptr
        end if

        f_c_string_attName = f_c_string_t(attName)
        status = check_f_c_string(f_c_string_attName%to_c())
        c_attName = check_f_c_string(f_c_string_attName%get_c_string())

        select type (attValue)
        type is (integer(c_int))
            c_attValue = c_loc(attValue)
            netcdfPutAttArray = c_netcdfPutAttIntArray(netcdfID, c_attName, c_attValue, attLen, c_varName, c_groupName)
        type is (real(c_float))
            c_attValue = c_loc(attValue)
            netcdfPutAttArray = c_netcdfPutAttRealArray(netcdfID, c_attName, c_attValue, attLen, c_varName, c_groupName)
        class default
            netcdfPutAttArray = -2
        end select
    end function netcdfPutAttArray


end module netcdf_cxx_mod
