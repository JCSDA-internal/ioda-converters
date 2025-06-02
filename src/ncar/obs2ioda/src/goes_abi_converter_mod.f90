module goes_abi_converter_mod

contains

    subroutine check(status)
        integer, intent(in) :: status
        integer, parameter :: success = 0
        if(status /= success) then
            print *, 'Error in output_iodav3. Error code = ', status
            call exit(1)
        end if
    end subroutine check

    ! transpose_and_flatten:
    !   Transposes a 2D real matrix and flattens it into a 1D array.
    !
    !   Arguments:
    !     - mat (real, dimension(:,:), intent(in)):
    !       The input 2D matrix to be transposed and flattened.
    !     - flat_mat_trans (real, dimension(:), intent(out)):
    !       The output 1D array containing the flattened transpose of `mat`.
    !       Must be pre-allocated with size equal to size(mat,1) * size(mat,2).
    !
    !   Notes:
    !     - The subroutine performs an internal check to ensure `flat_mat_trans`
    !       has the correct size. If not, it prints an error message and stops execution.
    subroutine transpose_and_flatten(mat, flat_mat_trans)
        implicit none
        real, intent(in)  :: mat(:,:)
        real, intent(out) :: flat_mat_trans(:)
        integer :: m, n, i, j

        ! Get dimensions of input matrix
        m = size(mat, 1)
        n = size(mat, 2)

        ! Safety check
        if (size(flat_mat_trans) /= m * n) then
            print *, "Error: flat_mat_trans must have size m*n"
            stop 1
        end if

        ! Transpose and flatten
        flat_mat_trans = reshape(transpose(mat), [m*n])
    end subroutine transpose_and_flatten

    ! write_iodav3_netcdf:
    !   Writes GOES-ABI observation data into a NetCDF file formatted for IODA-v3.
    !
    !   Arguments:
    !     - fname (character(len=*), intent(in)):
    !       Path to the output NetCDF file.
    !     - nlocs (integer(i_kind), intent(in)):
    !       Number of observation locations.
    !     - nchans (integer(i_kind), intent(in)):
    !       Number of instrument channels.
    !     - missing_r (real(r_kind), intent(in)):
    !       Fill value for missing real values.
    !     - missing_i (integer(i_kind), intent(in)):
    !       Fill value for missing integer values.
    !     - datetime (integer(i_llong), dimension(nlocs), intent(in)):
    !       Array of datetime values in 64-bit integer format.
    !     - lat_out, lon_out (real(r_kind), dimension(nlocs), intent(in)):
    !       Latitude and longitude values.
    !     - scan_pos_out, sat_zen_out, sat_azi_out (real(r_kind), dimension(nlocs), intent(in)):
    !       Scan position and sensor viewing geometry.
    !     - sun_zen_out, sun_azi_out (real(r_kind), dimension(nlocs), intent(in)):
    !       Solar geometry values.
    !     - bt_out (real(r_kind), dimension(nchans, nlocs), intent(in)):
    !       Brightness temperature values (K).
    !     - err_out (real(r_kind), dimension(nchans, nlocs), intent(in)):
    !       Observation error estimates.
    !     - qf_out (real(r_kind), dimension(nchans, nlocs), intent(in)):
    !       Pre-quality control flags.
    subroutine write_iodav3_netcdf(fname, nlocs, nchans, missing_r, missing_i, &
            datetime, lat_out, lon_out, scan_pos_out, sat_zen_out, sat_azi_out, &
            sun_zen_out, sun_azi_out, bt_out, err_out, qf_out)

        use netcdf_cxx_mod
        use define_mod, only: r_kind, i_kind, i_llong
        use netcdf, only: NF90_REAL, NF90_INT, NF90_INT64
        implicit none

        character(len=*), intent(in) :: fname
        integer(i_kind),  intent(in) :: nlocs, nchans
        real(r_kind),     intent(in) :: missing_r
        integer(i_kind),  intent(in) :: missing_i
        integer(i_llong), intent(in) :: datetime(nlocs)
        real(r_kind), intent(in) :: lat_out(nlocs), lon_out(nlocs)
        real(r_kind), intent(in) :: scan_pos_out(nlocs), sat_zen_out(nlocs), sat_azi_out(nlocs)
        real(r_kind), intent(in) :: sun_zen_out(nlocs), sun_azi_out(nlocs)
        real(r_kind), intent(in) :: bt_out(nchans, nlocs)
        real(r_kind), intent(in) :: err_out(nchans, nlocs)
        real(r_kind), intent(in) :: qf_out(nchans, nlocs)

        integer :: ncid, nlocs_dimid, nchans_dimid
        real(r_kind), allocatable :: rtmp1d(:)

        allocate(rtmp1d(nlocs*nchans))

        call check(netcdfCreate(fname, ncid))
        call check(netcdfAddGroup(ncid, 'ObsValue'))
        call check(netcdfAddGroup(ncid, 'ObsError'))
        call check(netcdfAddGroup(ncid, 'PreQC'))
        call check(netcdfAddGroup(ncid, 'MetaData'))

        call check(netcdfAddDim(ncid, 'nlocs', nlocs, nlocs_dimid))
        call check(netcdfAddDim(ncid, 'nchans', nchans, nchans_dimid))

        ! Define variables
        call check(netcdfAddVar(ncid, "brightness_temperature", NF90_REAL, 2, ['nlocs ', 'nchans'], 'ObsValue', fillValue=missing_r))
        call check(netcdfPutAtt(ncid, 'units', 'K', "brightness_temperature", 'ObsValue'))
        call check(netcdfAddVar(ncid, "brightness_temperature", NF90_REAL, 2, ['nlocs ', 'nchans'], 'ObsError', fillValue=missing_r))
        call check(netcdfPutAtt(ncid, 'units', 'K', "brightness_temperature", 'ObsError'))
        call check(netcdfAddVar(ncid, "brightness_temperature", NF90_INT, 2, ['nlocs ', 'nchans'], 'PreQC', fillValue=missing_i))

        call check(netcdfAddVar(ncid, 'latitude', NF90_REAL, 1, ['nlocs'], 'MetaData', fillValue=missing_r))
        call check(netcdfAddVar(ncid, 'longitude', NF90_REAL, 1, ['nlocs'], 'MetaData', fillValue=missing_r))
        call check(netcdfAddVar(ncid, 'solar_azimuth_angle', NF90_REAL, 1, ['nlocs'], 'MetaData', fillValue=missing_r))
        call check(netcdfAddVar(ncid, 'scan_position', NF90_REAL, 1, ['nlocs'], 'MetaData', fillValue=missing_r))
        call check(netcdfAddVar(ncid, 'sensor_azimuth_angle', NF90_REAL, 1, ['nlocs'], 'MetaData', fillValue=missing_r))
        call check(netcdfAddVar(ncid, 'solar_zenith_angle', NF90_REAL, 1, ['nlocs'], 'MetaData', fillValue=missing_r))
        call check(netcdfAddVar(ncid, 'sensor_zenith_angle', NF90_REAL, 1, ['nlocs'], 'MetaData', fillValue=missing_r))
        call check(netcdfAddVar(ncid, 'sensor_view_angle', NF90_REAL, 1, ['nlocs'], 'MetaData', fillValue=missing_r))
        call check(netcdfAddVar(ncid, 'datetime', NF90_INT64, 1, ['nlocs'], 'MetaData', fillValue=missing_i))
        call check(netcdfAddVar(ncid, 'sensor_channel', NF90_INT, 1, ['nchans'], 'MetaData', fillValue=missing_i))

        call transpose_and_flatten(bt_out, rtmp1d)
        call check(netcdfPutVar(ncid, 'brightness_temperature', rtmp1d, 'ObsValue'))
        call transpose_and_flatten(err_out, rtmp1d)
        call check(netcdfPutVar(ncid, 'brightness_temperature', rtmp1d, 'ObsError'))
        call transpose_and_flatten(qf_out, rtmp1d)
        call check(netcdfPutVar(ncid, 'brightness_temperature', rtmp1d, 'PreQC'))

        call check(netcdfPutVar(ncid, 'latitude', lat_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'longitude', lon_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'solar_azimuth_angle', sun_azi_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'scan_position', scan_pos_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'sensor_azimuth_angle', sat_azi_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'solar_zenith_angle', sun_zen_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'sensor_zenith_angle', sat_zen_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'sensor_view_angle', sat_zen_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'sensor_channel', (/7,8,9,10,11,12,13,14,15,16/), 'MetaData'))
        call check(netcdfPutVar(ncid, 'datetime', datetime, 'MetaData'))
        call check(netcdfClose(ncid))
        deallocate(rtmp1d)
    end subroutine write_iodav3_netcdf
end module goes_abi_converter_mod
