module goes_abi_converter_mod

    ! transpose_and_flatten:
    !   Transposes a 2D T matrix and flattens it into a 1D array.
    !
    !   Arguments:
    !     - mat (T, dimension(:,:), intent(in)):
    !       The input 2D matrix to be transposed and flattened.
    !     - flat_mat_trans (T, dimension(:), intent(out)):
    !       The output 1D array containing the flattened transpose of `mat`.
    !       Must be pre-allocated with size equal to size(mat,1) * size(mat,2).
    !
    !   Notes:
    !     - The subroutine performs an internal check to ensure `flat_mat_trans`
    !       has the correct size. If not, it prints an error message and stops execution.
    !     - The subroutine is generic and can handle both real and integer matrices.
    interface transpose_and_flatten
        module procedure transpose_and_flatten_real
        module procedure transpose_and_flatten_int
    end interface transpose_and_flatten

contains

    subroutine check(status)
        integer, intent(in) :: status
        integer, parameter :: success = 0
        if(status /= success) then
            print *, 'Error in output_iodav3. Error code = ', status
            call exit(1)
        end if
    end subroutine check

    ! See documentation for transpose_and_flatten interface
    subroutine transpose_and_flatten_real(mat, flat_mat_trans)
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
    end subroutine transpose_and_flatten_real

    ! See documentation for transpose_and_flatten interface
    subroutine transpose_and_flatten_int(mat, flat_mat_trans)
        implicit none
        integer, intent(in)  :: mat(:,:)
        integer, intent(out) :: flat_mat_trans(:)
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
    end subroutine transpose_and_flatten_int

    ! @brief Sets the output filename for GOES ABI data based on satellite ID and time.
    !
    ! This subroutine generates the output filename for GOES ABI data by combining the
    ! satellite ID and the start time. The satellite ID is converted to lowercase,
    ! and the start time is formatted into the filename as "yyyyMMddhh_mm". The filename
    ! follows the format:
    !
    !   abi_<sat_id_lower>_obs_<time_str>.h5
    !
    ! @param fname (inout) The output filename. It will be updated with the generated
    !        filename based on the satellite ID and time.
    ! @param sat_id (in) The satellite ID (e.g., "G16") to be used in the filename.
    ! @param time_start (in) The start time in ISO 8601 format (e.g., "2018-04-15T00:00:41.9Z").
    !
    ! @note The satellite ID is converted to lowercase and only the year, month, day,
    !       and hour from the `time_start` string are used in the generated filename.
    !       The minute value is extracted from `time_start` and included in the filename.
    subroutine set_goes_abi_out_fname(fname, sat_id, time_start)
        use utils_mod, only : to_lower
        implicit none
        character(len=*), intent(inout) :: fname
        character(len=*), intent(in) :: sat_id, time_start
        character(len=:), allocatable :: sat_id_lower
        integer :: iyear, imonth, iday, ihour, imin
        character(len=32) :: time_str

        ! Set the output filename based on satellite ID and time string
        read(time_start( 1: 4), '(i4)') iyear
        read(time_start( 6: 7), '(i2)') imonth
        read(time_start( 9:10), '(i2)') iday
        read(time_start(12:13), '(i2)') ihour
        write(time_str, '(i4.4, i2.2, i2.2, i2.2)') iyear, imonth, iday, ihour
        time_str = trim(time_str) // '_' // time_start(15:16)
        sat_id_lower = to_lower(trim(sat_id))
        fname = 'abi_' // trim(sat_id_lower) // '_obs_' // trim(time_str)  // '.h5'
    end subroutine set_goes_abi_out_fname

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
    !     - qf_out (integer(i_kind), dimension(nchans, nlocs), intent(in)):
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
        integer(i_kind), intent(in) :: qf_out(nchans, nlocs)

        integer :: ncid, nlocs_dimid, nchans_dimid
        real(r_kind), allocatable :: rtmp1d(:)
        integer(i_kind), allocatable :: itmp1d(:)

        allocate(rtmp1d(nlocs*nchans))
        allocate(itmp1d(nlocs*nchans))

        call check(netcdfCreate(fname, ncid))
        call check(netcdfAddGroup(ncid, 'ObsValue'))
        call check(netcdfAddGroup(ncid, 'ObsError'))
        call check(netcdfAddGroup(ncid, 'PreQC'))
        call check(netcdfAddGroup(ncid, 'MetaData'))

        call check(netcdfAddDim(ncid, 'nlocs', nlocs, nlocs_dimid))
        call check(netcdfAddVar(ncid, 'nlocs', NF90_INT, 1, ['nlocs']))

        call check(netcdfAddDim(ncid, 'nchans', nchans, nchans_dimid))
        call check(netcdfAddVar(ncid, 'nchans', NF90_INT, 1, ['nchans']))

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
        call check(netcdfAddVar(ncid, 'dateTime', NF90_INT64, 1, ['nlocs'], 'MetaData'))
        call check(netcdfPutAtt(ncid, "units", "seconds since 1970-01-01T00:00:00Z", 'dateTime', 'MetaData'))
        call check(netcdfAddVar(ncid, 'sensor_channel', NF90_INT, 1, ['nchans'], 'MetaData', fillValue=missing_i))

        call check(netcdfPutVar(ncid, 'nchans', (/7,8,9,10,11,12,13,14,15,16/)))

        call transpose_and_flatten(bt_out, rtmp1d)
        call check(netcdfPutVar(ncid, 'brightness_temperature', rtmp1d, 'ObsValue'))
        call transpose_and_flatten(err_out, rtmp1d)
        call check(netcdfPutVar(ncid, 'brightness_temperature', rtmp1d, 'ObsError'))
        call transpose_and_flatten(qf_out, itmp1d)
        call check(netcdfPutVar(ncid, 'brightness_temperature', itmp1d, 'PreQC'))

        call check(netcdfPutVar(ncid, 'latitude', lat_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'longitude', lon_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'solar_azimuth_angle', sun_azi_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'scan_position', scan_pos_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'sensor_azimuth_angle', sat_azi_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'solar_zenith_angle', sun_zen_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'sensor_zenith_angle', sat_zen_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'sensor_view_angle', sat_zen_out, 'MetaData'))
        call check(netcdfPutVar(ncid, 'sensor_channel', (/7,8,9,10,11,12,13,14,15,16/), 'MetaData'))
        call check(netcdfPutVar(ncid, 'dateTime', datetime, 'MetaData'))
        call check(netcdfClose(ncid))
        deallocate(rtmp1d)
    end subroutine write_iodav3_netcdf
end module goes_abi_converter_mod
