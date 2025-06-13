! @brief Unit test for the `set_goes_abi_out_fname` subroutine.
!
! This program tests the `set_goes_abi_out_fname` subroutine, which generates
! the output filename based on satellite ID and the provided start time.
program set_goes_abi_out_fname_test
    use goes_abi_converter_mod, only: set_goes_abi_out_fname
    implicit none

    character(len=256) :: fname
    character(len=10) :: sat_id
    character(len=22) :: time_start
    character(len=256) :: expected_fname
    integer :: i

    ! Test 1 - Regular satellite ID and start time
    sat_id = "G16"
    time_start = "2018-04-15T00:00:41.9Z"
    expected_fname = "abi_g16_obs_2018041500_00.h5"
    call set_goes_abi_out_fname(fname, sat_id, time_start)
    if (.not. fname == expected_fname) then
        print *, " FAILED"
        print *, "  Expected: ", expected_fname
        print *, "  Got:      ", fname
        stop 1
    end if

    ! Test 2 - Regular satellite ID and start time with different minutes
    sat_id = "G16"
    time_start = "2018-04-15T00:15:41.9Z"
    expected_fname = "abi_g16_obs_2018041500_15.h5"
    call set_goes_abi_out_fname(fname, sat_id, time_start)
    if (.not. fname == expected_fname) then
        print *, " FAILED"
        print *, "  Expected: ", expected_fname
        print *, "  Got:      ", fname
        stop 1
    end if

    ! Test 3 - Different satellite ID
    sat_id = "G6"
    time_start = "2018-04-15T00:15:41.9Z"
    expected_fname = "abi_g6_obs_2018041500_15.h5"
    call set_goes_abi_out_fname(fname, sat_id, time_start)
    if (.not. fname == expected_fname) then
        print *, " FAILED"
        print *, "  Expected: ", expected_fname
        print *, "  Got:      ", fname
        stop 1
    end if
end program set_goes_abi_out_fname_test
