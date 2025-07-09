! get_julian_time_test:
!   Unit test for the `get_julian_time` subroutine in utils_mod.
!
!   Description:
!     This program verifies that the `get_julian_time` subroutine correctly computes
!     64-bit integer epoch time values corresponding to known Gregorian date-times.
!     It checks two test cases:
!       - 2025-01-01 00:00:00 → 1735689600
!       - 2025-01-01 00:10:00 → 1735690200
!
!   Notes:
!     - Fails with a non-zero exit code if any computed epoch_time differs from the
!       expected reference value.
!     - Output messages indicate the failing index and the expected vs. actual values.
!     - This test assumes the epoch_time output represents seconds since
!       1970-01-01T00:00:00Z (Unix epoch).
program get_julian_time_test
    use kinds, only : i_kind, i_llong, r_double
    use utils_mod, only : get_julian_time
    implicit none
    integer(i_llong), dimension(2) :: epoch_time
    real(r_double), dimension(2) :: gstime
    integer(i_llong), dimension(2) :: ref_epoch_time
    integer(i_kind), dimension(2) :: year, month, day, hour, minute, second
    integer :: i
    ref_epoch_time = [1735689600_i_llong, 1735690200_i_llong]  ! Reference epochs for 2025-01-01 00:00:00 and 2025-01-01 00:10:00
    year = [2025_i_kind, 2025_i_kind]
    month = [1_i_kind, 1_i_kind]
    day = [1_i_kind, 1_i_kind]
    hour = [0_i_kind, 0_i_kind]
    minute = [0_i_kind, 10_i_kind]
    second = [0_i_kind, 0_i_kind]
    call get_julian_time(year(1), month(1), day(1), &
            hour(1), minute(1), second(1), gstime(1), epoch_time(1))
    call get_julian_time(year(2), month(2), day(2), &
            hour(2), minute(2), second(2), gstime(2), epoch_time(2))
    do i = 1, 2
        if (epoch_time(i) /= ref_epoch_time(i)) then
            print *, "Test failed for epoch_time ", i
            print *, "Expected:", ref_epoch_time(i), "Got:", epoch_time(i)
            stop 1
        end if
    end do
end program
