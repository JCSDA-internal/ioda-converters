! Test that solar zenith angle is ~0° at equator on equinox at noon
subroutine test_equator_noon(name, status)
    use ieee_arithmetic, only : ieee_is_nan
    use ahi_HSD_mod, only : calc_solar_zenith_angle
    use kinds, only : r_single
    implicit none

    character(*), intent(in) :: name
    integer, intent(out) :: status
    real(r_single) :: solzen, expected
    real(r_single), parameter :: tol = 3.0_r_single

    call calc_solar_zenith_angle(0.0, 0.0, 12, 0, 79, solzen)
    expected = 0.0
    if (ieee_is_nan(solzen) .or. abs(solzen - expected) > tol) then
        status = 1
    else
        status = 0
    end if
    call calc_solar_zenith_angle(0.0, 0.0, 12, 0, 79, solzen)
    if (ieee_is_nan(solzen) .or. abs(solzen - expected) > tol) then
        status = 1
    else
        status = 0
    end if
end subroutine test_equator_noon

! Test that invalid hour input returns NaN
subroutine test_invalid_hour(name, status)
    use ieee_arithmetic, only : ieee_is_nan
    use ahi_HSD_mod, only : calc_solar_zenith_angle
    use kinds, only : r_single
    implicit none

    character(*), intent(in) :: name
    integer, intent(out) :: status
    real(r_single) :: solzen

    call calc_solar_zenith_angle(0.0, 0.0, 25, 0, 80, solzen)
    if (ieee_is_nan(solzen)) then
        status = 0
    else
        status = 1
    end if
end subroutine test_invalid_hour

! Test that invalid minute input returns NaN
subroutine test_invalid_minute(name, status)
    use ieee_arithmetic, only : ieee_is_nan
    use ahi_HSD_mod, only : calc_solar_zenith_angle
    use kinds, only : r_single
    implicit none

    character(*), intent(in) :: name
    integer, intent(out) :: status
    real(r_single) :: solzen

    call calc_solar_zenith_angle(0.0, 0.0, 0, 61, 80, solzen)
    if (ieee_is_nan(solzen)) then
        status = 0
    else
        status = 1
    end if
end subroutine test_invalid_minute

! Test that invalid Julian day input returns NaN
subroutine test_invalid_day(name, status)
    use ieee_arithmetic, only : ieee_is_nan
    use ahi_HSD_mod, only : calc_solar_zenith_angle
    use kinds, only : r_single
    implicit none

    character(*), intent(in) :: name
    integer, intent(out) :: status
    real(r_single) :: solzen

    call calc_solar_zenith_angle(0.0, 0.0, 0, 0, 367, solzen)
    if (ieee_is_nan(solzen)) then
        status = 0
    else
        status = 1
    end if
end subroutine test_invalid_day

! Test symmetry of solar zenith angle before and after noon
subroutine test_symmetric_noon(name, status)
    use ieee_arithmetic, only : ieee_is_nan
    use ahi_HSD_mod, only : calc_solar_zenith_angle
    use kinds, only : r_single
    implicit none

    character(*), intent(in) :: name
    integer, intent(out) :: status
    real(r_single), parameter :: tol = 3.0_r_single
    real(r_single) :: angle1, angle2

    call calc_solar_zenith_angle(0.0, 0.0, 11, 8, 80, angle1)
    call calc_solar_zenith_angle(0.0, 0.0, 13, 8, 79, angle2)
    if (ieee_is_nan(angle1) .or. ieee_is_nan(angle2) .or. abs(angle1 - angle2) > tol) then
        status = 1
    else
        status = 0
    end if
end subroutine test_symmetric_noon

! Driver program to run solar zenith angle unit tests
program calc_solar_zenith_angle_test
    implicit none
    integer :: i
    integer :: test_status(5)
    character(len = 20) :: test_names(5)

    test_names(1) = "Test Equator Noon"
    test_names(2) = "Test Invalid Hour"
    test_names(3) = "Test Symmetric Noon"
    test_names(4) = "Test Invalid Minute"
    test_names(5) = "Test Invalid Julian Day"
    test_status(:) = 0


    ! Test cases
    call test_equator_noon(test_names(1), test_status(1))
    call test_invalid_hour(test_names(2), test_status(2))
    call test_symmetric_noon(test_names(3), test_status(3))
    call test_invalid_minute(test_names(4), test_status(4))
    call test_invalid_day(test_names(5), test_status(5))

    do i = 1, size(test_status)
        if (test_status(i) /= 0) then
            write(*, *) trim(test_names(i)), " failed."
        else
            write(*, *) trim(test_names(i)), " passed."
        end if
    end do

    ! Call stop if any test failed
    if (any(test_status /= 0)) then
        call exit(1)
    end if

    write(*, *) "All tests passed successfully."
end program calc_solar_zenith_angle_test
