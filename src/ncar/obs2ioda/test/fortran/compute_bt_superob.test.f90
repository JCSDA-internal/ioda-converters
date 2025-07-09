program test_compute_bt_superob
    use ahi_HSD_mod, only: compute_bt_superob
    use kinds, only: r_double, r_kind
    implicit none


    ! Local variables
    real(r_kind) :: result, expected
    real(r_kind), allocatable :: bt(:,:)
    integer :: status

    status = 0

    ! Test 1: all valid values
    allocate(bt(2,2))
    bt = reshape([280.0, 282.0, 284.0, 286.0], shape(bt))
    result = compute_bt_superob(bt, -999.0_r_kind)
    expected = 283.0
    if (abs(result - expected) > 1.0e-6_r_kind) then
        write(*,*) "Test 1 failed: Expected", expected, "Got:", result
        stop 1
    end if

    ! Test 2: some values <= 0.0
    bt = reshape([280.0, -1.0, 0.0, 285.0], shape(bt))
    result = compute_bt_superob(bt, -999.0_r_kind)
    expected = (280.0 + 285.0) / 2.0
    if (abs(result - expected) > 1.0e-6_r_kind) then
        write(*,*) "Test 2 failed: Expected", expected, "Got:", result
        stop 1
    end if

    ! Test 3: all values invalid
    bt = reshape([-10.0, 0.0, -5.0, 0.0], shape(bt))
    result = compute_bt_superob(bt, 275.0)
    expected = 275.0
    if (abs(result - expected) > 1.0e-6_r_kind) then
        write(*,*) "Test 3 failed: Expected", expected, "Got:", result
        stop 1
    end if
end program test_compute_bt_superob
