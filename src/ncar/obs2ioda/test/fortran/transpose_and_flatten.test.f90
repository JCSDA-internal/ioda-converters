!> @brief Test program for transpose_and_flatten subroutine.
!>
!> This test verifies that the transpose_and_flatten subroutine correctly
!> transposes a 2D matrix and flattens the result into a 1D array.
!>
!> The input matrix is:
!>     [1.0 3.0 5.0]
!>     [2.0 4.0 6.0]
!> represented in Fortran as mat(2,3) with column-major order.
!>
!> The transpose of mat is:
!>     [1.0 2.0]
!>     [3.0 4.0]
!>     [5.0 6.0]
!>
!> The expected flattened result is: [1.0, 3.0, 5.0, 2.0, 4.0, 6.0]

program transpose_and_flatten_test
    use goes_abi_converter_mod, only: transpose_and_flatten
    implicit none

    real, dimension(2, 3) :: mat
    real, dimension(6) :: flat, expected
    integer :: i

    ! Initialize test matrix: columns are [1 2], [3 4], [5 6]
    mat = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], shape(mat))

    ! Expected result after transpose and flatten
    expected = [1.0, 3.0, 5.0, 2.0, 4.0, 6.0]

    ! Call the subroutine
    call transpose_and_flatten(mat, flat)

    ! Compare result
    do i = 1, size(flat)
        if (abs(flat(i) - expected(i)) > 1e-6) then
            print *, "Test failed at index ", i
            print *, "Expected:", expected(i), "Got:", flat(i)
            stop 1
        end if
    end do

    print *, "Test passed."
end program transpose_and_flatten_test
