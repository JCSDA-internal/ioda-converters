! @brief Unit test for the `to_lower` function.
!
! This program tests the `to_lower` function, which converts a string to lowercase.
! It checks if the function correctly converts input strings to lowercase for various cases:
! - Test 1: Converting "HELLO WORLD" to "hello world".
! - Test 2: Converting "Test123" to "test123".
!
! Each test compares the output of the `to_lower` function with the expected result.
! If the output matches the expected value, the test is marked as passed. Otherwise,
! it is marked as failed, and the program stops with an error message.
program to_lower_test
    use utils_mod, only: to_lower
    implicit none

    character(len=100) :: input_str, output_str, expected
    integer :: i

    ! Test 1
    input_str = "HELLO WORLD"
    expected = "hello world"
    output_str = to_lower(input_str)

    ! Check result for Test 1
    if (output_str == expected) then
        print *, "Test 1 PASSED"
    else
        print *, "Test 1 FAILED"
        print *, "  Expected: ", expected
        print *, "  Got:      ", output_str
        stop 1
    end if

    input_str = "Test123"
    expected = "test123"
    output_str = to_lower(input_str)

    if (output_str == expected) then
        print *, "Test 2 PASSED"
    else
        print *, "Test 2 FAILED"
        print *, "  Expected: ", expected
        print *, "  Got:      ", output_str
        stop 1
    end if
end program to_lower_test
