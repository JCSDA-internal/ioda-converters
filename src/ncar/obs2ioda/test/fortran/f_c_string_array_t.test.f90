!> @brief Unit test for `f_c_string_array_t`, which converts Fortran string arrays
!> to C-compatible arrays of null-terminated strings.
!>
!> This test constructs two Fortran string arrays and verifies that they are correctly
!> converted to C-style `char**` arrays using the `f_c_string_array_t` type. The results
!> are passed to C++ functions to validate correct conversion and null-termination.
!>
!> The following test cases are covered:
!> - Single string: ["abc"]
!> - Multi-string array: ["abc", "def", "ghi"]
!>
!> The test will exit with status code 1 if any test fails.
program f_c_string_array_t_test
    use iso_c_binding, only : c_ptr, c_associated
    use f_c_string_array_t_mod
    implicit none

    !> @brief C interface for checking if the first string in the array is "abc".
    interface
        function str_array_equals_abc(c_string_array) bind(C, name = "str_array_equals_abc")
            import :: c_ptr
            type(c_ptr), value :: c_string_array
            integer :: str_array_equals_abc
        end function str_array_equals_abc

        !> @brief C interface for checking if the string array is ["abc", "def", "ghi"].
        function str_array_equals_abc_def_ghi(c_string_array) bind(C, name = "str_array_equals_abc_def_ghi")
            import :: c_ptr
            type(c_ptr), value :: c_string_array
            integer :: str_array_equals_abc_def_ghi
        end function str_array_equals_abc_def_ghi
    end interface

    type(f_c_string_array_t) :: abc, abc_def_ghi
    integer :: status

    !> @test Convert ["abc"] to a C string array and validate with C++.
    abc = f_c_string_array_t(["abc"])
    status = check_f_c_string_array(abc%to_c())
    status = str_array_equals_abc(check_f_c_string_array(abc%get_c_string_array()))
    if (status /= 0) then
        print *, "Error: C string array does not equal 'abc'. Status:", status
        stop 1
    else
        print *, "Success: C string array equals 'abc'."
    end if

    !> @test Convert ["abc", "def", "ghi"] to a C string array and validate with C++.
    abc_def_ghi = f_c_string_array_t(["abc", "def", "ghi"])
    status = check_f_c_string_array(abc_def_ghi%to_c())
    status = str_array_equals_abc_def_ghi(check_f_c_string_array(abc_def_ghi%get_c_string_array()))
    if (status /= 0) then
        print *, "Error: C string array does not equal 'abc', 'def', 'ghi'. Status:", status
        stop 1
    else
        print *, "Success: C string array equals 'abc', 'def', 'ghi'."
    end if

end program f_c_string_array_t_test
