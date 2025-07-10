!> @brief Unit test for `f_c_string_t` conversion to C-compatible null-terminated strings.
!>
!> This test verifies that Fortran strings can be safely converted to C strings
!> using the `f_c_string_t` module, and that the resulting strings match
!> expected values in C.
!>
!> The test passes two strings (`"abc"` and `"abc def"`) to C functions via
!> C pointers and checks that the C++ side sees the expected strings.
!>
!> Exits with a non-zero code if any test fails.
program f_c_string_t_test
    use iso_c_binding, only : c_ptr, c_associated
    use f_c_string_t_mod
    implicit none

    !> @brief C interface to a function that checks if a C string equals "abc".
    interface
        function str_equals_abc(c_string) bind(C, name = "str_equals_abc")
            import :: c_ptr
            type(c_ptr), value :: c_string
            integer :: f_c_string_cpp_test
        end function str_equals_abc

        !> @brief C interface to a function that checks if a C string equals "abc def".
        function str_equals_abc_space_def(c_string) bind(C, name = "str_equals_abc_space_def")
            import :: c_ptr
            type(c_ptr), value :: c_string
            integer :: f_c_string_cpp_test
        end function str_equals_abc_space_def
    end interface

    type(f_c_string_t) :: abc, abc_def
    integer :: status

    !> @test Convert "abc" to C string and verify with C++ side
    abc = f_c_string_t("abc")
    status = check_f_c_string(abc%to_c())
    status = str_equals_abc(check_f_c_string(abc%get_c_string()))
    if (status /= 0) then
        print *, "Error: C string does not equal 'abc'. Status:", status
        stop 1
    else
        print *, "Success: C string equals 'abc'."
    end if

    !> @test Convert "abc def" to C string and verify with C++ side
    abc_def = f_c_string_t("abc def")
    status = check_f_c_string(abc_def%to_c())
    status = str_equals_abc_space_def(check_f_c_string(abc_def%get_c_string()))
    if (status /= 0) then
        print *, "Error: C string does not equal 'abc def'. Status:", status
        stop 1
    else
        print *, "Success: C string equals 'abc def'."
    end if
end program f_c_string_t_test
