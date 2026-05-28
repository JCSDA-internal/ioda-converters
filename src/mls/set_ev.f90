subroutine set_ev(c_env, c_value)

    use iso_c_binding, only: c_char, c_int, c_null_char
    implicit none

    character(len=*) :: c_env
    character(len=*) :: c_value
    
    ! Define the interface to the C standard library 'setenv' function
    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(c, name="setenv")
            import :: c_char, c_int
            ! Strings passed to C must be arrays of C characters
            character(kind=c_char), dimension(*), intent(in) :: name
            character(kind=c_char), dimension(*), intent(in) :: value
            ! Passed by value in C
            integer(c_int), value, intent(in) :: overwrite
        end function c_setenv
    end interface

    integer(c_int) :: ierr

    ! Call the C function. 
    ! IMPORTANT: We must append C_NULL_CHAR to terminate the strings for C.
    ! The third argument '1_c_int' tells C to overwrite it if it already exists.
    ierr = c_setenv(TRIM(c_env) // c_null_char, trim(adjustl(c_value)) // c_null_char, 1_c_int)

    if (ierr == 0) then
        print *, "Successfully set ",TRIM(c_env),"=",TRIM(ADJUSTL(c_value))
    else
        print *, "Failed to set environment variable ",TRIM(c_env)
    end if

end subroutine set_ev
