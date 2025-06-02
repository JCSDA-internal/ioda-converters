! Module providing functionality for handling a single Fortran string as a
! C-compatible null-terminated string.
module f_c_string_t_mod
    use iso_c_binding, only : c_loc, c_ptr, c_null_char, &
            c_char, c_f_pointer, c_null_ptr, c_size_t, c_associated
    implicit none

    public :: c_strlen

    public :: f_c_string_t

    interface
        ! Declares an interface for the C function `strlen` to calculate the length
        ! of a null-terminated C string.
        !
        ! Arguments:
        ! - c_string: A C pointer to the null-terminated string whose length is to be calculated.
        !
        ! Returns:
        ! - n: The length of the C string as an integer of kind `c_size_t`.
        function strlen(c_string) bind(C, name = "strlen") result(n)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: c_string
            integer(c_size_t) :: n
        end function strlen
    end interface

    ! Type to represent a single Fortran string that can be converted to/from
    ! a C-compatible null-terminated string.
    !
    ! Fields:
    ! - fc_string: Allocatable character string in C-compatible format
    !   (null-terminated).
    type :: f_c_string_t
        character(len = :, kind = c_char), allocatable :: fc_string

    contains

        procedure :: to_c => to_c
        procedure :: to_f => to_f
        final :: cleanup
    end type f_c_string_t

contains

    ! Computes the length of a null-terminated C string.
    !
    ! This function first checks if the input pointer is a null pointer. If the pointer
    ! is null, it returns `-1`. Otherwise, it calls the `strlen` wrapper for C's
    ! `strlen` function to calculate the length of the string.
    !
    ! Arguments:
    ! - c_string: A C pointer to the null-terminated string whose length is to be calculated.
    !
    ! Returns:
    ! - n: The length of the C string as an integer of kind `c_size_t`.
    !      Returns `-1` if `c_string` is a null pointer.
    function c_strlen(c_string) result(n)
        type(c_ptr), value :: c_string
        integer(c_size_t) :: n
        if (.not. c_associated(c_string)) then
            n = -1
            return
        end if
        n = strlen(c_string)
    end function c_strlen

    ! Converts a Fortran string to a C-compatible null-terminated string.
    !
    ! Arguments:
    ! - this: The instance of f_c_string_t being operated on.
    ! - f_string: The Fortran string to be converted.
    !
    ! Returns:
    ! - c_string: A C pointer to the null-terminated string in memory.
    function to_c(this, f_string) result(c_string)
        class(f_c_string_t), target, intent(inout) :: this
        character(len = *), intent(in) :: f_string
        type(c_ptr) :: c_string
        integer :: n
        n = len_trim(f_string)
        if (allocated(this%fc_string)) then
            deallocate(this%fc_string)
        end if
        allocate(character(len = n + 1) :: this%fc_string)
        this%fc_string = f_string(1:n) // c_null_char
        c_string = c_loc(this%fc_string)
    end function to_c

    ! Converts a C-compatible null-terminated string back into a Fortran string.
    !
    ! Arguments:
    ! - this: The instance of f_c_string_t being operated on.
    ! - c_string: A C pointer to the null-terminated string in memory.
    !
    ! Returns:
    ! - f_string: A Fortran string reconstructed from the C-compatible string.
    function to_f(this, c_string) result(f_string)
        class(f_c_string_t), intent(inout) :: this
        type(c_ptr), intent(in) :: c_string
        character(len = :), allocatable :: f_string
        character(len = 1, kind = c_char), pointer :: fc_string_pointer(:)
        integer(c_size_t) :: n

        n = c_strlen(c_string)
        if (n < 0) then
            f_string = ""
            return
        end if
        allocate(character(len = n) :: f_string)
        call c_f_pointer(c_string, fc_string_pointer, [n + 1])
        f_string = transfer(fc_string_pointer(1:n), f_string)
    end function to_f

    ! Automatically deallocates memory associated with the C-compatible
    ! null-terminated string when the instance of f_c_string_t goes out of scope
    ! or is explicitly deallocated.
    !
    ! Arguments:
    ! - this: The instance of f_c_string_t being finalized.
    subroutine cleanup(this)
        type(f_c_string_t), intent(inout) :: this
        if (allocated(this%fc_string)) then
            deallocate(this%fc_string)
        end if
    end subroutine cleanup

end module f_c_string_t_mod

