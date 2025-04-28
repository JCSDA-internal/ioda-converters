! Module providing functionality for handling one-dimensional arrays of
! Fortran strings as C-compatible null-terminated strings.
module f_c_string_1D_t_mod
    use iso_c_binding, only : c_loc, c_ptr, c_null_char, c_char, c_f_pointer, c_null_ptr, &
            c_associated, c_size_t
    use f_c_string_t_mod, only : f_c_string_t, c_strlen
    implicit none

    ! Type to handle a 1D array of Fortran strings and convert them to/from
    ! C-compatible null-terminated strings. A given instance of this type
    ! should only be used for related data transfer operations, and should
    ! not be used for multiple unrelated data transfers.
    !
    ! Array of f_c_string_t, each representing a null-terminated C string.
    ! type(f_c_string_t), allocatable :: f_c_string_t_array(:)
    !
    ! Array of C pointers, each pointing to a null-terminated string in memory.
    ! type(c_ptr), allocatable :: fc_string_1D(:)
    type :: f_c_string_1D_t
        ! Allocatable C-compatible null-terminated string
        type(f_c_string_t), allocatable :: f_c_string_t_array(:)
        type(c_ptr), allocatable :: fc_string_1D(:)

    contains

        procedure :: to_c => to_c
        procedure :: to_f => to_f
        final :: cleanup
    end type f_c_string_1D_t

contains

    ! Converts a Fortran array of strings to a C-compatible null-terminated
    ! string array and stores it internally as `fc_string_1D`.
    !
    ! Arguments:
    ! - this: The instance of f_c_string_1D_t being operated on.
    ! - f_string_1D: A 1D Fortran array of character strings.
    !
    ! Returns:
    ! - c_string_1D: A C pointer to an array of C pointers, where
    !       each pointer in the array points to a C string.
    function to_c(this, f_string_1D) result(c_string_1D)
        class(f_c_string_1D_t), target, intent(inout) :: this
        character(len = *), intent(in) :: f_string_1D(:)
        type(c_ptr) :: c_string_1D
        integer :: i, m
        m = size(f_string_1D)
        if (allocated(this%fc_string_1D)) then
            deallocate(this%fc_string_1D)
        end if
        if (allocated(this%f_c_string_t_array)) then
            deallocate(this%f_c_string_t_array)
        end if
        allocate(this%f_c_string_t_array(m))
        allocate(this%fc_string_1D(m))
        do i = 1, m
            this%fc_string_1D(i) = this%f_c_string_t_array(i)%to_c(f_string_1D(i))
        end do
        c_string_1D = c_loc(this%fc_string_1D)
    end function to_c

    ! Converts a C-compatible null-terminated string array back into a
    ! Fortran array of strings.
    !
    ! Arguments:
    ! - this: The instance of f_c_string_1D_t being operated on.
    ! - c_string_1D: A C pointer to an array of C pointers, where
    !       each pointer in the array points to a C string.
    ! - m: The number of strings in the array.
    !
    ! Returns:
    ! - f_string_1D: A Fortran array of strings reconstructed from the C
    !   string array.
    function to_f(this, c_string_1D, m) result(f_string_1D)
        class(f_c_string_1D_t), intent(inout) :: this
        type(c_ptr), intent(in) :: c_string_1D
        integer, intent(in) :: m
        character(len = :), allocatable :: f_string_1D(:)
        type(c_ptr), pointer :: fc_string_1D_pointer(:)
        integer :: i
        integer(c_size_t) :: n
        if (m < 0 .or. .not. c_associated(c_string_1D)) then
            f_string_1D = [""]
            return
        end if
        if (allocated(this%f_c_string_t_array)) then
            if (size(this%f_c_string_t_array) /= m) then
                deallocate(this%f_c_string_t_array)
                allocate(this%f_c_string_t_array(m))
            end if
        else
            allocate(this%f_c_string_t_array(m))
        end if
        call c_f_pointer(c_string_1D, fc_string_1D_pointer, [m])
        n = 0
        do i = 1, m
            n = max(n, c_strlen(fc_string_1D_pointer(i)))
        end do
        allocate(character(len = n) :: f_string_1D(1:m))
        do i = 1, m
            f_string_1D(i) = this%f_c_string_t_array(i)%to_f(fc_string_1D_pointer(i))
        end do

    end function to_f

    ! Automatically deallocates memory associated with the instance of
    ! f_c_string_1D_t when it goes out of scope or is explicitly deallocated.
    !
    ! Arguments:
    ! - this: The instance of f_c_string_1D_t being finalized.
    subroutine cleanup(this)
        type(f_c_string_1D_t), intent(inout) :: this
        if (allocated(this%fc_string_1D)) then
            deallocate(this%fc_string_1D)
        end if
        if (allocated(this%f_c_string_t_array)) then
            deallocate(this%f_c_string_t_array)
        end if
    end subroutine cleanup

end module f_c_string_1D_t_mod
