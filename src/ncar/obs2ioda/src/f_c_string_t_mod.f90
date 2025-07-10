!> @brief Module for converting a single Fortran string to a C-compatible null-terminated string.
!>
!> Provides a derived type `f_c_string_t` to wrap a Fortran string and manage its conversion
!> to a C string via `iso_c_binding`. Includes initialization, C conversion, pointer access,
!> and cleanup.
module f_c_string_t_mod
    use iso_c_binding, only : c_loc, c_ptr, c_null_char, &
            c_char, c_f_pointer, c_null_ptr, c_size_t, c_associated
    implicit none
    private

    public :: f_c_string_t
    public :: check_f_c_string

    !> @brief Interface for constructing a new `f_c_string_t` from a Fortran string.
    interface f_c_string_t
        module procedure :: make_f_c_string
    end interface f_c_string_t

    !> @brief Interface for checking status codes and C string pointers.
    interface check_f_c_string
        module procedure :: check_status
        module procedure :: check_c_string
    end interface check_f_c_string

    !> @brief Derived type that wraps a Fortran string and provides C-compatible access.
    type :: f_c_string_t
        character(len = :), allocatable, private :: f_string !< Original Fortran string
        type(c_ptr), private :: c_string                     !< Pointer to null-terminated C string
        character(len = :, kind = c_char), allocatable, private :: fc_string !< Allocated null-terminated character buffer
        logical, private :: initialized = .false.            !< Flag indicating if initialized
        logical, private :: c_memcpy = .false.               !< Flag indicating if memory for C conversion was allocated

    contains

        procedure, private :: init => init
        procedure :: to_c => to_c
        procedure :: get_c_string => get_c_string
        final :: cleanup
    end type f_c_string_t

contains

    !> @brief Constructor function for creating an `f_c_string_t` instance.
    !> @param f_string Fortran string to wrap.
    !> @return A new instance of `f_c_string_t`.
    function make_f_c_string(f_string) result(instance)
        character(len = *), intent(in) :: f_string
        integer :: status
        type(f_c_string_t) :: instance
        status = instance%init(f_string)
    end function make_f_c_string

    !> @brief Initializes the internal state of the object from a Fortran string.
    !> Trims the string and marks the object as initialized.
    !> @param this Object being initialized.
    !> @param f_string Input Fortran string.
    !> @return Status code (0 on success, -1 on failure).
    function init(this, f_string) result(status)
        class(f_c_string_t), target, intent(inout) :: this
        character(len = *), intent(in) :: f_string
        integer :: n
        integer :: status
        status = -1
        if (this%initialized) then
            return
        end if
        n = len_trim(f_string)
        this%f_string = f_string(1:n)
        this%initialized = .true.
        status = 0
    end function init

    !> @brief Converts the Fortran string to a C-compatible null-terminated string.
    !> Allocates a `character(kind=c_char)` array with a null terminator and stores a C pointer.
    !> @param this Object containing the Fortran string.
    !> @return Status code (0 on success, -1 on failure).
    function to_c(this) result(status)
        class(f_c_string_t), target, intent(inout) :: this
        type(c_ptr) :: c_string
        integer :: n
        integer :: status
        status = -1
        if (.not. this%initialized) then
            return
        end if
        n = len_trim(this%f_string)
        allocate(character(len = n + 1, kind = c_char) :: this%fc_string)
        this%fc_string = this%f_string(1:n) // c_null_char
        this%c_string = c_loc(this%fc_string)
        this%c_memcpy = .true.
        status = 0
    end function to_c

    !> @brief Returns the pointer to the null-terminated C string.
    !> @param this Object holding the C string.
    !> @return Pointer to C string, or `c_null_ptr` if `to_c` was not called.
    function get_c_string(this) result(c_string)
        class(f_c_string_t), intent(inout) :: this
        type(c_ptr) :: c_string
        if (.not. this%c_memcpy) then
            c_string = c_null_ptr
            return
        end if
        c_string = this%c_string
    end function get_c_string

    !> @brief Finalization procedure to deallocate the internal C string buffer.
    !> @param this Object being finalized.
    subroutine cleanup(this)
        type(f_c_string_t), intent(inout) :: this
        if (allocated(this%fc_string)) then
            deallocate(this%fc_string)
        end if
    end subroutine cleanup

    !> @brief Checks an integer status code and exits if it is non-zero.
    !> @param status The status code to check.
    !> @return Returns the same status for chaining.
    function check_status(status)
        integer, intent(in) :: status
        integer :: check_status
        check_status = status
        if (status /= 0) then
            print *, "Error: f_c_string_t operation failed with status:", status
            call exit(1)
        end if
    end function check_status

    !> @brief Checks if a C pointer is associated (not null).
    !> Exits the program if the pointer is null.
    !> @param c_string The C string pointer to validate.
    !> @return Same pointer if valid.
    function check_c_string(c_string)
        type(c_ptr), intent(in) :: c_string
        type(c_ptr) :: check_c_string
        if (.not. c_associated(c_string)) then
            print *, "Error: C string pointer is null."
            call exit(1)
            return
        end if
        check_c_string = c_string
    end function check_c_string

end module f_c_string_t_mod
