!> @brief Module for managing arrays of Fortran strings as C-compatible null-terminated string arrays.
!>
!> This module defines the `f_c_string_array_t` type for wrapping a Fortran string array
!> and converting it into an array of C-compatible null-terminated strings, suitable for
!> passing to C APIs. Each element is converted using `f_c_string_t`.
module f_c_string_array_t_mod
    use iso_c_binding, only : c_loc, c_ptr, c_null_char, c_char, c_f_pointer, c_null_ptr, &
            c_associated, c_size_t
    use f_c_string_t_mod, only : f_c_string_t, check_f_c_string
    implicit none

    private

    public :: f_c_string_array_t
    public :: check_f_c_string_array

    !> @brief Constructor interface for creating an `f_c_string_array_t` from a Fortran string array.
    interface f_c_string_array_t
        module procedure :: make_f_c_string_array
    end interface f_c_string_array_t

    !> @brief Interface for checking the status of operations or the validity of C string pointers.
    interface check_f_c_string_array
        module procedure :: check_status
        module procedure :: check_c_string_array
    end interface check_f_c_string_array

    !> @brief Type for wrapping and converting a Fortran string array to a C string array.
    type :: f_c_string_array_t
        private
        character(len = :), allocatable :: f_string_array(:)      !< Original Fortran strings
        type(f_c_string_t), allocatable :: f_c_string_t_array(:)  !< Internal C-compatible wrappers
        type(c_ptr), allocatable :: fc_string_array(:)            !< Array of C pointers to individual strings
        type(c_ptr) :: c_string_array                             !< Pointer to C array of string pointers
        logical :: initialized = .false.                          !< Whether the array was initialized
        logical :: c_memcpy = .false.                             !< Whether memory for C strings was allocated

    contains
        procedure, private :: init => init
        procedure :: to_c => to_c
        procedure :: get_c_string_array => get_c_string_array
        final :: cleanup
    end type f_c_string_array_t

contains

    !> @brief Factory function for constructing a `f_c_string_array_t` instance.
    !>
    !> Initializes the type and stores a copy of the input Fortran string array.
    !> @param f_string_array Array of Fortran strings.
    !> @return Initialized `f_c_string_array_t` instance.
    function make_f_c_string_array(f_string_array) result(instance)
        character(len = *), intent(in) :: f_string_array(:)
        integer :: status
        type(f_c_string_array_t) :: instance
        status = instance%init(f_string_array)
    end function make_f_c_string_array

    !> @brief Initializes the `f_c_string_array_t` instance with a Fortran string array.
    !>
    !> Stores the input array and marks the instance as initialized.
    !> @param this The object being initialized.
    !> @param f_string_array Fortran string array to store.
    !> @return Status code (0 on success, -1 on failure).
    function init(this, f_string_array) result(status)
        class(f_c_string_array_t), target, intent(inout) :: this
        character(len = *), intent(in) :: f_string_array(:)
        integer :: i, m
        integer :: status
        status = -1
        if (this%initialized) then
            return
        end if
        this%f_string_array = f_string_array
        this%initialized = .true.
        status = 0
    end function init

    !> @brief Converts the stored Fortran string array to a C-compatible array of null-terminated strings.
    !>
    !> Uses `f_c_string_t` to individually convert each element, then stores an array of C pointers.
    !> @param this The object holding the Fortran string array.
    !> @return Status code (0 on success, -1 on failure).
    function to_c(this) result(status)
        class(f_c_string_array_t), target, intent(inout) :: this
        integer :: i, m
        integer :: status
        type(f_c_string_t) f_c_string
        status = -1
        if (.not. this%initialized) then
            print *, "Error: f_c_string_array_t is not initialized."
            return
        end if
        m = size(this%f_string_array)
        allocate(this%f_c_string_t_array(m))
        allocate(this%fc_string_array(m))
        do i = 1, m
            this%f_c_string_t_array(i) = f_c_string_t(this%f_string_array(i))
            status = check_f_c_string(this%f_c_string_t_array(i)%to_c())
            this%fc_string_array(i) = check_f_c_string(this%f_c_string_t_array(i)%get_c_string())
        end do
        this%c_string_array = c_loc(this%fc_string_array)
        this%c_memcpy = .true.
        status = 0
    end function to_c

    !> @brief Returns the C pointer to the array of C string pointers.
    !>
    !> If the internal conversion has not been performed (`to_c()` not called), returns `c_null_ptr`.
    !> @param this The object holding the C-compatible strings.
    !> @return C pointer to the array of string pointers.
    function get_c_string_array(this) result(c_string_array)
        class(f_c_string_array_t), intent(inout) :: this
        type(c_ptr) :: c_string_array
        if (.not. this%c_memcpy) then
            c_string_array = c_null_ptr
            return
        end if
        c_string_array = this%c_string_array
    end function get_c_string_array

    !> @brief Finalization subroutine to clean up allocated memory.
    !>
    !> Deallocates the internal array of string wrappers and their corresponding C pointers.
    !> @param this The object to clean up.
    subroutine cleanup(this)
        type(f_c_string_array_t), intent(inout) :: this
        if (allocated(this%fc_string_array)) then
            deallocate(this%fc_string_array)
        end if
        if (allocated(this%f_c_string_t_array)) then
            deallocate(this%f_c_string_t_array)
        end if
    end subroutine cleanup

    !> @brief Validates a status code and exits the program if non-zero.
    !> @param status The status code to validate.
    !> @return Same status code if valid.
    function check_status(status)
        integer, intent(in) :: status
        integer :: check_status
        check_status = status
        if (status /= 0) then
            print *, "Error: f_c_string_array_t operation failed with status:", status
            call exit(1)
        end if
    end function check_status

    !> @brief Validates that a C pointer to a string array is associated.
    !>
    !> If the pointer is null, prints an error and exits.
    !> @param c_string_array Pointer to a C string array.
    !> @return Same pointer if valid.
    function check_c_string_array(c_string_array)
        type(c_ptr), intent(in) :: c_string_array
        type(c_ptr) :: check_c_string_array
        if (.not. c_associated(c_string_array)) then
            print *, "Error: C string array pointer is null."
            call exit(1)
            return
        end if
        check_c_string_array = c_string_array
    end function check_c_string_array

end module f_c_string_array_t_mod
