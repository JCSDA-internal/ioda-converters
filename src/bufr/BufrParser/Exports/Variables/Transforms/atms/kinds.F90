module kinds
!$$$  module documentation block
!                .      .    .                                       .
! module:   kinds
!   prgmmr: treadon          org: np23                date: 2004-08-15
!   modified by pnichols jcsda data : 2023-3-13
!
! abstract:  Module to hold specification kinds for variable declaration.
!            This module is based on (copied from) Paul vanDelst's 
!            type_kinds module found in the community radiative transfer
!            model
!
! module history log:
!   2004-08-15  treadon
!   2011-07-04  todling - define main precision during compilation
!
! Subroutines Included:
!
! Functions Included:
!
! remarks:
!   The numerical data types defined in this module are:
!      i_byte    - specification kind for byte (1-byte) integer variable
!      i_short   - specification kind for short (2-byte) integer variable
!      i_long    - specification kind for long (4-byte) integer variable
!      i_llong   - specification kind for double long (8-byte) integer variable
!      r_single  - specification kind for single precision (4-byte) real variable
!      r_double  - specification kind for double precision (8-byte) real variable
!      r_quad    - specification kind for quad precision (16-byte) real variable
!
!      i_kind    - generic specification kind for default integer
!      r_kind    - generic specification kind for default floating point
!
!
! attributes:
!   language: f90
!   iso fortran compliant compilers (all of the useful ones)
!
!$$$ end documentation block
  use,intrinsic :: iso_fortran_env
  implicit none
  private

! Integer type definitions below

! Integer types
  integer, parameter, public  :: i_byte  = int8
  integer, parameter, public  :: i_short = int16      ! short integer
  integer, parameter, public  :: i_int  = int32      ! long  integer
  integer, parameter, public  :: i_llong = int64

! Expected 8-bit byte sizes of the integer kinds
  integer, parameter, public :: num_bytes_for_i_byte  = 1
  integer, parameter, public :: num_bytes_for_i_short = 2
  integer, parameter, public :: num_bytes_for_i_int  = 4
  integer, parameter, public :: num_bytes_for_i_llong = 8

  integer, parameter, public :: i_kind = i_int
  integer, parameter, public :: num_bytes_for_i_kind = 4

! Real definitions below

! Real types
  integer, parameter, public  :: r_single = real32
  integer, parameter, public  :: r_double = real64
  integer, parameter, public  :: r_quad   = real128

! Expected 8-bit byte sizes of the real kinds
  integer, parameter, public :: num_bytes_for_r_single = 4
  integer, parameter, public :: num_bytes_for_r_double = 8
  integer, parameter, public :: num_bytes_for_r_quad   = 16

#ifdef _REAL64_
  integer, parameter, public :: r_kind = r_double
  integer, parameter, public :: num_bytes_for_r_kind = 8
#elif defined _REAL128_
  integer, parameter, public :: r_kind = r_quad
  integer, parameter, public :: num_bytes_for_r_kind = 16
#else
  integer, parameter, public :: r_kind = r_single
  integer, parameter, public :: num_bytes_for_r_kind = 4
#endif
!  integer, parameter, public :: dp = r_kind ! is this right ?
!  integer, parameter, public :: sp = r_single
!  integer, parameter, public :: spc = r_single
!  integer, parameter, public :: dpc = r_double
!  integer, parameter, public :: dpi = i_llong 
end module kinds
