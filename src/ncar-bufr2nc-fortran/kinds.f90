module kinds
!$$$  module documentation block
!                .      .    .                                       .
! module:   kinds
!   prgmmr: treadon          org: np23                date: 2004-08-15
!   prgmer: pnichols                                  data: 2023-03-09
!
! abstract:  Module to hold specification kinds for variable declaration.
!            This module is based on (copied from) Paul vanDelst's 
!            type_kinds module found in the community radiative transfer
!            model. Modifed to take
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
!      i_long    - specification kind for long (4/8-byte) integer variable
!      i_int     - specification kind for int (4 bytes) integer variable
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
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
!  use intrinsic fortran types
  use,intrinsic :: iso_fortran_env
  implicit none
  private

! Integer type definitions below

! Integer types
  integer, parameter, public  :: i_byte  = int8       ! one byte integer
  integer, parameter, public  :: i_short = int16      ! short integer
  integer, parameter, public  :: i_long  = int64      ! long  integer = 64 bit on LP64 systems
  integer, parameter, public  :: i_init = int32       ! 32 bit integer
  integer, parameter, private :: llong_t = int64      ! llong integer
  integer, parameter, public  :: i_llong = int64      ! long long

! Expected 8-bit byte sizes of the integer kinds
  integer, parameter, public :: num_bytes_for_i_byte  = 1
  integer, parameter, public :: num_bytes_for_i_short = 2
  integer, parameter, public :: num_bytes_for_i_int   = 4
  integer, parameter, public :: num_bytes_for_i_long  = 8
  integer, parameter, public :: num_bytes_for_i_llong = 8

! Define arrays for default definition
  integer, parameter, private :: num_i_kinds = 5
  integer, parameter, dimension( num_i_kinds ), private :: integer_types = (/ &
       i_byte, i_short, i_int, i_long,  i_llong  /) 
  integer, parameter, dimension( num_i_kinds ), private :: integer_byte_sizes = (/ &
       num_bytes_for_i_byte, num_bytes_for_i_short, num_bytes_for_i_int, &
       num_bytes_for_i_long, num_bytes_for_i_llong  /)

! Default values
! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT INTEGER TYPE KIND ***
  integer, parameter, private :: default_integer = 3  ! 1=byte, 
                                                      ! 2=short,
                                                      ! 3=int 
                                                      ! 4=long, 
                                                      ! 5=llong
  integer, parameter, public  :: i_kind = integer_types( default_integer )
  integer, parameter, public  :: num_bytes_for_i_kind = &
       integer_byte_sizes( default_integer )


! Real definitions below

! Real types
  integer, parameter, public  :: r_single = real32  ! single precision
  integer, parameter, public  :: r_double = real64 ! double precision
  integer, parameter, private :: quad_t   = real128 ! quad precision
  integer, parameter, public  :: r_quad   = real128

! Expected 8-bit byte sizes of the real kinds
  integer, parameter, public :: num_bytes_for_r_single = 4
  integer, parameter, public :: num_bytes_for_r_double = 8
  integer, parameter, public :: num_bytes_for_r_quad   = 16

! Define arrays for default definition
  integer, parameter, private :: num_r_kinds = 3
  integer, parameter, dimension( num_r_kinds ), private :: real_kinds = (/ &
       r_single, r_double, r_quad    /) 
  integer, parameter, dimension( num_r_kinds ), private :: real_byte_sizes = (/ &
       num_bytes_for_r_single, num_bytes_for_r_double, &
       num_bytes_for_r_quad    /)

! Default values
! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT REAL TYPE KIND ***
  integer, parameter, private :: default_real = 1  ! 1=single, 
  integer, parameter, public  :: r_kind = real_kinds( default_real )
  integer, parameter, public  :: num_bytes_for_r_kind = &
       real_byte_sizes( default_real )

end module kinds
