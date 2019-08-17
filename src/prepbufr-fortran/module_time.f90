module module_time
!
!  module: functions to read and write netcdf files
!
!  Ming Hu
!
! program history log:
!   2017-11-01 Hu           initial build
! 
! Subroutines Included:
!
  implicit none

  public :: mtime
! set default to private
  private
  type :: mtime
    contains
      procedure :: date2mins
      procedure :: elapseday
      procedure :: juliandaynum
      procedure :: julian2date
      procedure :: mins2date
  end type
!
contains

subroutine mins2date(this,mins,y,m,d,h,f)
! from minutes since 1970, January, 01 back to date
   implicit none
   class(mtime),intent(in) :: this
   integer,intent(in) :: mins
   integer,intent(out) :: y,m,d,h,f
   integer :: numday,wd,yd
   external :: elapseday

   numday=mins/1440
   f=mins-numday*1440
   h=f/60
   f=f-h*60

   call this%julian2date(numday,y,m,d,wd,yd)

end subroutine

function date2mins(this,y,m,d,h,f) result(mm)
! how many minutes since 1970, January, 01
   implicit none
   class(mtime),intent(in) :: this
   integer :: mm
   integer,intent(in) :: y,m,d,h,f
   integer :: numday
   external :: elapseday

   numday=this%elapseday(y,m,d)

   mm=numday*24*60+h*60+f

end function

function elapseday(this,y,m,d) result(numday)
! elapse days since 1970, January, 01
   implicit none
   class(mtime),intent(in) :: this
   integer :: numday
   integer,intent(in) :: y,m,d

   integer :: yy, mm

   mm = mod((m + 9) , 12)
   yy = y - mm/10
   numday=365*yy + yy/4 - yy/100 + yy/400 + (mm*306 + 5)/10 + ( d - 1 )

   numday=numday-719468

end function

FUNCTION juliandaynum(this, IYEAR,MONTH,IDAY) result(IW3JDN)
   implicit none
   class(mtime),intent(in) :: this
   integer :: IW3JDN
   integer,intent(in) :: IYEAR,MONTH,IDAY

       IW3JDN  =    IDAY - 32075                                         &
                  + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4        &
                  + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12       &
                  - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4
       IW3JDN  =    IW3JDN - 2440588  ! since 1970.01.01
       RETURN
END function

SUBROUTINE julian2date(this,JLDAYN,IYEAR,MONTH,IDAY,IDAYWK,IDAYYR)
! W3FS26
   implicit none
   class(mtime),intent(in) :: this
   integer,intent(out) :: JLDAYN,IYEAR,MONTH,IDAY,IDAYWK,IDAYYR
   integer :: L,N,I,J

       JLDAYN = JLDAYN + 2440588
       L      = JLDAYN + 68569
       N      = 4 * L / 146097
       L      = L - (146097 * N + 3) / 4
       I      = 4000 * (L + 1) / 1461001
       L      = L - 1461 * I / 4 + 31
       J      = 80 * L / 2447
       IDAY   = L - 2447 * J / 80
       L      = J / 11
       MONTH  = J + 2 - 12 * L
       IYEAR  = 100 * (N - 49) + I + L
       IDAYWK = MOD((JLDAYN + 1),7) + 1
       IDAYYR = JLDAYN -  &
       (-31739 +1461 * (IYEAR+4799) / 4 - 3 * ((IYEAR+4899)/100)/4)
       RETURN
END SUBROUTINE

end module module_time
