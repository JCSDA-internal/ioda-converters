module utils_mod

! adapated from WRFDA/var/da/da_tools/da_advance_time.inc

use kinds, only: r_kind,i_kind,r_double, i_llong

implicit none
private
public :: da_advance_time
public :: get_julian_time
public :: da_get_time_slots

contains

subroutine da_advance_time (date_in, dtime, date_out)

   ! HISTORY: 11/17/2008 modified and simplified from da_util/da_advance_time.f90
   !
   !   modified from da_advance_cymdh, 
   !   - has accuracy down to second,
   !   - can use day/hour/minute/second (with/without +/- sign) to advance time,
   !   - can digest various input date format if it still has the right order (ie. cc yy mm dd hh nn ss)
   !   - can digest flexible time increment 
   !
   !   eg.: da_advance_time 20070730      12             # advance 12 h 
   !        da_advance_time 2007073012   -1d2h30m30s     # back 1 day 2 hours 30 minutes and 30 seconds
   !        da_advance_time 2007073012    1s-3h30m       # back 3 hours 30 minutes less 1 second
   !

   implicit none

   character(len=*),  intent(in)            :: date_in, dtime
   character(len=14), intent(out)           :: date_out
  
   integer :: ccyy, mm, dd, hh, nn, ss, dday, dh, dn, ds, gday, gsec
   integer :: i, n
   character(len=14) :: ccyymmddhhnnss
   integer :: datelen

   ccyymmddhhnnss = parsedate(date_in)
   datelen = len_trim(ccyymmddhhnnss)

   if (datelen == 8) then
      read(ccyymmddhhnnss(1:10), fmt='(i4, 2i2)')  ccyy, mm, dd
      hh = 0
      nn = 0
      ss = 0
   else if (datelen == 10) then
      read(ccyymmddhhnnss(1:10), fmt='(i4, 3i2)')  ccyy, mm, dd, hh
      nn = 0
      ss = 0
   else if (datelen == 12) then
      read(ccyymmddhhnnss(1:12), fmt='(i4, 4i2)')  ccyy, mm, dd, hh, nn
      ss = 0
   else if (datelen == 14) then
      read(ccyymmddhhnnss(1:14), fmt='(i4, 5i2)')  ccyy, mm, dd, hh, nn, ss
   else
      stop 'wrong input date'
   endif

   if (.not. validdate(ccyy,mm,dd,hh,nn,ss)) then
      write(0,*) trim(ccyymmddhhnnss)
      stop 'Start date is not valid, or has wrong format'
   endif

   call parsedt(dtime,dday,dh,dn,ds)

   hh = hh + dh
   nn = nn + dn
   ss = ss + ds

   ! advance minute according to second
   do while (ss < 0) 
      ss = ss + 60
      nn = nn - 1
   end do
   do while (ss > 59) 
      ss = ss - 60
      nn = nn + 1
   end do

   ! advance hour according to minute
   do while (nn < 0) 
      nn = nn + 60
      hh = hh - 1
   end do
   do while (nn > 59) 
      nn = nn - 60
      hh = hh + 1
   end do

   ! advance day according to hour
   do while (hh < 0) 
      hh = hh + 24
      dday = dday - 1
   end do

   do while (hh > 23) 
      hh = hh - 24
      dday = dday + 1
   end do

   ! advance day if dday /= 0
   if (dday /= 0) call change_date ( ccyy, mm, dd, dday)

   write(ccyymmddhhnnss(1:14), fmt='(i4, 5i2.2)')  ccyy, mm, dd, hh, nn, ss
   !if (datelen<14) then
   !   if(nn /= 0) datelen=12
   !   if(ss /= 0) datelen=14
   !endif
   date_out = ccyymmddhhnnss

contains

subroutine change_date( ccyy, mm, dd, delta )

   implicit none

   integer, intent(inout) :: ccyy, mm, dd
   integer, intent(in)    :: delta

   integer, dimension(12) :: mmday
   integer                :: dday, direction

   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   mmday(2) = 28

   if (mod(ccyy,4) == 0) then
      mmday(2) = 29

      if (mod(ccyy,100) == 0) then
         mmday(2) = 28
      end if

      if (mod(ccyy,400) == 0) then
         mmday(2) = 29
      end if
   end if

   dday = abs(delta)
   direction = sign(1,delta)

   do while (dday > 0) 

      dd = dd + direction

      if (dd == 0) then
         mm = mm - 1

         if (mm == 0) then
            mm = 12
            ccyy = ccyy - 1
         end if

         dd = mmday(mm)
      elseif ( dd > mmday(mm)) then
         dd = 1
         mm = mm + 1
         if(mm > 12 ) then
            mm = 1
            ccyy = ccyy + 1
         end if
      end if

      dday = dday - 1

   end do
   return
end subroutine change_date

function parsedate(datein)
   character(len=*), intent(in) :: datein

   character(len=14) :: parsedate
   character(len=1 ) :: ch
   integer :: n, i
   parsedate = '00000000000000'
   i=0
   do n = 1, len_trim(datein)
      ch = datein(n:n)
      if (ch >= '0' .and. ch <= '9') then
         i=i+1
         parsedate(i:i)=ch
      end if
   end do
   if (parsedate(11:14) == '0000') then
      parsedate(11:14) = ''
   else if(parsedate(13:14) == '00') then
      parsedate(13:14) = ''
   end if
   return 
end function parsedate

subroutine parsedt(dt,dday,dh,dn,ds)
   character(len=*),  intent(in)    :: dt
   integer,           intent(inout) :: dday, dh, dn, ds

   character(len=1 ) :: ch
   integer :: n,i,d,s,nounit
   ! initialize time and sign
   nounit=1
   dday=0
   dh=0
   dn=0
   ds=0
   d=0
   s=1
   do n = 1, len_trim(dt)
      ch = dt(n:n)
      select case (ch)
         case ('0':'9')
           read(ch,fmt='(i1)') i
           d=d*10+i
         case ('-')
           s=-1
         case ('+')
           s=1
         case ('d')
           nounit=0
           dday=dday+d*s
           d=0
         case ('h')
           nounit=0
           dh=dh+d*s
           d=0
         case ('n','m')
           nounit=0
           dn=dn+d*s
           d=0
         case ('s')
           nounit=0
           ds=ds+d*s
           d=0
         case default
      end select
   end do
   if (nounit==1) dh=d*s
end subroutine parsedt

function isleapyear(year)
   ! check if year is leapyear
   integer,intent(in) :: year
   logical :: isleapyear
   if( mod(year,4) .ne. 0 ) then
     isleapyear=.FALSE.
   else
     isleapyear=.TRUE.
     if ( mod(year,100) == 0 .and. mod(year,400) .ne. 0 ) isleapyear=.FALSE.
   endif
end function isleapyear

function validdate(ccyy,mm,dd,hh,nn,ss)
   integer, intent(in) :: ccyy,mm,dd,hh,nn,ss

   logical :: validdate

   validdate = .true.

   if(ss > 59 .or. ss < 0 .or. &
      nn > 59 .or. nn < 0 .or. &
      hh > 23 .or. hh < 0 .or. &
                   dd < 1 .or. &
      mm > 12 .or. mm < 1 ) validdate = .false.

   if (mm == 2 .and. ( dd > 29 .or. &
                     ((.not. isleapyear(ccyy)) .and. dd > 28))) &
      validdate = .false.
end function validdate

end subroutine da_advance_time

subroutine get_julian_time(year,month,day,hour,minute,second,gstime,epoch)

! from WRFDA/var/da/da_tools/da_get_julian_time.inc

   implicit none

   integer(i_kind), intent(in)  :: year
   integer(i_kind), intent(in)  :: month
   integer(i_kind), intent(in)  :: day
   integer(i_kind), intent(in)  :: hour
   integer(i_kind), intent(in)  :: minute
   integer(i_kind), intent(in)  :: second
   real(r_double),  intent(out) :: gstime
   integer(i_llong), intent(out), optional :: epoch

   integer(i_kind) :: iw3jdn, ndays

   iw3jdn  =    day - 32075 &
              + 1461 * (year + 4800 + (month - 14) / 12) / 4 &
              + 367 * (month - 2 - (month - 14) / 12 * 12) / 12 &
              - 3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
   ndays = iw3jdn - 2443510

   gstime = ndays*1440.0 + hour*60.0 + minute*1.0

   if ( present(epoch) ) then
     ! since 1978-01-01
     epoch = (ndays*1440 + hour*60 + minute)*60 + second
     ! since 1970-01-01
     epoch = epoch + 252460800
   end if

end subroutine get_julian_time

subroutine da_get_time_slots(nt,tmin,tmax,time_slots)

! adapted from WRFDA/var/da/da_tools/da_get_time_slots.inc

   implicit none

   integer,           intent (in)      :: nt    ! number of time slots
   character(len=14), intent (in)      :: tmin  ! begin of time window
   character(len=14), intent (in)      :: tmax  ! end of time window
   real(r_double),    intent (out)     :: time_slots(0:nt)

   integer   :: min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
   integer   :: max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss
   real      :: dt
   integer   :: it

   read(unit=tmin,fmt='(i4,5i2)') min_yyyy, min_mm, min_dd, min_hh, min_mn, min_ss
   read(unit=tmax,fmt='(i4,5i2)') max_yyyy, max_mm, max_dd, max_hh, max_mn, max_ss

   call get_julian_time(min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss,time_slots(0))
   call get_julian_time(max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss,time_slots(nt))

   ! dt/2 between time_slots 0 and 1
   ! dt/2 between time_slots nt-1 and nt
   ! dt   between time_slots 1 and 2, 2 and 3, ..., nt-2 and nt-1
   if (nt > 1) then
      dt = (time_slots(nt)-time_slots(0))/float(nt-1)
      time_slots(1)  = time_slots(0)+dt*0.5
      do it=2,nt-1
         time_slots(it) = time_slots(it-1)+dt
      end do
   end if

end subroutine da_get_time_slots

end module utils_mod
