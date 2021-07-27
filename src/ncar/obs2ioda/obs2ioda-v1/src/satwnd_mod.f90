module satwnd_mod

use kinds, only: r_kind, i_kind, r_double
use define_mod, only: nobtype, set_obtype_conv, obtype_list, xdata, &
   nvar_met, nvar_info, type_var_info, name_var_met, name_var_info, &
   t_kelvin, missing_r, missing_i, vflag, itrue, ifalse, nstring, ndatetime, not_use
use ufo_vars_mod, only: ufo_vars_getindex, var_prs, var_u, var_v, var_ts, var_tv, var_q, var_ps
use utils_mod, only: da_advance_time
use netcdf, only: nf90_int, nf90_float, nf90_char

implicit none
private
public  :: read_satwnd
public  :: filter_obs_satwnd
public  :: sort_obs_satwnd

type datalink_satwnd
   character(len=nstring)    :: msg_type   ! BUFR message type name
   character(len=nstring)    :: stid       ! station identifier
   character(len=ndatetime)  :: datetime   ! ccyy-mm-ddThh:mm:ssZ
   character(len=nstring)    :: obtype     ! ob type, eg satwnd
   integer(i_kind)           :: satid      ! satellite identifier
   integer(i_kind)           :: rptype     ! prepbufr report type
   integer(i_kind)           :: obtype_idx ! index of obtype in obtype_list
   integer(i_kind)           :: qm         ! quality marker
   real(r_kind)              :: err        ! ob error
   real(r_kind)              :: lat        ! latitude in degree
   real(r_kind)              :: lon        ! longitude in degree
   real(r_kind)              :: prs        ! pressure
   real(r_kind)              :: landsea    ! land sea qualifier 0:land, 1:sea, 2:coast
   real(r_kind)              :: satzen     ! satellite zenith angle in degree
   real(r_kind)              :: wspd
   real(r_kind)              :: wdir
   real(r_kind)              :: uwind
   real(r_kind)              :: vwind
   real(r_kind)              :: cvwd       ! coefficient of variation
   real(r_kind)              :: pccf1      ! percent confidence, Quality Index without forecast (qifn)
   real(r_kind)              :: pccf2      ! percent confidence, Estimated Error (EE) in m/s converted to percent confidence

   type (datalink_satwnd), pointer :: next ! pointer to next data
end type datalink_satwnd

type(datalink_satwnd), pointer :: rhead=>null(), rlink=>null()

contains

!--------------------------------------------------------------

subroutine read_satwnd(filename, filedate)

   implicit none

   character (len=*),  intent(in)  :: filename
   character (len=10), intent(out) :: filedate  ! ccyymmddhh

   real(r_kind), parameter  :: r8bfms = 9.0E08  ! threshold to check for BUFR missing value
   real(r_kind), parameter  :: pi = acos(-1.0)

   integer(i_kind), parameter :: ntime = 6      ! number of data to read in timestr
   integer(i_kind), parameter :: ninfo = 5      ! number of data to read in infostr
   integer(i_kind), parameter :: nlalo = 2      ! number of data to read in lalostr
   integer(i_kind), parameter :: ndata = 3      ! number of data to read in datastr
   integer(i_kind), parameter :: nqc1  = 2      ! number of data to read in qc1str
   integer(i_kind), parameter :: nqc2  = 2      ! number of data to read in qc2str

   character(len=80) :: timestr, infostr, lalostr, datastr, qc1str, qc2str

   real(r_double), dimension(ntime)  :: timedat
   real(r_double), dimension(ninfo)  :: infodat
   real(r_double), dimension(nlalo)  :: lalodat
   real(r_double), dimension(ndata)  :: wdata
   real(r_double), dimension(nqc1,2) :: qc1dat
   real(r_double), dimension(nqc2,4) :: qc2dat

   character(len=8)  :: subset
   character(len=10) :: cdate

   integer(i_kind) :: iunit, iost, iret, i
   integer(i_kind) :: nchan
   integer(i_kind) :: idate
   integer(i_kind) :: num_report_infile
   integer(i_kind) :: ireadmg, ireadsb

   integer(i_kind) :: iyear, imonth, iday, ihour, imin, isec

   real(r_kind) :: angearth
   logical      :: use_errtable
   real(r_kind) :: oetab(300,33,6)  ! 300 ob types, 33 levels (rows), 6 variables (columns)
   real(r_kind) :: coef
   real(r_kind) :: pob
   integer(i_kind) :: junit, k, kx, ilev, itype, ivar

   continue ! end of declaration

   write(*,*) '--- reading '//trim(filename)//' ---'

   timestr = 'YEAR MNTH DAYS HOUR MINU SECO'
   infostr = 'SAID LSQL SAZA OGCE SWCM'
   lalostr = 'CLATH CLONH'
   datastr = 'PRLC WDIR WSPD'
   qc1str  = 'TCOV CVWD'
   qc2str  = 'GNAPS PCCF'

   use_errtable = .true.

   num_report_infile  = 0

   iunit = 96
   junit = 97

   ! open bufr file
   open (unit=iunit, file=trim(filename), &
         iostat=iost, form='unformatted', status='old')
   if (iost /= 0) then
      write(unit=*,fmt='(a,i5,a)') &
         "Error",iost," opening BUFR obs file "//trim(filename)
         return
   end if

   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readmg(iunit,subset,idate,iret)

   if ( iret /= 0 ) then
      write(unit=*,fmt='(A,I5,A)') &
         "Error",iret," reading BUFR obs file "//trim(filename)
      call closbf(iunit)
      return
   end if
   rewind(iunit)

   write(unit=*,fmt='(1x,a,i10)') trim(filename)//' file date is: ', idate
   write(unit=filedate, fmt='(i10)') idate
   read (filedate(1:10),'(i4,3i2)') iyear, imonth, iday, ihour

   ! open observation error table if provided.
   open (unit=junit, file='obs_errtable', form='formatted', &
         status='old', iostat=iost)
   if ( iost /= 0 ) then
      use_errtable = .false.
      write(unit=*,fmt='(A)') &
            "obs_errtable file is not found. Obs errors can not be assigned."
   else
      use_errtable = .true.
      write(unit=*,fmt='(A)') &
            "obs_errtable file is found. Will use user-provided obs errors."
   end if
   if ( use_errtable ) then
      read_loop: do
         read (junit,'(1x,i3)',iostat=iost) itype
         if ( iost /=0 ) exit read_loop
         do k = 1, 33
            read (junit,'(1x,6e12.5)',iostat=iost) (oetab(itype,k,ivar),ivar=1,6)
            if ( iost /=0 ) exit read_loop
         end do
      end do read_loop
   end if

   if ( .not. associated(rhead) ) then
      nullify ( rhead )
      allocate ( rhead )
      nullify ( rhead%next )
   end if

   if ( .not. associated(rlink) ) then
      rlink => rhead
   else
      allocate ( rlink%next )
      rlink => rlink%next
      nullify ( rlink%next )
   end if

   msg_loop: do while (ireadmg(iunit,subset,idate)==0)
!print*,subset
      if ( subset /= 'NC005030' .and. &
           subset /= 'NC005031' .and. &
           subset /= 'NC005032' .and. &
           subset /= 'NC005034' .and. &
           subset /= 'NC005039' ) then
         ! only handles GOES-16/17 AMV for now
         cycle msg_loop
      end if
      subset_loop: do while (ireadsb(iunit)==0)

         num_report_infile = num_report_infile + 1

         call ufbint(iunit,timedat,ntime,1,iret,timestr)

         iyear  = nint(timedat(1))
         imonth = nint(timedat(2))
         iday   = nint(timedat(3))
         ihour  = nint(timedat(4))
         imin   = nint(timedat(5))
         isec   = min(59, nint(timedat(6))) ! raw BUFR data that has SECO = 60.0 SECOND
                                            ! that was probably rounded from 59.x seconds
                                            ! reset isec to 59 rather than advancing one minute
         if ( iyear  > 1900 .and. iyear  < 3000 .and. &
              imonth >=   1 .and. imonth <=  12 .and. &
              iday   >=   1 .and. iday   <=  31 .and. &
              ihour  >=   0 .and. ihour  <   24 .and. &
              imin   >=   0 .and. imin   <   60 .and. &
              isec   >=   0 .and. isec   <   60 ) then
            write(unit=rlink%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
               iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'
         else
            cycle subset_loop
         end if

         call ufbint(iunit,lalodat,nlalo,1,iret,lalostr)
         if ( abs(lalodat(1)) > 90.0 .or. abs(lalodat(1)) > 360.0 ) cycle subset_loop

         call ufbint(iunit,infodat,ninfo,1,iret,infostr)
         call ufbint(iunit,wdata,ndata,1,iret,datastr)

         call ufbrep(iunit,qc1dat,nqc1,2,iret,qc1str)  ! 2 replications
         call ufbrep(iunit,qc2dat,nqc2,4,iret,qc2str)  ! 4 replications

         call fill_datalink(rlink, missing_r, missing_i)

         if ( lalodat(1) < r8bfms ) rlink % lat = lalodat(1)
         if ( lalodat(2) < r8bfms ) rlink % lon = lalodat(2)

         rlink % satid  = nint(infodat(1))  ! SAID satellite identifier

         rlink % obtype = 'satwnd'
         call set_rptype_satwnd(subset, rlink%satid, rlink%rptype, rlink%stid)

         if ( infodat(4) < r8bfms ) rlink % landsea = infodat(4)  ! LSQL land sea qualifier 0:land, 1:sea, 2:coast
         if ( infodat(5) < r8bfms ) rlink % satzen  = infodat(5)  ! SAZA satellite zenith angle (degree)

         if ( wdata(1) < r8bfms ) rlink % prs  = wdata(1)   ! PRLC pressure in Pa
         if ( wdata(2) < r8bfms ) rlink % wdir = wdata(2)
         if ( wdata(3) < r8bfms ) rlink % wspd = wdata(3)

         if ( qc1dat(2,1) < r8bfms ) rlink % cvwd  = qc1dat(2,1)
         if ( qc2dat(2,2) < r8bfms ) rlink % pccf1 = qc2dat(2,2)
         if ( qc2dat(2,4) < r8bfms ) rlink % pccf2 = qc2dat(2,4)

         if ( rlink%wdir >= 0.0 .and. rlink%wdir <= 360.0 .and. &
              rlink%wspd < r8bfms ) then
            angearth = rlink%wdir * pi / 180.0
            rlink%uwind = -1.0 * rlink%wspd * sin(angearth)
            rlink%vwind = -1.0 * rlink%wspd * cos(angearth)
         end if

         if ( use_errtable ) then
            kx = rlink % rptype
            pob = rlink % prs * 0.01 ! Pa to hPa
            do ilev = 1, 32
               if ( pob >= oetab(kx,ilev+1,1) .and. pob <= oetab(kx,ilev,1) ) then
                  coef = (pob-oetab(kx,ilev,1))/(oetab(kx,ilev,1)-oetab(kx,ilev+1,1))
                  rlink % err = (1.0+coef)*oetab(kx,ilev,4)-coef*oetab(kx,ilev+1,4) !uv
                  exit
               end if
            end do
         end if

!write(333,*) subset
!write(333,*) rlink%datetime,'/', rlink % lat, '/',rlink % lon,'/', rlink % prs,'/',rlink % wdir,'/',rlink % wspd
!write(333,*) rlink % cvwd,'/',rlink % pccf1 ,'/',rlink % pccf2
         allocate ( rlink%next )
         rlink => rlink%next
         nullify ( rlink%next )

      end do subset_loop ! ireadsb
   end do msg_loop ! ireadmg

   call closbf(iunit)
   close(iunit)

   write(*,'(1x,a,a,a,i10)') 'num_report_infile ', trim(filename), ' : ', num_report_infile
end subroutine read_satwnd

!--------------------------------------------------------------

subroutine filter_obs_satwnd

! refer to GSI/read_satwnd.f90

   implicit none

   real(r_kind) :: experr_norm
   logical :: EC_AMV_QC = .true.
   integer(i_kind) :: iland = 0

   rlink => rhead
   do while ( associated(rlink) )

      rlink%qm = 0

      if ( rlink%satzen > 68.0_r_kind ) rlink%qm = 15

      if ( rlink%pccf1 < 80.0_r_kind .or. rlink%pccf1 > 100.0_r_kind ) rlink%qm = 15 ! reject data with low QI

      if ( rlink%prs < 12500.0_r_kind ) rlink%qm = 15 ! reject data above 125hPa

      experr_norm = 10.0_r_kind - 0.1_r_kind * rlink%pccf2
      if ( rlink%wspd > 0.1_r_kind ) then
         experr_norm = experr_norm / rlink%wspd
      else
         experr_norm = 100.0_r_kind
      end if
      if ( experr_norm > 0.9_r_kind ) rlink%qm = 15 ! reject data with estimated error/spd>0.9

      if ( rlink%rptype == 240 .or. &
           rlink%rptype == 245 .or. &
           rlink%rptype == 246 .or. &
           rlink%rptype == 251 ) then
         if ( rlink%cvwd < 0.04_r_kind ) rlink%qm = 15
         if ( rlink%cvwd > 0.50_r_kind ) rlink%qm = 15
      end if

      if ( EC_AMV_QC ) then
         if ( rlink%pccf1 < 90_r_kind .or. rlink%pccf1 > 100.0_r_kind ) rlink%qm = 15 ! stricter QI
         if ( rlink%prs < 15000.0_r_kind) rlink%qm = 15 ! all high level
         if ( rlink%rptype == 251 .and. rlink%prs < 70000.0_r_kind ) rlink%qm = 15  ! VIS
         if ( rlink%rptype == 246 .and. rlink%prs > 30000.0_r_kind ) rlink%qm = 15  ! WVCA
         if ( nint(rlink%landsea) == iland .and. rlink%prs > 85000.0_r_kind) rlink%qm = 15  ! low over land
      end if

      rlink => rlink%next

   end do

end subroutine filter_obs_satwnd

!--------------------------------------------------------------

subroutine sort_obs_satwnd

   implicit none

   integer(i_kind)                       :: i, iv, k, ii
   integer(i_kind)                       :: ityp, irec, ivar
   integer(i_kind), dimension(nobtype)   :: nlocs
   integer(i_kind), dimension(nobtype)   :: nvars
   integer(i_kind), dimension(nobtype)   :: iloc
   character(len=12)                     :: obtype
   logical,         dimension(nvar_met)  :: vmask ! for counting available variables for one obtype

   nlocs(:) = 0
   nvars(:) = 0

   write(*,*) '--- sorting satwnd obs...'

   ! count the numbers
   rlink => rhead
   set_obtype_loop: do while ( associated(rlink) )

      ! find index of obtype in obtype_list
      rlink%obtype_idx = ufo_vars_getindex(obtype_list, rlink%obtype)
      if ( rlink % obtype_idx > 0 ) then
         ! obtype assigned, advance ob counts
         nlocs(rlink%obtype_idx) = nlocs(rlink%obtype_idx) + 1
      end if

      rlink => rlink%next
   end do set_obtype_loop

   write(*,'(1x,20x,a10)') 'nlocs'
   do i = 1, nobtype
      write(*,'(1x,a20,i10)') obtype_list(i), nlocs(i)
   end do

   ! allocate data arrays with the just counted numbers
   allocate (xdata(nobtype))
   do i = 1, nobtype
      xdata(i) % nlocs = nlocs(i)
      vmask = vflag(:,i) == itrue
      nvars(i) = count(vmask)
      xdata(i) % nvars = nvars(i)

      if ( nlocs(i) > 0 ) then

         allocate (xdata(i)%xinfo_int  (nlocs(i), nvar_info))
         allocate (xdata(i)%xinfo_float(nlocs(i), nvar_info))
         allocate (xdata(i)%xinfo_char (nlocs(i), nvar_info))
         xdata(i)%xinfo_int  (:,:) = missing_i
         xdata(i)%xinfo_float(:,:) = missing_r
         xdata(i)%xinfo_char (:,:) = ''

         if ( nvars(i) > 0 ) then
            allocate (xdata(i)%xfield(nlocs(i), nvars(i)))
            allocate (xdata(i)%var_idx(nvars(i)))
            xdata(i)%xfield(:,:)%val = missing_r ! initialize
            xdata(i)%xfield(:,:)%qm  = missing_i ! initialize
            xdata(i)%xfield(:,:)%err = missing_r ! initialize
            ivar = 0
            do iv = 1, nvar_met
               if ( vflag(iv,i) == ifalse ) cycle
               ivar = ivar + 1
               xdata(i)%var_idx(ivar) = iv
            end do
         end if
      end if
   end do

   ! transfer data from rlink to xdata

   iloc(:) = 0
   irec    = 0

   rlink => rhead
   reports: do while ( associated(rlink) )
      irec = irec + 1
      ityp = rlink%obtype_idx
      if ( ityp < 0 ) then
         rlink => rlink%next
         cycle reports
      end if
         iloc(ityp) = iloc(ityp) + 1

         do i = 1, nvar_info
            if ( type_var_info(i) == nf90_int ) then
               if ( name_var_info(i) == 'record_number' ) then
                  xdata(ityp)%xinfo_int(iloc(ityp),i) = irec
               end if
            else if ( type_var_info(i) == nf90_float ) then
               if ( trim(name_var_info(i)) == 'latitude' ) then
                  xdata(ityp)%xinfo_float(iloc(ityp),i) = rlink%lat
               else if ( trim(name_var_info(i)) == 'longitude' ) then
                  xdata(ityp)%xinfo_float(iloc(ityp),i) = rlink%lon
               else if ( trim(name_var_info(i)) == trim(var_prs) ) then
                  xdata(ityp)%xinfo_float(iloc(ityp),i) = rlink%prs
               end if
            else if ( type_var_info(i) == nf90_char ) then
               if ( trim(name_var_info(i)) == 'datetime' ) then
                  xdata(ityp)%xinfo_char(iloc(ityp),i) = rlink%datetime
               else if ( trim(name_var_info(i)) == 'station_id' ) then
                  xdata(ityp)%xinfo_char(iloc(ityp),i) = rlink%stid
               end if
            end if ! type_var_info
         end do

         do i = 1, nvars(ityp)
            ivar = xdata(ityp)%var_idx(i)
            if ( name_var_met(ivar) == trim(var_u) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = rlink%uwind
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = rlink%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = rlink%err
            else if ( name_var_met(ivar) == trim(var_v) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = rlink%vwind
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = rlink%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = rlink%err
            end if
            xdata(ityp)%xfield(iloc(ityp),i)%rptype = rlink%rptype
         end do
      rlink => rlink%next
   end do reports

   ! done with rlink
   ! release the linked list
   rlink => rhead
   do while ( associated(rlink) )
      rhead => rlink%next
      if ( associated (rlink) ) deallocate (rlink)
      rlink => rhead
   end do
   nullify (rhead)

end subroutine sort_obs_satwnd

!--------------------------------------------------------------

subroutine fill_datalink (datalink, rfill, ifill)

   implicit none

   type (datalink_satwnd), intent(inout) :: datalink
   real(r_kind),           intent(in)    :: rfill    ! fill value in real
   integer(i_kind),        intent(in)    :: ifill    ! fill value in integer

   integer(i_kind) :: i

   !datalink % datetime = ''
   datalink % lat      = rfill
   datalink % lon      = rfill
   datalink % satid    = ifill
   datalink % landsea  = rfill
   datalink % satzen   = rfill
   datalink % prs      = rfill
   datalink % wdir     = rfill
   datalink % wspd     = rfill
   datalink % cvwd     = rfill
   datalink % pccf1    = rfill
   datalink % pccf2    = rfill

end subroutine fill_datalink

!--------------------------------------------------------------

subroutine set_rptype_satwnd(subset, satid, rptype, stid)

  implicit none
  character(len=*), intent(in)  :: subset  ! bufr message subset
  integer(i_kind),  intent(in)  :: satid   ! satellite id
  integer(i_kind),  intent(out) :: rptype  ! report type
  character(len=nstring), intent(out) :: stid    ! station id

  character(len=3) :: csatid

  rptype = -1
  stid   = ''
  write(csatid, '(i3.3)') satid

  select case ( trim(subset) )
  case ( 'NC005030' )  ! IR LW
     rptype = 245
     stid = 'IR'//csatid
  case ( 'NC005039' )  ! IR SW
     rptype = 240
     stid = 'IR'//csatid
  case ( 'NC005032' )  ! VIS
     rptype = 251
     stid = 'VI'//csatid
  case ( 'NC005034' )  ! WV cloud top
     rptype = 246
     stid = 'WV'//csatid
  case ( 'NC005031' )  ! WV clear sky/deep layer
     rptype = 247
     stid = 'WV'//csatid
  end select
end subroutine set_rptype_satwnd

end module satwnd_mod

