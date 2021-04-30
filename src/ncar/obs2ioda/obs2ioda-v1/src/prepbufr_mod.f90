module prepbufr_mod

! adapated from WRFDA/var/da/da_obs_io/da_read_obs_bufr.inc

use kinds, only: r_kind, i_kind, r_double
use define_mod, only: nobtype, set_obtype_conv, obtype_list, xdata, &
   nvar_met, nvar_info, type_var_info, name_var_met, name_var_info, &
   t_kelvin, missing_r, missing_i, vflag, itrue, ifalse, nstring, ndatetime, not_use
use ufo_vars_mod, only: ufo_vars_getindex, var_prs, var_u, var_v, var_ts, var_tv, var_q, var_ps
use utils_mod, only: da_advance_time
use netcdf, only: nf90_int, nf90_float, nf90_char

implicit none
private
public  :: read_prepbufr
public  :: sort_obs_conv
public  :: filter_obs_conv
public  :: do_tv_to_ts

! variables for storing data
type field_type
   real(r_kind)       :: val          ! observation value
   integer(i_kind)    :: qm           ! observation quality marker
   real(r_kind)       :: err          ! observational error
   contains
     procedure :: init => init_field
end type field_type

type each_level_type
   type (field_type)  :: h            ! height in m
   type (field_type)  :: u            ! Wind x-component in m/s
   type (field_type)  :: v            ! Wind y-component in m/s
   type (field_type)  :: p            ! Pressure in Pa
   type (field_type)  :: t            ! Temperature in K
   type (field_type)  :: tv           ! virtual temperature in K
   type (field_type)  :: q            ! (kg/kg)
   real(r_kind)       :: lat          ! Latitude in degree
   real(r_kind)       :: lon          ! Longitude in degree
   real(r_kind)       :: dhr          ! obs time minus analysis time in hour
   real(r_kind)       :: pccf         ! percent confidence
   character(len=ndatetime) :: datetime     ! ccyy-mm-ddThh:mm:ssZ
   type (each_level_type), pointer :: next => null()
   contains
     procedure :: init => init_each_level
end type each_level_type

type report_conv
   ! data from BUFR file
   integer(i_kind)           :: t29         ! data dump report type
   integer(i_kind)           :: rptype      ! prepbufr report type
   integer(i_kind)           :: satid       ! satellite id
   character(len=nstring)    :: msg_type    ! BUFR message type name
   character(len=nstring)    :: stid        ! station identifier
   character(len=ndatetime)  :: datetime    ! ccyy-mm-ddThh:mm:ssZ
   integer(i_kind)           :: nlevels     ! number of levels
   real(r_kind)              :: lat         ! latitude in degree
   real(r_kind)              :: lon         ! longitude in degree
   real(r_kind)              :: elv         ! elevation in m
   real(r_kind)              :: dhr         ! obs time minus analysis time in hour
   type (field_type)         :: ps          ! surface pressure
   type (field_type)         :: slp         ! sea level pressure
   type (field_type)         :: pw          ! precipitable water
   type (each_level_type), pointer :: each => null()
   ! derived info
   character(len=nstring)    :: obtype      ! ob type, eg sonde, satwnd
   integer(i_kind)           :: obtype_idx  ! index of obtype in obtype_list
   type(each_level_type), pointer :: first => null()
   type(report_conv),     pointer :: next => null()
   contains
     procedure :: init => init_report
end type report_conv

type(report_conv), pointer :: phead=>null(), plink=>null()

integer(i_kind), parameter :: lim_qm = 4
logical :: do_tv_to_ts

contains

!--------------------------------------------------------------

subroutine read_prepbufr(filename, filedate)

   implicit none

   character (len=*),  intent(in)  :: filename
   character (len=10), intent(out) :: filedate  ! ccyymmddhh

   real(r_kind), parameter  :: r8bfms = 9.0E08  ! threshold to check for BUFR missing value

   logical           :: match, end_of_file, drift
   character(len=8)  :: subset, subst2, csid, csid2
   character(len=40) :: obstr,hdstr,qmstr,oestr, pcstr, drstr
   real(r_double)    :: r8sid, r8sid2
   real(r_double)    :: hdr(7), hdr2(7), hdr_save(7)
   real(r_double)    :: obs(8,255),qms(8,255),oes(8,255),pco(8,255)
   real(r_double)    :: obs2(8,255),qms2(8,255),oes2(8,255),pco2(8,255)
   real(r_double)    :: pmo(2,1), pmo2(2,1), pmo_save(2,1)
   real(r_double)    :: temp(8,255)
   real(r_double)    :: obs_save(8,255)
   real(r_double)    :: pob, pob1, pob2
   real(r_double)    :: drf(8,255), drf2(8,255) ! for balloon drift
   real(r_double)    :: satqc(1), satid(1)
   equivalence (r8sid, csid), (r8sid2, csid2)

   character(len=14) :: cdate, dsec, obs_date
   integer(i_kind)   :: idate, idate2
   integer(i_kind)   :: nlevels, nlevels2, lv1, lv2
   integer(i_kind)   :: iyear, imonth, iday, ihour, imin, isec
   integer(i_kind)   :: num_report_infile
   integer(i_kind)   :: iret, iret2, iost, n, i, j, k, i1, i2
   integer(i_kind)   :: kx, t29
   integer(i_kind)   :: tpc
   integer(i_kind)   :: iunit, junit, itype, ivar
   logical           :: use_errtable, combine_mass_wind
   real(r_kind)      :: oetab(300,33,6)  ! 300 ob types, 33 levels (rows), 6 variables (columns)
   real(r_kind)      :: coef
   real(r_kind)      :: qs, tval

   write(*,*) '--- reading '//trim(filename)//' ---'
   hdstr='SID XOB YOB DHR TYP ELV T29'
   obstr='POB QOB TOB ZOB UOB VOB PWO CAT' ! observation
   qmstr='PQM QQM TQM ZQM WQM NUL PWQ NUL' ! quality marker
   oestr='POE QOE TOE NUL WOE NUL PWE NUL' ! observation error
   pcstr='PPC QPC TPC ZPC WPC NUL PWP NUL' ! program code
   drstr='XDR YDR HRDR                   ' ! balloon drift code

   ! initialize variables

   if ( .not. associated(phead) ) then
      nullify ( phead )
      allocate ( phead )
      nullify ( phead%next )
   end if

   use_errtable      = .false.
   combine_mass_wind = .false.
   !do_tv_to_ts       = .false. !assignment moved to main.f90

   num_report_infile  = 0

   iunit = 96
   junit = 97

   ! open bufr file
   open (unit=iunit, file=trim(filename), &
        iostat=iost, form='unformatted', status='old')
   if (iost /= 0) then
      write(unit=*,fmt='(a,i5,a)') &
         "Error",iost," opening PREPBUFR obs file "//trim(filename)
         return
   end if
   ! open observation error table if provided.
   open (unit=junit, file='obs_errtable', form='formatted', &
         status='old', iostat=iost)
   if ( iost /= 0 ) then
      use_errtable = .false.
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

   call openbf(iunit,'IN',iunit)
   call datelen(10)

   call readns(iunit,subset,idate,iret)  ! read in the next subset
   if ( iret /= 0 ) then
      write(unit=*,fmt='(A,I5,A)') &
         "Error",iret," reading PREPBUFR obs file "//trim(filename)
      call closbf(iunit)
      return
   end if
   rewind(iunit)

   write(unit=*,fmt='(1x,a,a,i10)') trim(filename), ' file date is: ', idate

   write(unit=filedate,fmt='(i10)') idate

   ! read data

   match        = .false.
   end_of_file  = .false.
   reports: do while ( .not. end_of_file )

      if ( match ) then
         call readns(iunit,subset,idate,iret)  ! read in the next subset
         if ( iret /= 0 ) then
            write(unit=*,fmt='(A,I3,A,I3)') &
               "return code from readns",iret,       &
               "reach the end of PREPBUFR obs unit",iunit
            exit reports
         end if
      end if

      num_report_infile = num_report_infile + 1

      call ufbint(iunit,satqc,1,1,iret,'QIFN')
      call ufbint(iunit,satid,1,1,iret,'SAID')
      call ufbint(iunit,hdr,7,1,iret,hdstr)
      call ufbint(iunit,pmo,2,1,iret,'PMO PMQ')
      call ufbint(iunit,qms,8,255,nlevels,qmstr)
      call ufbint(iunit,oes,8,255,nlevels,oestr)
      call ufbint(iunit,pco,8,255,nlevels,pcstr)
      call ufbint(iunit,obs,8,255,nlevels,obstr)

      r8sid = hdr(1)
      t29 = nint(hdr(7))
      kx  = nint(hdr(5))

      if ( use_errtable ) then
         do k = 1, nlevels
            pob = obs(1,k)
            do lv1 = 1, 32
               if ( pob >= oetab(kx,lv1+1,1) .and. pob <= oetab(kx,lv1,1) ) then
                  coef = (pob-oetab(kx,lv1,1))/(oetab(kx,lv1,1)-oetab(kx,lv1+1,1))
                  oes(1,k) = (1.0+coef)*oetab(kx,lv1,5)-coef*oetab(kx,lv1+1,5) !p
                  oes(2,k) = (1.0+coef)*oetab(kx,lv1,3)-coef*oetab(kx,lv1+1,3) !q
                  oes(3,k) = (1.0+coef)*oetab(kx,lv1,2)-coef*oetab(kx,lv1+1,2) !t
                  oes(5,k) = (1.0+coef)*oetab(kx,lv1,4)-coef*oetab(kx,lv1+1,4) !uv
                  oes(7,k) = (1.0+coef)*oetab(kx,lv1,6)-coef*oetab(kx,lv1+1,6) !pw
                  exit
               end if
            end do
         end do
      end if

      drift = kx==120.or.kx==220.or.kx==221
      if ( drift ) call ufbint(iunit,drf,8,255,iret,drstr)

      call readns(iunit,subst2,idate2,iret)

      if ( iret /= 0 ) then
         end_of_file = .true.
      else
        if ( combine_mass_wind ) then
         match_check: do
            call ufbint(iunit,hdr2,7,1,iret2,hdstr)
            ! check if this subset and the previous one are matching mass and wind
            match = .true.
            if ( subset /= subst2 ) then
               match = .false.
               exit match_check
            end if
            r8sid2 = hdr2(1)
            if ( csid /= csid2 ) then  ! check SID
               match = .false.
               exit match_check
            end if
            do i = 2, 4   ! check XOB, YOB, DHR
               if ( hdr(i) /= hdr2(i) ) then
                  match = .false.
                  exit match_check
               end if
            end do
            if ( hdr(6) /= hdr2(6) ) then   ! check ELV
               match = .false.
               exit match_check
            end if
            !The two headers match, now read data from the second subset
            call ufbint(iunit,pmo2,2,1,nlevels2,'PMO PMQ')
            call ufbint(iunit,qms2,8,255,nlevels2,qmstr)
            call ufbint(iunit,oes2,8,255,nlevels2,oestr)
            call ufbint(iunit,pco2,8,255,nlevels2,pcstr)
            call ufbint(iunit,obs2,8,255,nlevels2,obstr)
            if ( drift ) call ufbint(iunit,drf2,8,255,iret,drstr)

            if ( use_errtable ) then
               kx = nint(hdr2(5))
               do k = 1, nlevels2
                  pob = obs2(1,k)
                  do lv1 = 1, 32
                     if ( pob >= oetab(kx,lv1+1,1) .and. pob <= oetab(kx,lv1,1) ) then
                        coef = (pob-oetab(kx,lv1,1))/(oetab(kx,lv1,1)-oetab(kx,lv1+1,1))
                        oes2(1,k) = (1.0+coef)*oetab(kx,lv1,5)-coef*oetab(kx,lv1+1,5) !p
                        oes2(2,k) = (1.0+coef)*oetab(kx,lv1,3)-coef*oetab(kx,lv1+1,3) !q
                        oes2(3,k) = (1.0+coef)*oetab(kx,lv1,2)-coef*oetab(kx,lv1+1,2) !t
                        oes2(5,k) = (1.0+coef)*oetab(kx,lv1,4)-coef*oetab(kx,lv1+1,4) !uv
                        oes2(7,k) = (1.0+coef)*oetab(kx,lv1,6)-coef*oetab(kx,lv1+1,6) !pw
                        exit
                     end if
                  end do
               end do
            end if

            ! If this is a surface report, the wind subset precedes the
            ! mass subset - switch the subsets around in order to combine
            ! the surface pressure properly
            kx = nint(hdr(5))
            if ( kx == 280 .or. kx == 281 .or. kx == 284  .or.  &
                 kx == 287 .or. kx == 288  ) then
               pmo_save = pmo2
               pmo2 = pmo
               pmo = pmo_save
               temp = obs2
               obs2 = obs
               obs = temp
               hdr_save = hdr2
               hdr2 = hdr
               hdr = hdr_save
               temp = qms2
               qms2 = qms
               qms = temp
               temp = oes2
               oes2 = oes
               oes = temp
               temp = pco2
               pco2 = pco
               pco = temp
            end if

            ! combine the two matching subsets
            do i = 1, 2
               if ( pmo(i,1) > r8bfms ) then
                  pmo(i,1) = pmo2(i,1)
               end if
            end do
            lev_loop: do lv2 = 1, nlevels2
               do lv1 = 1, nlevels
                  pob1 = obs(1,lv1)
                  pob2 = obs2(1,lv2)
                  if ( pob1 == pob2 ) then
                     do i = 1, 7   ! skip the CAT
                        if ( obs(i,lv1) > r8bfms ) then
                           obs(i,lv1) = obs2(i,lv2)
                           if ( obs2(i,lv2) <= r8bfms ) then
                              obs(8,lv1) = obs2(8,lv2)  ! rewrite CAT
                           end if
                        end if
                        if ( oes(i,lv1) > r8bfms ) then
                           oes(i,lv1) = oes2(i,lv2)
                        end if
                        if ( pco(i,lv1) > r8bfms ) then
                           pco(i,lv1) = pco2(i,lv2)
                        end if
                     end do
                     do i = 1, 8
                        if ( qms(i,lv1) > r8bfms ) then
                           qms(i,lv1) = qms2(i,lv2)
                        end if
                     end do
                     if ( drift) then
                        do i = 1, 3
                           if ( drf(i,lv1) > r8bfms ) then
                              drf(i,lv1) = drf2(i,lv2)
                           end if
                        end do
                     end if
                     cycle lev_loop
                  else if ( (pob2 > pob1) .or. (lv1 .eq. nlevels) ) then
                     nlevels = nlevels + 1
                     obs(:,nlevels) = obs2(:,lv2)
                     qms(:,nlevels) = qms2(:,lv2)
                     oes(:,nlevels) = oes2(:,lv2)
                     pco(:,nlevels) = pco2(:,lv2)
                     if ( drift) then
                        drf(:,nlevels) = drf2(:,lv2)
                     end if
                     cycle lev_loop
                  end if
               end do
            end do lev_loop
            ! sort combined report in descending pressure order
            do i1 = 1, nlevels-1
               do i2 = i1+1, nlevels
                  if ( obs(1,i2) .gt. obs(1,i1) ) then
                     temp(:,1) = obs(:,i1)
                     obs(:,i1) = obs(:,i2)
                     obs(:,i2) = temp(:,1)
                     temp(:,1) = qms(:,i1)
                     qms(:,i1) = qms(:,i2)
                     qms(:,i2) = temp(:,1)
                     temp(:,1) = oes(:,i1)
                     oes(:,i1) = oes(:,i2)
                     oes(:,i2) = temp(:,1)
                     temp(:,1) = pco(:,i1)
                     pco(:,i1) = pco(:,i2)
                     pco(:,i2) = temp(:,1)
                     if ( drift ) then
                        temp(:,1) = drf(:,i1)
                        drf(:,i1) = drf(:,i2)
                        drf(:,i2) = temp(:,1)
                     end if
                  end if
               end do
            end do
            exit match_check
         end do match_check
        end if ! combine_mass_wind

         if ( .not. match ) then
            subset = subst2
            idate = idate2
         end if

      end if ! readns iret=0

      ! skip some types
      !  61: Satellite soundings/retrievals/radiances
      !  66: SSM/I rain rate product
      !  72: NEXTRAD VAD winds
      if ( t29 == 61 .or. t29 == 66 .or. t29 == 72 ) cycle reports

      ! OSLK (elv=48) reported Ps=341hPa
      !if ( csid(1:4) == 'OSLK' ) cycle reports

      ! station 78383 (kx=184/t29=511) has station elevation=9999.0 and valid height quality markers
      ! a few other stations have station elevation=9999.0 and height quality markers = 15
      ! to do: this check could be done in filter_obs_conv by implementing blacklist
      if ( abs(hdr(6) - 9999.0) < 0.01 .and. t29 /= 41 ) cycle reports ! aircraft could have elv=9999.0

      ! the following (as in GSI read_prepbufr.f90) could reject reports at the poles
      if ( abs(hdr(2)) > 360.0 .or. abs(hdr(3)) > 90.0 ) cycle reports
      ! the following allows reports at the poles
      !if ( abs(hdr(2)-360.0) < 0.01 .or. abs(hdr(3)-90.0) < 0.01 ) cycle reports

      write(dsec,'(i6,a1)') int(hdr(4)*60.0*60.0), 's' ! seconds
      write(cdate,'(i10)') idate
      call da_advance_time (cdate(1:10), trim(dsec), obs_date)
      read (obs_date(1:14),'(i4,5i2)') iyear, imonth, iday, ihour, imin, isec
      if ( iyear  < 1900 .or. iyear  > 3000 .or. &
           imonth <    1 .or. imonth >   12 .or. &
           iday   <    1 .or. iday   >   31 .or. &
           ihour  <    0 .or. ihour  >=  24 .or. &
           imin   <    0 .or. imin   >=  60 .or. &
           isec   <    0 .or. isec   >=  60 ) then
         cycle reports
      end if

      if (.not. associated(plink)) then
         plink => phead
      else
         allocate ( plink%next )
         plink => plink%next
         nullify ( plink%next )
      end if

      ! initialize plink with missing values
      call plink % init()

      plink % msg_type(1:8) = subset
      plink % stid(1:5)     = csid(1:5)
      plink % rptype        = kx
      plink % t29           = t29
      call set_obtype_conv(plink%t29, plink%obtype)
      plink % lon           = hdr(2)
      plink % lat           = hdr(3)
      plink % dhr           = hdr(4)    ! difference in hour
      plink % elv           = hdr(6)

      write(unit=plink%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
         iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'

      if ( satid(1) < r8bfms )  then
         plink % satid = nint(satid(1))
      end if

      if ( pmo(1,1) < r8bfms ) then
         plink % slp % val = pmo(1,1)*100.0
         plink % slp % qm  = nint(pmo(2,1))
      end if
      if ( obs(7,1) < r8bfms ) then
         plink % pw % val = obs(7,1) * 0.1    ! convert to cm
         if ( qms(7,1) < r8bfms ) then
            plink % pw % qm  = nint(qms(7,1))
         end if
         if ( oes(7,1) < r8bfms ) then
            plink % pw % err = oes(7,1)
         end if
      end if

      plink % nlevels = 0 ! initialize

      loop_nlevels: do k = 1, nlevels

         ! pressure and height quality markers do not carry over to ioda
         ! qm=8: Observed surface pressure is > 1100 mb or < 450 mb,
         !       or is more than 100 mb above or below model (guess) surface pressure,
         !       or an observed pressure on any level is <= 0 mb or more than 100 mb
         !       above or below model (guess) pressure at same level,
         !       or a non-pressure observation failed a limit check
         ! qm=15: Observation is flagged for non-use by analysis
         if ( qms(1,k) < r8bfms ) then ! pressure
            if ( nint(qms(1,k)) == 8 .or. nint(qms(1,k)) == 15 ) cycle loop_nlevels
         end if
         if ( qms(4,k) < r8bfms ) then ! height
            if ( nint(qms(4,k)) == 8 .or. nint(qms(4,k)) == 15 ) cycle loop_nlevels
         end if

         plink % nlevels = plink % nlevels + 1

         if ( .not. associated(plink % each) ) then
            allocate ( plink % each )
            plink % first => plink % each
         else
            allocate ( plink % each % next)
            plink % each => plink % each % next
            nullify ( plink % each % next )
         end if

         call plink % each % init()

         if ( satqc(1) < r8bfms ) then
            plink % each % pccf = satqc(1)
         end if

         if ( drift ) then
            if ( drf(1,k) < r8bfms ) plink % each % lon = drf(1,k)
            if ( drf(2,k) < r8bfms ) plink % each % lat = drf(2,k)
            if ( drf(3,k) < r8bfms ) then
               plink % each % dhr = drf(3,k)
               write(dsec,'(i6,a1)') int(drf(3,k)*60.0*60.0), 's' ! seconds
               call da_advance_time (cdate(1:10), trim(dsec), obs_date)
               read (obs_date(1:14),'(i4,5i2)') iyear, imonth, iday, ihour, imin, isec
               write(unit=plink%each%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
                  iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'
            end if
         end if

         if ( obs(1,k) > 0.0 .and. obs(1,k) < r8bfms ) then
            plink % each % p % val = obs(1,k)*100.0  ! convert to Pa
            if ( qms(1,k) < r8bfms ) then
               plink % each % p % qm  = nint(qms(1,k))
            end if
            if ( oes(1,k) < r8bfms ) then
               plink % each % p % err = oes(1,k)*100.0 ! convert to Pa
            end if
            ! obs(8,k) == CAT (Data Level Category)
            ! CAT=0: Surface level (mass reports only)
            if ( nint(obs(8,k)) == 0 ) then
               plink % ps % val = plink % each % p % val
               plink % ps % qm  = plink % each % p % qm
               plink % ps % err = plink % each % p % err
            end if
         end if

         if ( obs(4,k) < r8bfms ) then
            plink % each % h % val = obs(4,k)
            if ( qms(4,k) < r8bfms ) then
               plink % each % h % qm  = nint(qms(4,k))
            end if
         end if

         tpc = missing_i
         if ( obs(3,k) < r8bfms ) then
            obs(3,k) = obs(3,k) + t_kelvin
            if ( pco(3,k) < r8bfms ) tpc = nint(pco(3,k))
         end if

         ! scale q and compute t from tv, if they aren't missing
         if (obs(2,k) > 0.0 .and. obs(2,k) < r8bfms) then
            obs(2,k) = obs(2,k)*1e-6  ! mg/kg to kg/kg
            if (obs(3,k) > -200.0 .and. obs(3,k) < 350.0) then
               if ( do_tv_to_ts .and. tpc == 8 ) then   ! program code 008 VIRTMP
                  ! 0.61 is used in NCEP prepdata.f to convert T to Tv
                  obs(3,k) = obs(3,k) / (1.0 + 0.61 * obs(2,k))
               end if
            end if
         end if

         if ( do_tv_to_ts .or. tpc /= 8 ) then   ! program code 008 VIRTMP
            ! sensible temperature
            if ( obs(3,k) < r8bfms ) then
               plink % each % t % val = obs(3,k)
               if ( qms(3,k) < r8bfms ) then
                  plink % each % t % qm  = nint(qms(3,k))
               end if
               if ( oes(3,k) < r8bfms ) then
                  plink % each % t % err = oes(3,k)
               end if
            end if
         else
            ! virtual temperature
            if ( obs(3,k) < r8bfms ) then
               plink % each % tv % val = obs(3,k)
               if ( qms(3,k) < r8bfms ) then
                  plink % each % tv % qm  = nint(qms(3,k))
               end if
               if ( oes(3,k) < r8bfms ) then
                  plink % each % tv % err = oes(3,k)
               end if
            end if
         end if

         if (obs(5,k) < r8bfms .and. obs(6,k) < r8bfms ) then
            plink % each % u % val = obs(5,k)
            plink % each % v % val = obs(6,k)
            if ( qms(5,k) < r8bfms ) then
               plink % each % u % qm  = nint(qms(5,k))
            end if
            if ( qms(6,k) < r8bfms ) then
               plink % each % v % qm  = nint(qms(6,k))
            else
               plink % each % v % qm  = plink % each % u % qm
            end if
            if ( oes(5,k) < r8bfms ) then
               plink % each % u % err = oes(5,k)
            end if
            if ( oes(6,k) < r8bfms ) then
               plink % each % v % err = oes(6,k)
            else
               plink % each % v % err = plink % each % u % err
            end if
         end if

         if ( obs(2,k)>0.0 .and. obs(2,k)<r8bfms ) then
            plink % each % q % val = obs(2,k)
            if ( qms(2,k) < r8bfms ) then
               plink % each % q % qm  = nint(qms(2,k))
            end if
            if ( oes(2,k) < r8bfms ) then
               if ( abs(plink%each%p%val - missing_r) > 0.01 ) then
                  if ( abs(plink%each%t%val - missing_r) > 0.01 ) then
                     call calc_qs(plink%each%t%val, plink%each%p%val, qs)
                  else if ( abs(plink%each%tv%val - missing_r) > 0.01 ) then
                     tval = plink%each%tv%val / (1.0 + 0.61 * obs(2,k))
                     call calc_qs(tval, plink%each%p%val, qs)
                  end if
                  plink % each % q % err = oes(2,k)*10.0 ! convert to % from PREPBUFR percent divided by 10
                  plink % each % q % err = plink % each % q % err * qs * 0.01 ! convert from RH to q
               end if
            end if
         end if

      end do loop_nlevels

   end do reports

   write(*,*) 'num_report_infile ', trim(filename), ' : ', num_report_infile

   call closbf(iunit)
   close(iunit)
   if ( use_errtable ) then
      close(junit)
   end if

end subroutine read_prepbufr

!--------------------------------------------------------------

subroutine sort_obs_conv

   implicit none

   integer(i_kind)                       :: i, iv, k, ii
   integer(i_kind)                       :: ityp, irec, ivar
   integer(i_kind), dimension(nobtype)   :: nrecs
   integer(i_kind), dimension(nobtype)   :: nlocs
   integer(i_kind), dimension(nobtype)   :: nvars
   integer(i_kind), dimension(nobtype)   :: iloc
   character(len=12)                     :: obtype
   logical,         dimension(nvar_met)  :: vmask ! for counting available variables for one obtype
!   logical,         dimension(nvar_info) :: imask ! for counting dimension of one var type
!   integer(i_kind)                       :: ninfo_int
!   integer(i_kind)                       :: ninfo_float
!   integer(i_kind)                       :: ninfo_char

   nrecs(:) = 0
   nlocs(:) = 0
   nvars(:) = 0

   write(*,*) '--- sorting conv obs...'

   ! set obtype from dump data type t29 and
   ! and count the numbers
   plink => phead
   set_obtype_loop: do while ( associated(plink) )

      !move this call to subroutine read_prepbufr
      !because plink%obtype is needed in subroutine filter_obs_conv which is called before this sort_obs_conv
      !call set_obtype_conv(plink%t29, plink%obtype)

      ! find index of obtype in obtype_list
      plink%obtype_idx = ufo_vars_getindex(obtype_list, plink%obtype)
      if ( plink % obtype_idx > 0 ) then
         ! obtype assigned, advance ob counts
         nrecs(plink%obtype_idx) = nrecs(plink%obtype_idx) + 1
         nlocs(plink%obtype_idx) = nlocs(plink%obtype_idx) + plink%nlevels
      !else
         !found undefined t29=534/kx=180,280 in file
         !write(*,*) 't29 = ', plink%t29
         !write(*,*) 'kx  = ', plink%rptype
      end if

      plink => plink%next
   end do set_obtype_loop

   !write(*,*) 'num_report_decoded = ', sum(nrecs(:))
   write(*,'(1x,20x,2a10)') 'nrecs', 'nlocs'
   do i = 1, nobtype
      write(*,'(1x,a20,2i10)') obtype_list(i), nrecs(i), nlocs(i)
   end do

   ! allocate data arrays with the just counted numbers
   allocate (xdata(nobtype))
   do i = 1, nobtype
      xdata(i) % nrecs = nrecs(i)
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

   ! transfer data from plink to xdata

   iloc(:) = 0
   irec    = 0

   plink => phead
   reports: do while ( associated(plink) )
      irec = irec + 1
      ityp = plink%obtype_idx
      if ( ityp < 0 ) then
         plink => plink%next
         cycle reports
      end if
      plink % each => plink % first
      levels: do while ( associated(plink % each) )
         iloc(ityp) = iloc(ityp) + 1

         do i = 1, nvar_info
            if ( type_var_info(i) == nf90_int ) then
               if ( name_var_info(i) == 'record_number' ) then
                  xdata(ityp)%xinfo_int(iloc(ityp),i) = irec
               end if
            else if ( type_var_info(i) == nf90_float ) then
               if ( name_var_info(i) == 'time' ) then
                  if ( plink%each%dhr > missing_r ) then  ! time drift
                     xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%each%dhr
                  else
                     xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%dhr
                  end if
               else if ( trim(name_var_info(i)) == 'station_elevation' ) then
                  xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%elv
               else if ( trim(name_var_info(i)) == 'latitude' ) then
                  if ( plink%each%lat > missing_r ) then  ! drift
                     xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%each%lat
                  else
                     xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%lat
                  end if
               else if ( trim(name_var_info(i)) == 'longitude' ) then
                  if ( plink%each%lon > missing_r ) then  ! drift
                     xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%each%lon
                  else
                     xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%lon
                  end if
               else if ( trim(name_var_info(i)) == 'height' ) then
                  xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%each%h%val
               else if ( trim(name_var_info(i)) == trim(var_prs) ) then
                  xdata(ityp)%xinfo_float(iloc(ityp),i) = plink%each%p%val
               end if
            else if ( type_var_info(i) == nf90_char ) then
               if ( trim(name_var_info(i)) == 'datetime' ) then
                  if ( plink%each%dhr > missing_r ) then  ! time drift
                     xdata(ityp)%xinfo_char(iloc(ityp),i) = plink%each%datetime
                  else
                     xdata(ityp)%xinfo_char(iloc(ityp),i) = plink%datetime
                  end if
               else if ( trim(name_var_info(i)) == 'station_id' ) then
                  xdata(ityp)%xinfo_char(iloc(ityp),i) = plink%stid
               end if
            end if ! type_var_info
         end do

         do i = 1, nvars(ityp)
            ivar = xdata(ityp)%var_idx(i)
            if ( name_var_met(ivar) == trim(var_prs) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = plink%each%p%val
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = plink%each%p%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = plink%each%p%err
            else if ( name_var_met(ivar) == trim(var_u) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = plink%each%u%val
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = plink%each%u%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = plink%each%u%err
            else if ( name_var_met(ivar) == trim(var_v) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = plink%each%v%val
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = plink%each%v%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = plink%each%v%err
            else if ( name_var_met(ivar) == trim(var_ts) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = plink%each%t%val
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = plink%each%t%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = plink%each%t%err
            else if ( name_var_met(ivar) == trim(var_tv) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = plink%each%tv%val
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = plink%each%tv%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = plink%each%tv%err
            else if ( name_var_met(ivar) == trim(var_q) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = plink%each%q%val
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = plink%each%q%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = plink%each%q%err
            else if ( name_var_met(ivar) == trim(var_ps) ) then
               xdata(ityp)%xfield(iloc(ityp),i)%val = plink%ps%val
               xdata(ityp)%xfield(iloc(ityp),i)%qm  = plink%ps%qm
               xdata(ityp)%xfield(iloc(ityp),i)%err = plink%ps%err
            end if
            xdata(ityp)%xfield(iloc(ityp),i)%rptype = plink%rptype
         end do
         plink % each => plink % each % next
      end do levels
      plink => plink%next
   end do reports

   ! done with plink
   ! release the linked list
   plink => phead
   do while ( associated(plink) )
      phead => plink%next
      do while ( associated(plink % each) )
         if ( associated(plink % each) ) deallocate (plink % each)
      end do
      nullify (plink%first)
      if ( associated (plink) ) deallocate (plink)
      plink => phead
   end do
   nullify (phead)

end subroutine sort_obs_conv

!--------------------------------------------------------------

subroutine filter_obs_conv

! refer to GSI/read_prepbufr.f90
!
! iuse information is extracted from global_convinfo.txt
! to do: come up with a better way to handle iuse

   implicit none

   logical         :: adjust_obserr
   integer(i_kind) :: zqm
   integer(i_kind) :: k
   integer(i_kind) :: iuse_ps
   integer(i_kind) :: iuse_uv
   integer(i_kind) :: iuse_t
   integer(i_kind) :: iuse_tv
   integer(i_kind) :: iuse_q
   real(r_kind)    :: hpa500 = 50000.0
   real(r_kind)    :: hpa100 = 10000.0
   real(r_kind)    :: inflate_factor = 1.2

   adjust_obserr = .false.

   write(*,*) '--- filtering conv obs ---'
   plink => phead
   link_loop: do while ( associated(plink) )

      plink % each => plink % first

      do while ( associated(plink % each) )

         if ( trim(plink%obtype) == 'sfc' ) then

            ! initialize as not used
            iuse_ps = ifalse

            if ( plink%rptype == 120 .or. &
                 plink%rptype == 180 .or. &
                 plink%rptype == 181 .or. &
                 plink%rptype == 182 .or. &
                 plink%rptype == 187 ) then

               iuse_ps = itrue

               zqm = plink % each % h % qm
               if ( zqm >= lim_qm .and. zqm /= 15 .and. zqm /= 9 ) then
                  plink % ps % qm = 9
               end if

               if ( plink % ps % val < hpa500 .or. plink % ps % qm >= lim_qm .or. &
                    zqm >= lim_qm ) then
                  iuse_ps = ifalse
               end if

               if ( adjust_obserr ) then
                  if ( plink % ps % qm == 3 .or. plink % ps % qm == 7 ) then
                     plink % ps % err = plink % ps % err * inflate_factor
                  end if
               end if
            end if

            if ( iuse_ps == ifalse .and. plink % ps % qm /= missing_i ) then
               plink % ps % qm = plink % ps % qm + not_use
            end if
         end if

         ! initialize as not used
         iuse_uv = ifalse
         iuse_t  = ifalse
         iuse_tv = ifalse
         iuse_q  = ifalse

         ! adjust observation height for wind reports
         if ( plink % rptype >= 280 .and. plink % rptype < 300 ) then
            plink % each % h % val = plink % elv + 10.0
            if ( plink % rptype == 280 ) then
               if ( plink % t29 == 522 .or. &
                    plink % t29 == 523 .or. &
                    plink % t29 == 531 ) then
                  plink % each % h % val = 20.0
               end if
            end if
            if ( plink % rptype == 282 ) then
               plink % each % h % val = plink % elv + 20.0
            end if
            if ( plink % rptype == 285 .or. &
                 plink % rptype == 286 .or. &
                 plink % rptype == 289 .or. &
                 plink % rptype == 290 ) then
               plink % each % h % val = plink % elv
               plink % elv = 0.0
            end if
         else
            if ( plink % rptype >= 221 .and. plink % rptype <= 229 ) then
               if ( abs(plink % each % h % val - missing_r) > 0.01 ) then
                  if ( plink % elv >= plink % each % h % val ) then
                     plink % each % h % val = plink % elv + 10.0
                  end if
               end if
            end if
         end if ! wind types

         if ( plink%rptype == 120 .or. &
              plink%rptype == 132 .or. &
              plink%rptype == 133 .or. &
              plink%rptype == 180 .or. &
              plink%rptype == 182 ) then
            if ( plink % each % q % qm < lim_qm ) iuse_q = itrue
         end if

         if ( plink%rptype == 120 .or. &
              plink%rptype == 130 .or. &
              plink%rptype == 132 .or. &
              plink%rptype == 133 .or. &
              plink%rptype == 180 .or. &
              plink%rptype == 182 ) then
            if ( plink % each % t  % qm < lim_qm ) iuse_t  = itrue
            if ( plink % each % tv % qm < lim_qm ) iuse_tv = itrue
         end if

         if ( plink%rptype == 220 .or. &
              plink%rptype == 221 .or. &
              plink%rptype == 223 .or. &
              plink%rptype == 224 .or. &
              plink%rptype == 229 .or. &
              plink%rptype == 230 .or. &
              plink%rptype == 231 .or. &
              plink%rptype == 232 .or. &
              plink%rptype == 233 .or. &
              plink%rptype == 280 .or. &
              plink%rptype == 282 .or. &
              plink%rptype == 289 .or. &
              plink%rptype == 290 ) then
            if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
         else if ( plink%rptype >= 241 .and. plink%rptype <= 260 ) then
            if ( plink%rptype == 242 ) then
               if ( plink % satid == 171 .or. &
                    plink % satid == 172 .or. &
                    plink % satid == 173 .or. &
                    plink % satid == 174 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 243 ) then
               if ( plink % satid ==  55 .or. &
                    plink % satid ==  70 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 244 ) then
               if ( plink % satid ==   3 .or. &
                    plink % satid ==   4 .or. &
                    plink % satid == 206 .or. &
                    plink % satid == 207 .or. &
                    plink % satid == 209 .or. &
                    plink % satid == 223 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 245 ) then
               if ( plink % satid == 259 .or. &
                    plink % satid == 270 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 246 ) then
               if ( plink % satid == 259 .or. &
                    plink % satid == 270 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 247 ) then
               if ( plink % satid == 259 .or. &
                    plink % satid == 270 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 250 ) then
               if ( plink % satid == 171 .or. &
                    plink % satid == 172 .or. &
                    plink % satid == 173 .or. &
                    plink % satid == 174 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 252 ) then
               if ( plink % satid == 171 .or. &
                    plink % satid == 172 .or. &
                    plink % satid == 173 .or. &
                    plink % satid == 174 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 253 ) then
               if ( plink % satid ==  55 .or. &
                    plink % satid ==  70 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 254 ) then
               if ( plink % satid ==  55 .or. &
                    plink % satid ==  70 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 257 ) then
               if ( plink % satid == 783 .or. &
                    plink % satid == 784 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 258 ) then
               if ( plink % satid == 783 .or. &
                    plink % satid == 784 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 259 ) then
               if ( plink % satid == 783 .or. &
                    plink % satid == 784 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            else if ( plink%rptype == 260 ) then
               if ( plink % satid == 224 ) then
                  if ( plink % each % u % qm < lim_qm .and. plink % each % v % qm < lim_qm ) iuse_uv = itrue
               end if
            end if
         end if

         if ( plink%rptype == 243 .or. &
              plink%rptype == 253 .or. &
              plink%rptype == 254 ) then
            if ( iuse_uv == itrue .and. plink % each % pccf < 85.0 ) then
               iuse_uv = ifalse
            end if
         end if

         if ( plink % each % p % qm >= lim_qm ) then
            iuse_uv = ifalse
            iuse_t  = ifalse
            iuse_tv = ifalse
            iuse_q  = ifalse
         end if

         if ( iuse_uv == ifalse .and. plink % each % u  % qm /= missing_i ) then
            plink % each % u % qm = plink % each % u % qm + not_use
         end if
         if ( iuse_uv == ifalse .and. plink % each % v  % qm /= missing_i ) then
            plink % each % v % qm = plink % each % v % qm + not_use
         end if
         if ( iuse_t  == ifalse .and. plink % each % t  % qm /= missing_i ) then
            plink % each % t % qm = plink % each % t % qm + not_use
         end if
         if ( iuse_tv == ifalse .and. plink % each % tv % qm /= missing_i ) then
            plink % each % tv % qm = plink % each % tv % qm + not_use
         end if
         if ( iuse_q  == ifalse .and. plink % each % q  % qm /= missing_i ) then
            plink % each % q % qm = plink % each % q % qm + not_use
         end if

         if ( adjust_obserr ) then
            if ( plink % each % p % val < hpa100 ) then
               if ( abs(plink % each%t%err - missing_r) > 0.01 ) then
                  plink % each % t  % err = plink % each % t  % err * inflate_factor
               end if
               if ( abs(plink % each%tv%err - missing_r) > 0.01 ) then
                  plink % each % tv % err = plink % each % tv % err * inflate_factor
               end if
            end if
            if ( plink % each % t % qm == 3 .or. &
                 plink % each % t % qm == 7  ) then
               plink % each % t % err = plink % each % t % err * inflate_factor
            end if
            if ( plink % each % tv % qm == 3 .or. &
                 plink % each % tv % qm == 7  ) then
               plink % each % tv % err = plink % each % tv % err * inflate_factor
            end if
            if ( plink % each % u % qm == 3 .or. &
                 plink % each % u % qm == 7  ) then
               plink % each % u % err = plink % each % u % err * inflate_factor
            end if
            if ( plink % each % v % qm == 3 .or. &
                 plink % each % v % qm == 7  ) then
               plink % each % v % err = plink % each % v % err * inflate_factor
            end if
            if ( plink % each % q % qm == 3 .or. &
                 plink % each % q % qm == 7  ) then
               plink % each % q % err = plink % each % q % err * inflate_factor
            end if
         end if

         plink % each => plink % each%next

      end do ! nlevels

      plink => plink%next
   end do link_loop

end subroutine filter_obs_conv

!--------------------------------------------------------------

subroutine init_field(self)
  implicit none
  class (field_type), intent(inout) :: self
  real(r_kind)    :: rfill
  integer(i_kind) :: ifill

  rfill = missing_r
  ifill = missing_i

  self % val = rfill
  self % qm  = ifill
  self % err = rfill
end subroutine init_field

!--------------------------------------------------------------

subroutine init_each_level(self)
  implicit none
  class (each_level_type), intent(inout) :: self
  real(r_kind)    :: rfill
  integer(i_kind) :: ifill

  rfill = missing_r
  ifill = missing_i

  self % lat  = rfill
  self % lon  = rfill
  self % dhr  = rfill
  self % pccf = rfill
  self % datetime = ''
  call self % h  % init()
  call self % u  % init()
  call self % v  % init()
  call self % t  % init()
  call self % tv % init()
  call self % p  % init()
  call self % q  % init()

end subroutine init_each_level

!--------------------------------------------------------------

subroutine init_report(self)
  implicit none
  class (report_conv), intent(inout) :: self
  real(r_kind)    :: rfill
  integer(i_kind) :: ifill

  rfill = missing_r
  ifill = missing_i

   self % msg_type  = ''
   self % stid      = ''
   self % datetime  = ''
   self % lon       = rfill
   self % lat       = rfill
   self % dhr       = rfill
   self % elv       = rfill
   self % rptype    = ifill
   self % t29       = ifill
   self % satid     = ifill

   call self % ps  % init()
   call self % slp % init()
   call self % pw  % init()

end subroutine init_report

!--------------------------------------------------------------

subroutine calc_qs (t, p, qs, wrt_ice)

! calculate saturation vapor pressure and saturation specific humidity
! given temperature and pressure

   implicit none

   real(r_kind), intent(in)  :: t, p
   real(r_kind), intent(out) :: qs
   logical,      intent(in), optional :: wrt_ice

   real(r_kind), parameter :: t_kelvin = 273.15
   real(r_kind), parameter :: t_triple = 273.16  ! triple point of water
   real(r_kind), parameter :: gas_constant = 287.0
   real(r_kind), parameter :: gas_constant_v = 461.6
   real(r_kind), parameter :: rd_over_rv = gas_constant/gas_constant_v
   real(r_kind), parameter :: rd_over_rv1 = 1.0 - rd_over_rv
   real(r_kind), parameter :: es_alpha = 611.2
   real(r_kind), parameter :: es_beta  = 17.67
   real(r_kind), parameter :: es_gamma = 243.5
   real(r_kind), parameter :: t_c_ref1 =   0.0   ! C
   real(r_kind), parameter :: t_c_ref2 = -20.0   ! C
   real(r_kind), parameter :: a0 = 6.107799961
   real(r_kind), parameter :: a1 = 4.436518521e-01
   real(r_kind), parameter :: a2 = 1.428945805e-02
   real(r_kind), parameter :: a3 = 2.650648471e-04
   real(r_kind), parameter :: a4 = 3.031240396e-06
   real(r_kind), parameter :: a5 = 2.034080948e-08
   real(r_kind), parameter :: a6 = 6.136820929e-11
   real(r_kind), parameter :: c1 = 9.09718
   real(r_kind), parameter :: c2 = 3.56654
   real(r_kind), parameter :: c3 = 0.876793
   real(r_kind), parameter :: c4 = 6.1071
   real(r_kind)            :: t_c, t1_c          ! T in degree C
   real(r_kind)            :: es
   logical                 :: ice

   ice = .false.
   if ( present(wrt_ice) ) then
      if ( wrt_ice ) ice = .true.
   end if

   t_c  = t - t_kelvin
   t1_c = t - t_triple

   ! Calculate saturation vapor pressure es

   if ( .not. ice ) then   ! over water only

      es = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )

   else   ! consider ice-water and ice effects

      if ( t1_c > t_c_ref1 ) then   ! vapor pressure over water
         es = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )
      else if ( (t1_c <= t_c_ref1) .and. (t1_c >= t_c_ref2) ) then   ! vapor pressure over water below 0C
         es = a0 + t1_c * (a1 + t1_c * (a2 + t1_c * (a3 + t1_c * (a4 + t1_c * (a5 + t1_c * a6)))))
         es = es * 100.0  ! to Pa
      else   ! vapor pressure over ice
         es = 10.0 ** ( -c1 * (t_triple / t - 1.0) - c2 * alog10(t_triple / t) +  &
                         c3 * (1.0 - t / t_triple) +      alog10(c4))
         es = es * 100.0  ! to Pa
      end if

   end if

   ! Calculate saturation specific humidity qs

   qs = rd_over_rv * es / ( p - rd_over_rv1 * es )

end subroutine calc_qs

end module prepbufr_mod

