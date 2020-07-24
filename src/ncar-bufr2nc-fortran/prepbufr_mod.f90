module prepbufr_mod

! adapated from WRFDA/var/da/da_obs_io/da_read_obs_bufr.inc

use kinds, only: r_kind, i_kind, r_double
use define_types_mod, only: nobtype, set_obtype_conv, obtype_list, xdata, &
   nvar_met, nvar_info, type_var_info, name_var_met, name_var_info, &
   t_kelvin, missing_r, missing_i, vflag, itrue, ifalse, nstring, ndatetime
use ufo_vars_mod, only: ufo_vars_getindex, var_prs, var_u, var_v, var_ts, var_tv, var_q, var_ps
use utils_mod, only: da_advance_time
use netcdf, only: nf90_int, nf90_float, nf90_char

implicit none
private
public  :: read_prepbufr
public  :: sort_obs_conv

! variables for storing data
type field_type
   real(r_kind)       :: val          ! observation value
   integer(i_kind)    :: qc           ! observation QC
   real(r_kind)       :: err          ! observational error
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
end type each_level_type

type datalink_conv
   ! data from BUFR file
   integer(i_kind)           :: t29         ! data dump report type
   integer(i_kind)           :: rptype      ! prepbufr report type
   character(len=nstring)    :: msg_type    ! BUFR message type name
   character(len=nstring)    :: stid        ! station identifier
   character(len=ndatetime)  :: datetime    ! ccyy-mm-ddThh:mm:ssZ
   integer(i_kind)           :: nlevels     ! number of levels
   real(r_kind)              :: lat         ! latitude in degree
   real(r_kind)              :: lon         ! longitude in degree
   real(r_kind)              :: elv         ! elevation in m
   real(r_kind)              :: dhr         ! obs time minus analysis time in hour
   type (field_type)         :: slp         ! sea level pressure
   type (field_type)         :: pw          ! precipitable water
   type (each_level_type), allocatable, dimension(:) :: each
   ! derived info
   character(len=nstring)    :: obtype      ! ob type, eg sonde, satwnd
   integer(i_kind)           :: obtype_idx  ! index of obtype in obtype_list
   type(datalink_conv), pointer :: next
end type datalink_conv

type(datalink_conv), pointer :: phead=>null(), plink=>null()

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
   logical           :: use_errtable, combine_mass_wind, do_tv_to_ts
   real(r_kind)      :: oetab(300,33,6)  ! 300 ob types, 33 levels (rows), 6 variables (columns)
   real(r_kind)      :: coef
   real(r_kind)      :: qs

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
   do_tv_to_ts       = .false.

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
   ! scan reports first

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

      call ufbint(iunit,hdr,7,1,iret2,hdstr)
      call ufbint(iunit,pmo,2,1,nlevels,'PMO PMQ')
      call ufbint(iunit,qms,8,255,nlevels,qmstr)
      call ufbint(iunit,oes,8,255,nlevels,oestr)
      call ufbint(iunit,pco,8,255,nlevels,pcstr)
      call ufbint(iunit,obs,8,255,nlevels,obstr)

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
      !if ( t29 == 61 .or. t29 == 66 .or. t29 == 72 ) cycle reports

      if ( abs(hdr(2)) > 360.0 .or. abs(hdr(3)) > 90.0 ) cycle reports

      write(dsec,'(i4,a1)') int(hdr(4)*60.0*60.0), 's' ! seconds
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

      if ( nlevels > 0 ) then
         plink % nlevels = nlevels
         allocate ( plink % each(1:nlevels) )
      end if

      ! initialize plink with missing values
      call fill_datalink (plink, missing_r, missing_i)

      r8sid = hdr(1)
      plink % msg_type(1:8) = subset
      plink % stid(1:5)     = csid(1:5)
      plink % rptype        = kx
      plink % t29           = t29
      plink % lon           = hdr(2)
      plink % lat           = hdr(3)
      plink % dhr           = hdr(4)    ! difference in hour
      plink % elv           = hdr(6)

      write(unit=plink%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
         iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'

      if ( pmo(1,1) < r8bfms ) then
         plink % slp % val = pmo(1,1)*100.0
         plink % slp % qc  = nint(pmo(2,1))
      end if
      if ( obs(7,1) < r8bfms ) then
         plink % pw % val = obs(7,1) * 0.1    ! convert to cm
         if ( qms(7,1) < r8bfms ) then
            plink % pw % qc  = nint(qms(7,1))
         end if
         if ( oes(7,1) < r8bfms ) then
            plink % pw % err = oes(7,1)
         end if
      end if

      loop_nlevels: do k = 1, nlevels

         if ( drift ) then
            if ( drf(1,k) < r8bfms ) plink % each (k) % lon = drf(1,k)
            if ( drf(2,k) < r8bfms ) plink % each (k) % lat = drf(2,k)
            if ( drf(3,k) < r8bfms ) plink % each (k) % dhr = drf(3,k)
         end if

         if ( obs(1,k) > 0.0 .and. obs(1,k) < r8bfms ) then
            plink % each (k) % p % val = obs(1,k)*100.0  ! convert to Pa
            if ( qms(1,k) < r8bfms ) then
               plink % each (k) % p % qc  = nint(qms(1,k))
            end if
            if ( oes(1,k) < r8bfms ) then
               plink % each (k) % p % err = oes(1,k)*100.0 ! convert to Pa
            end if
         end if

         ! set t units to Kelvin
         if (obs(3,k) > -200.0 .and. obs(3,k) < 300.0) then
            obs(3,k) = obs(3,k) + t_kelvin
         end if

         ! scale q and compute t from tv, if they aren't missing
         if (obs(2,k) > 0.0 .and. obs(2,k) < r8bfms) then
            obs(2,k) = obs(2,k)*1e-6
            tpc = nint(pco(3,k))
            if (obs(3,k) > -200.0 .and. obs(3,k) < 350.0) then
               if ( do_tv_to_ts .and. tpc >= 8 ) then   ! program code 008 VIRTMP
                  ! 0.61 is used in NCEP prepdata.f to convert T to Tv
                  obs(3,k) = obs(3,k) / (1.0 + 0.61 * obs(2,k))
               end if
            end if
         end if

         if ( do_tv_to_ts .or. tpc < 8 ) then   ! program code 008 VIRTMP
            ! sensible temperature
            if ( obs(3,k) < r8bfms ) then
               plink % each (k) % t % val = obs(3,k)
               if ( qms(3,k) < r8bfms ) then
                  plink % each (k) % t % qc  = nint(qms(3,k))
               end if
               if ( oes(3,k) < r8bfms ) then
                  plink % each (k) % t % err = oes(3,k)
               end if
            end if
         else
            ! virtual temperature
            if ( obs(3,k) < r8bfms ) then
               plink % each (k) % tv % val = obs(3,k)
               if ( qms(3,k) < r8bfms ) then
                  plink % each (k) % tv % qc  = nint(qms(3,k))
               end if
               if ( oes(3,k) < r8bfms ) then
                  plink % each (k) % tv % err = oes(3,k)
               end if
            end if
         end if

         if (obs(5,k) < r8bfms .and. obs(6,k) < r8bfms ) then
            plink % each (k) % u % val = obs(5,k)
            plink % each (k) % v % val = obs(6,k)
            if ( qms(5,k) < r8bfms ) then
               plink % each (k) % u % qc  = nint(qms(5,k))
            end if
            if ( qms(6,k) < r8bfms ) then
               plink % each (k) % v % qc  = nint(qms(6,k))
            else
               plink % each (k) % v % qc  = plink % each (k) % u % qc
            end if
            if ( oes(5,k) < r8bfms ) then
               plink % each (k) % u % err = oes(5,k)
            end if
            if ( oes(6,k) < r8bfms ) then
               plink % each (k) % v % err = oes(6,k)
            else
               plink % each (k) % v % err = plink % each (k) % u % err
            end if
         end if

         if ( obs(2,k)>0.0 .and. obs(2,k)<r8bfms ) then
            plink % each (k) % q % val = obs(2,k)
            plink % each (k) % q % qc  = nint(qms(2,k))
            if ( oes(2,k) < r8bfms ) then
               if ( abs(plink%each(k)%p%val - missing_r) > 0.01 ) then
                  ! use either t or tv (ignore the difference between t and tv) for calculating qs
                  if ( abs(plink%each(k)%t%val - missing_r) > 0.01 ) then
                     call calc_qs(plink%each(k)%t%val, plink%each(k)%p%val, qs)
                  else if ( abs(plink%each(k)%tv%val - missing_r) > 0.01 ) then
                     call calc_qs(plink%each(k)%tv%val, plink%each(k)%p%val, qs)
                  end if
                  plink % each (k) % q % err = oes(2,k)*10.0 ! convert to % from PREPBUFR percent divided by 10
                  plink % each (k) % q % err = plink % each (k) % q % err * qs * 0.01 ! convert from RH to q
               end if
            end if
         end if

         if ( obs(4,k) < r8bfms )then
            plink % each (k) % h % val = obs(4,k)
            if ( qms(4,k) < r8bfms ) then
               plink % each (k) % h % qc  = nint(qms(4,k))
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
      if ( .not. allocated(plink % each) ) then
         plink => plink%next
         cycle set_obtype_loop
      end if

      call set_obtype_conv(plink%t29, plink%obtype)

      ! find index of obtype in obtype_list
      plink%obtype_idx = ufo_vars_getindex(obtype_list, plink%obtype)
      if ( plink % obtype_idx > 0 ) then
         ! obtype assigned, advance ob counts
         nrecs(plink%obtype_idx) = nrecs(plink%obtype_idx) + 1
         nlocs(plink%obtype_idx) = nlocs(plink%obtype_idx) + plink%nlevels
      !else
         !found undefined t29=534 in file
         !write(*,*) 't29 = ', plink%t29
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

         !imask = type_var_info(:) == nf90_int
         !ninfo_int = count(imask)
         !allocate (xdata(i)%xinfo_int  (ninfo_int,   nlocs(i)))
         !imask = type_var_info(:) == nf90_float
         !ninfo_float = count(imask)
         !allocate (xdata(i)%xinfo_float(ninfo_float, nlocs(i)))
         !imask = type_var_info(:) == nf90_char
         !ninfo_char = count(imask)
         !allocate (xdata(i)%xinfo_char (ninfo_char,  nlocs(i)))

         ! easier to jsut allocate nvar_info for all types
         allocate (xdata(i)%xinfo_int  (nvar_info, nlocs(i)))
         allocate (xdata(i)%xinfo_float(nvar_info, nlocs(i)))
         allocate (xdata(i)%xinfo_char (nvar_info, nlocs(i)))
         xdata(i)%xinfo_int  (:,:) = missing_i
         xdata(i)%xinfo_float(:,:) = missing_r
         xdata(i)%xinfo_char (:,:) = ''

         if ( nvars(i) > 0 ) then
            allocate (xdata(i)%xfield(nvars(i), nlocs(i)))
            allocate (xdata(i)%var_idx(nvars(i)))
            xdata(i)%xfield(:,:)%val = missing_r ! initialize
            xdata(i)%xfield(:,:)%qc  = missing_i ! initialize
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
      if ( .not. allocated(plink%each) ) then
         plink => plink%next
         cycle reports
      end if
      ityp = plink%obtype_idx
      if ( ityp < 0 ) then
         plink => plink%next
         cycle reports
      end if
      if (  plink%nlevels < 1 ) then
         plink => plink%next
         cycle reports
      end if
      do k = 1, plink%nlevels
         iloc(ityp) = iloc(ityp) + 1

         do i = 1, nvar_info
            if ( type_var_info(i) == nf90_int ) then
               if ( name_var_info(i) == 'record_number' ) then
                  xdata(ityp)%xinfo_int(i,iloc(ityp)) = irec
               end if
            else if ( type_var_info(i) == nf90_float ) then
               if ( name_var_info(i) == 'time' ) then
                  xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%dhr
               else if ( trim(name_var_info(i)) == 'station_elevation' ) then
                  xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%elv
               else if ( trim(name_var_info(i)) == 'latitude' ) then
                  if ( plink%each(k)%lat > missing_r ) then  ! drift
                     xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%each(k)%lat
                     if ( plink%each(k)%dhr > missing_r ) then  ! time drift
                        xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%each(k)%dhr
                     end if
                  else
                     xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%lat
                  end if
               else if ( trim(name_var_info(i)) == 'longitude' ) then
                  if ( plink%each(k)%lon > missing_r ) then  ! drift
                     xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%each(k)%lon
                  else
                     xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%lon
                  end if
               else if ( trim(name_var_info(i)) == 'height' ) then
                  xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%each(k)%h%val
               else if ( trim(name_var_info(i)) == trim(var_prs) ) then
                  xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%each(k)%p%val
               end if
            else if ( type_var_info(i) == nf90_char ) then
               if ( trim(name_var_info(i)) == 'datetime' ) then
                  xdata(ityp)%xinfo_char(i,iloc(ityp)) = plink%datetime
               else if ( trim(name_var_info(i)) == 'station_id' ) then
                  xdata(ityp)%xinfo_char(i,iloc(ityp)) = plink%stid
               end if
            end if ! type_var_info
         end do

         do i = 1, nvars(ityp)
            ivar = xdata(ityp)%var_idx(i)
            if ( name_var_met(ivar) == trim(var_prs) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%each(k)%p%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%each(k)%p%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%each(k)%p%err
            else if ( name_var_met(ivar) == trim(var_u) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%each(k)%u%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%each(k)%u%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%each(k)%u%err
            else if ( name_var_met(ivar) == trim(var_v) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%each(k)%v%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%each(k)%v%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%each(k)%v%err
            else if ( name_var_met(ivar) == trim(var_ts) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%each(k)%t%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%each(k)%t%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%each(k)%t%err
            else if ( name_var_met(ivar) == trim(var_tv) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%each(k)%tv%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%each(k)%tv%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%each(k)%tv%err
            else if ( name_var_met(ivar) == trim(var_q) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%each(k)%q%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%each(k)%q%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%each(k)%q%err
            else if ( name_var_met(ivar) == trim(var_ps) ) then
               if ( plink%obtype == 'sfc' .and. k == 1 ) then
                  xdata(ityp)%xfield(i,iloc(ityp))%val = plink%each(k)%p%val
                  xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%each(k)%p%qc
                  xdata(ityp)%xfield(i,iloc(ityp))%err = plink%each(k)%p%err
               end if
            end if
            xdata(ityp)%xfield(i,iloc(ityp))%rptype = plink%rptype
         end do
      end do
      plink => plink%next
   end do reports

   ! done with plink
   ! release the linked list
   plink => phead
   do while ( associated(plink) )
      phead => plink%next
      if ( allocated (plink%each) ) deallocate (plink%each)
      if ( associated (plink) ) deallocate (plink)
      plink => phead
   end do
   nullify (phead)

end subroutine sort_obs_conv

!--------------------------------------------------------------

subroutine fill_datalink (datalink, rfill, ifill)

   implicit none

   type (datalink_conv), intent(inout) :: datalink
   real(r_kind),         intent(in)    :: rfill     ! fill value in real
   integer(i_kind),      intent(in)    :: ifill     ! fill value in integer

   datalink % msg_type  = ''
   datalink % stid      = ''
   datalink % datetime  = ''
   datalink % lon       = rfill
   datalink % lat       = rfill
   datalink % dhr       = rfill
   datalink % elv       = rfill
   datalink % rptype    = ifill
   datalink % t29       = ifill
   datalink % slp % val = rfill
   datalink % slp % qc  = ifill
   datalink % slp % err = rfill
   datalink % pw  % val = rfill
   datalink % pw  % qc  = ifill
   datalink % pw  % err = rfill

   if ( allocated (datalink % each) ) then
      datalink % each (:) % h % val  = rfill
      datalink % each (:) % h % qc   = ifill
      datalink % each (:) % h % err  = rfill

      datalink % each (:) % u % val  = rfill
      datalink % each (:) % u % qc   = ifill
      datalink % each (:) % u % err  = rfill

      datalink % each (:) % v % val  = rfill
      datalink % each (:) % v % qc   = ifill
      datalink % each (:) % v % err  = rfill

      datalink % each (:) % t % val  = rfill
      datalink % each (:) % t % qc   = ifill
      datalink % each (:) % t % err  = rfill

      datalink % each (:) % tv % val = rfill
      datalink % each (:) % tv % qc  = ifill
      datalink % each (:) % tv % err = rfill

      datalink % each (:) % p % val  = rfill
      datalink % each (:) % p % qc   = ifill
      datalink % each (:) % p % err  = rfill

      datalink % each (:) % q % val  = rfill
      datalink % each (:) % q % qc   = ifill
      datalink % each (:) % q % err  = rfill

      datalink % each (:) % lat      = rfill
      datalink % each (:) % lon      = rfill
   end if

end subroutine fill_datalink

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

