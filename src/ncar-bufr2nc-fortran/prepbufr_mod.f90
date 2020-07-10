module prepbufr_mod

! adapated from WRFDA/var/da/da_obs_io/da_read_obs_bufr.inc

use kinds, only: r_kind,i_kind,r_single,r_double
use define_types_mod, only: nobtype, set_obtype, obtype_list, xdata, &
   nvar_met, nvar_info_int, nvar_info_float, nvar_info_char20, nvar_info_char50, &
   name_var_met, name_var_info_int, name_var_info_float, name_var_info_char20, &
   name_var_info_char50, head, plink, t_kelvin, missing_r, missing_i, &
   vflag, itrue, ifalse
use ufo_vars_mod, only: ufo_vars_getindex, var_prs, var_u, var_v, var_ts, var_q, var_ps
use utils_mod, only: da_advance_time

implicit none
private
public  :: read_prepbufr
public  :: sort_convobs
public  :: cdate_bfile

character(len=10) :: cdate_bfile  ! date (ccyymmddhh) in bufr file

contains

!--------------------------------------------------------------

subroutine read_prepbufr(filename)

   implicit none

   character (len=*), intent(in) :: filename

   real(r_kind), parameter  :: r8bfms = 9.0D08  ! BUFR missing value threshold

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

   character(len=14) :: cdate, dmn, obs_date
   integer(i_kind)   :: idate, idate2
   integer(i_kind)   :: nlevels, nlevels2, lv1, lv2
   integer(i_kind)   :: iyear, imonth, iday, ihour, imin
   integer(i_kind)   :: num_report_infile
   integer(i_kind)   :: iret, iret2, iost, n, i, j, k, i1, i2
   integer(i_kind)   :: kx, t29
   integer(i_kind)   :: tpc
   integer(i_kind)   :: iunit, junit, itype, ivar
   logical           :: use_errtable, combine_mass_wind
   real(r_kind)      :: oetab(300,33,6)  ! 300 ob types, 33 levels (rows), 6 variables (columns)
   real(r_kind)      :: coef

   write(*,*) '--- reading prepbufr...'
   hdstr='SID XOB YOB DHR TYP ELV T29'
   obstr='POB QOB TOB ZOB UOB VOB PWO CAT' ! observation
   qmstr='PQM QQM TQM ZQM WQM NUL PWQ NUL' ! quality marker
   oestr='POE QOE TOE NUL WOE NUL PWE NUL' ! observation error
   pcstr='PPC QPC TPC ZPC WPC NUL PWP NUL' ! program code
   drstr='XDR YDR HRDR                   ' ! balloon drift code

   ! initialize variables

   if ( .not. associated(head) ) then
      nullify ( head )
      allocate ( head )
      nullify ( head%next )
   end if

   use_errtable = .false.
   combine_mass_wind = .false.

   num_report_infile  = 0

   iunit = 96
   junit = 97

   !hcl-todo: inquire file in the main program
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
   !rewind(iunit)

   write(unit=*,fmt='(a,i10)') ' prepbufr file date is: ', idate

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

      if (.not. associated(plink)) then
         plink => head
      else
         allocate ( plink%next )
         plink => plink%next
         nullify ( plink%next )
      end if

      call ufbint(iunit,hdr,7,1,iret2,hdstr)
      call ufbint(iunit,pmo,2,1,nlevels,'PMO PMQ')
      call ufbint(iunit,qms,8,255,nlevels,qmstr)
      call ufbint(iunit,oes,8,255,nlevels,oestr)
      call ufbint(iunit,pco,8,255,nlevels,pcstr)
      call ufbint(iunit,obs,8,255,nlevels,obstr)

      r8sid = hdr(1)
      plink % platform % info % name(1:8)  = subset
      plink % platform % info % name(9:40) = '                                '
      plink % platform % info % id(1:5)    = csid(1:5)
      plink % platform % info % id(6:40)   = '                                   '
      plink % platform % info % dhr        = hdr(4)    ! difference in hour
      plink % platform % info % elv        = hdr(6)
      plink % platform % info % lon        = hdr(2)
      plink % platform % info % lat        = hdr(3)

      ! Restrict to a range of reports, useful for debugging
      !if (num_report < report_start) cycle reports
      !if (num_report > report_end)  exit reports

      ! check date
      write(cdate,'(i10)') idate
      write(dmn,'(i4,a1)') int(plink%platform%info%dhr*60.0), 'm'
      call da_advance_time (cdate(1:10), trim(dmn), obs_date)
      if ( obs_date(13:14) /= '00' ) then
         write(0,*) 'wrong date: ', trim(cdate), trim(dmn), trim(obs_date)
         stop
      else
         read (obs_date(1:12),'(i4,4i2)') iyear, imonth, iday, ihour, imin
      end if
      write(unit=plink%platform%info%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
         iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', 0, 'Z'

      cdate_bfile = cdate(1:10)
      t29 = int(0.1 + hdr(7))
      kx  = int(0.1+hdr(5))

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

      plink % nlevels = nlevels
      plink % rptype  = kx
      plink % t29     = t29

      allocate ( plink%platform%each(1:nlevels) )

      plink % platform % slp % val   = missing_r
      plink % platform % slp % qc    = missing_i
      plink % platform % slp % err   = missing_r
      plink % platform % pw  % val   = missing_r
      plink % platform % pw  % qc    = missing_i
      plink % platform % pw  % err   = missing_r
      if ( pmo(1,1) < r8bfms ) then
         plink % platform % slp % val = pmo(1,1)*100.0
         plink % platform % slp % qc  = nint(pmo(2,1))
      end if
      if ( obs(7,1) < r8bfms ) then
         plink % platform % pw % val = obs(7,1) * 0.1    ! convert to cm
         if ( qms(7,1) < r8bfms ) then
            plink % platform % pw % qc  = nint(qms(7,1))
         end if
         if ( oes(7,1) < r8bfms ) then
            plink % platform % pw % err = oes(7,1)
         end if
      end if
      do i = 1, nlevels
         plink % platform % each (i) % h % val = missing_r
         plink % platform % each (i) % h % qc  = missing_i
         plink % platform % each (i) % h % err = missing_r

         plink % platform % each (i) % u % val = missing_r
         plink % platform % each (i) % u % qc  = missing_i
         plink % platform % each (i) % u % err = missing_r

         plink % platform % each (i) % v = plink % platform % each (i) % u

         plink % platform % each (i) % t % val = missing_r
         plink % platform % each (i) % t % qc  = missing_i
         plink % platform % each (i) % t % err = missing_r

         plink % platform % each (i) % p % val = missing_r
         plink % platform % each (i) % p % qc  = missing_i
         plink % platform % each (i) % p % err = missing_r

         plink % platform % each (i) % q % val = missing_r
         plink % platform % each (i) % q % qc  = missing_i
         plink % platform % each (i) % q % err = missing_r

         plink % platform % each (i) % lat     = missing_r
         plink % platform % each (i) % lon     = missing_r
      end do

      do k = 1, nlevels

         if ( drift ) then
            if ( drf(1,k) < r8bfms ) plink % platform % each (k) % lon = drf(1,k)
            if ( drf(2,k) < r8bfms ) plink % platform % each (k) % lat = drf(2,k)
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
               if ( tpc >= 8 ) then   ! program code 008 VIRTMP
                  ! 0.61 is used in NCEP prepdata.f to convert T to Tv
                  obs(3,k) = obs(3,k) / (1.0 + 0.61 * obs(2,k))
               end if
            end if
         end if

         !Not currently used
         !cat=nint(obs(8,k))

         if ( obs(3,k) < r8bfms ) then
            plink % platform % each (k) % t % val = obs(3,k)
            if ( qms(3,k) < r8bfms ) then
               plink % platform % each (k) % t % qc  = nint(qms(3,k))
            end if
            if ( oes(3,k) < r8bfms ) then
               plink % platform % each (k) % t % err = oes(3,k)
            end if
         end if

         if (obs(5,k) < r8bfms .and. obs(6,k) < r8bfms ) then
            plink % platform % each (k) % u % val = obs(5,k)
            plink % platform % each (k) % v % val = obs(6,k)
            if ( qms(5,k) < r8bfms ) then
               plink % platform % each (k) % u % qc  = nint(qms(5,k))
            end if
            if ( qms(6,k) < r8bfms ) then
               plink % platform % each (k) % v % qc  = nint(qms(6,k))
            else
               plink % platform % each (k) % v % qc  = plink % platform % each (k) % u % qc
            end if
            if ( oes(5,k) < r8bfms ) then
               plink % platform % each (k) % u % err = oes(5,k)
            end if
            if ( oes(6,k) < r8bfms ) then
               plink % platform % each (k) % v % err = oes(6,k)
            else
               plink % platform % each (k) % v % err = plink % platform % each (k) % u % err
            end if
         end if
         if ( obs(2,k)>0.0 .and. obs(2,k)<r8bfms ) then
            plink % platform % each (k) % q % val = obs(2,k)
            plink % platform % each (k) % q % qc  = nint(qms(2,k))
            ! leave q_err as missing because of lacking rh_err to q_err conversion
            !if ( oes(2,k) < r8bfms ) then
            !   plink % platform % each (k) % q % err = oes(2,k)*10.0 ! convert to % from PREPBUFR percent divided by 10
            !end if
         end if

         if ( obs(4,k) < r8bfms )then
            plink % platform % each (k) % h % val = obs(4,k)
            if ( qms(4,k) < r8bfms ) then
               plink % platform % each (k) % h % qc  = nint(qms(4,k))
            end if
         end if

         if ( obs(1,k) > 0.0 .and. obs(1,k) < r8bfms ) then
            plink % platform % each (k) % p % val = obs(1,k)*100.0  ! convert to Pa
            if ( qms(1,k) < r8bfms ) then
               plink % platform % each (k) % p % qc  = nint(qms(1,k))
            end if
            if ( oes(1,k) < r8bfms ) then
               plink % platform % each (k) % p % err = oes(1,k)*100.0 ! convert to Pa
            end if
         end if
      end do ! nlevels

   end do reports

   write(*,*) 'num_report_infile = ', num_report_infile

   call closbf(iunit)
   close(iunit)
   if ( use_errtable ) then
      close(junit)
   end if

end subroutine read_prepbufr

subroutine sort_convobs

   implicit none

   integer(i_kind)                      :: i, iv, k
   integer(i_kind)                      :: ityp, irec, ivar
   integer(i_kind), dimension(nobtype)  :: nrecs
   integer(i_kind), dimension(nobtype)  :: nlocs
   integer(i_kind), dimension(nobtype)  :: nvar
   integer(i_kind), dimension(nobtype)  :: iloc
   character(len=12)                    :: obtype
   logical,         dimension(nvar_met) :: mask ! for counting available variables for one obtype

   nrecs(:) = 0
   nlocs(:) = 0
   nvar(:)  = 0

   write(*,*) '--- sorting conv obs...'

   ! set obtype from dump data type t29 and
   ! and count the numbers
   plink => head
   set_obtype_loop: do while ( associated(plink) )
      if ( .not. allocated(plink%platform%each) ) then
         plink => plink%next
         cycle set_obtype_loop
      end if

      call set_obtype(plink%t29, plink%obtype)

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

   write(*,*) 'num_report_decoded = ', sum(nrecs(:))
   do i = 1, nobtype
      write(*,*) obtype_list(i), nrecs(i), nlocs(i)
   end do

   ! allocate data arrays with the just counted numbers
   allocate (xdata(nobtype))
   do i = 1, nobtype
      xdata(i) % nrecs = nrecs(i)
      xdata(i) % nlocs = nlocs(i)

      if ( nlocs(i) > 0 ) then
         allocate (xdata(i)%xinfo_float(nvar_info_float, nlocs(i)))
         allocate (xdata(i)%xinfo_int  (nvar_info_int,   nlocs(i)))
         allocate (xdata(i)%xinfo_char20(nvar_info_char20,nlocs(i)))
         allocate (xdata(i)%xinfo_char50(nvar_info_char50,nlocs(i)))

         mask = vflag(:,i) == itrue
         nvar(i) = count(mask)
         if ( nvar(i) > 0 ) then
            allocate (xdata(i)%xfield(nvar(i), nlocs(i)))
            allocate (xdata(i)%var_idx(nvar(i)))
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

   plink => head
   reports: do while ( associated(plink) )
      irec = irec + 1
      if ( .not. allocated(plink%platform%each) ) then
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

         do i = 1, nvar_info_int
            if ( trim(name_var_info_int(i)) == 'record_number' ) then
               xdata(ityp)%xinfo_int(i,iloc(ityp)) = irec
            end if
         end do
         do i = 1, nvar_info_float
            if ( name_var_info_float(i) == 'time' ) then
               xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%platform%info%dhr
            else if ( trim(name_var_info_float(i)) == 'station_elevation' ) then
               xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%platform%info%elv
            else if ( trim(name_var_info_float(i)) == 'latitude' ) then
               if ( plink%platform%each(k)%lat > missing_r ) then  ! drift
                  xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%platform%each(k)%lat
               else
                  xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%platform%info%lat
               end if
            else if ( trim(name_var_info_float(i)) == 'longitude' ) then
               if ( plink%platform%each(k)%lon > missing_r ) then  ! drift
                  xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%platform%each(k)%lon
               else
                  xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%platform%info%lon
               end if
            else if ( trim(name_var_info_float(i)) == 'height' ) then
               xdata(ityp)%xinfo_float(i,iloc(ityp)) = plink%platform%each(k)%h%val
            end if
         end do
         do i = 1, nvar_info_char20
            if ( trim(name_var_info_char20(i)) == 'datetime' ) then
               xdata(ityp)%xinfo_char20(i,iloc(ityp)) = plink%platform%info%datetime
            end if
         end do
         do i = 1, nvar_info_char50
            if ( trim(name_var_info_char50(i)) == 'station_id' ) then
               xdata(ityp)%xinfo_char50(i,iloc(ityp)) = plink%platform%info%id
            end if
         end do

         do i = 1, nvar(ityp)
            ivar = xdata(ityp)%var_idx(i)
            if ( name_var_met(ivar) == trim(var_prs) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%platform%each(k)%p%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%platform%each(k)%p%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%platform%each(k)%p%err
            else if ( name_var_met(ivar) == trim(var_u) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%platform%each(k)%u%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%platform%each(k)%u%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%platform%each(k)%u%err
            else if ( name_var_met(ivar) == trim(var_v) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%platform%each(k)%v%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%platform%each(k)%v%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%platform%each(k)%v%err
            else if ( name_var_met(ivar) == trim(var_ts) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%platform%each(k)%t%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%platform%each(k)%t%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%platform%each(k)%t%err
            else if ( name_var_met(ivar) == trim(var_q) ) then
               xdata(ityp)%xfield(i,iloc(ityp))%val = plink%platform%each(k)%q%val
               xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%platform%each(k)%q%qc
               xdata(ityp)%xfield(i,iloc(ityp))%err = plink%platform%each(k)%q%err
            else if ( name_var_met(ivar) == trim(var_ps) ) then
               if ( plink%obtype == 'sfc' .and. k == 1 ) then
                  xdata(ityp)%xfield(i,iloc(ityp))%val = plink%platform%each(k)%p%val
                  xdata(ityp)%xfield(i,iloc(ityp))%qc  = plink%platform%each(k)%p%qc
                  xdata(ityp)%xfield(i,iloc(ityp))%err = plink%platform%each(k)%p%err
               end if
            end if
            xdata(ityp)%xfield(i,iloc(ityp))%rptype = plink%rptype
         end do
      end do
      plink => plink%next
   end do reports

   ! done with plink
   ! release the linked list
   plink => head
   do while ( associated(plink) )
      head => plink%next
      if ( allocated (plink%platform%each) ) deallocate (plink%platform%each)
      if ( associated (plink) ) deallocate (plink)
      plink => head
   end do
   nullify (head)

end subroutine sort_convobs

end module prepbufr_mod

