!!!--------  README --------------------------------------------------------------------
!  this is a temporary routine to generate netcdf4 file
!  for jedi/ufo/gnssro/ operator test
!  IODA VERSION 2
!  Copyright UCAR 2022
!  Author: Hailing Zhang

module gnssro_bufr2ioda
   use netcdf
   use define_mod, only: ndatetime, output_info_type
   implicit none
   private
   public :: read_write_gnssro

   ! Type definitions. Note: we should eventually use the ones from the kinds module (whose names are not identical)
   integer, parameter :: i_kind  = selected_int_kind(8)  ! 4
   integer, parameter :: i_64    = selected_int_kind(10)  ! 8
   integer, parameter :: r_kind  = selected_real_kind(15)  ! 8
   real(r_kind), parameter :: r_missing = huge(1_i_kind)  ! perhaps this is a typo and should be huge(1.0_r_kind)?
   integer(i_kind), parameter :: i_missing = huge(1_i_kind)
   integer(i_64), parameter :: i64_missing = huge(1_i_64)

   ! module parameters
   integer(i_kind), parameter :: n1ahdr = 13, maxlevs = 500, datelength = 10, mxib = 31
   character (*), parameter :: dateformat = '(i10.10)' ! the number of positions corresponds to datelength above 
   character (*), parameter :: hdr1a = 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID SIID PTID GEODU SCLF OGCE'
   character (*), parameter :: nemo = 'QFRO'
   logical, parameter :: verbose = .false.

   ! output obs data stucture
   type gnssro_type
      integer(i_kind), allocatable, dimension(:) :: said
      integer(i_kind), allocatable, dimension(:) :: siid
      integer(i_kind), allocatable, dimension(:) :: sclf
      integer(i_kind), allocatable, dimension(:) :: ptid
      integer(i_kind), allocatable, dimension(:) :: recn
      integer(i_kind), allocatable, dimension(:) :: asce
      integer(i_kind), allocatable, dimension(:) :: ogce
      real(r_kind), allocatable, dimension(:) :: time
      integer(i_64),allocatable, dimension(:) :: epochtime
      character(len = ndatetime), allocatable, dimension(:) :: datetime
      real(r_kind), allocatable, dimension(:) :: lat
      real(r_kind), allocatable, dimension(:) :: lon
      real(r_kind), allocatable, dimension(:) :: rfict
      real(r_kind), allocatable, dimension(:) :: azim
      real(r_kind), allocatable, dimension(:) :: geoid
      real(r_kind), allocatable, dimension(:) :: msl_alt
      real(r_kind), allocatable, dimension(:) :: ref
      real(r_kind), allocatable, dimension(:) :: refoe_gsi
      real(r_kind), allocatable, dimension(:) :: bend_ang
      real(r_kind), allocatable, dimension(:) :: impact_para
      real(r_kind), allocatable, dimension(:) :: bndoe_gsi
      real(r_kind), allocatable, dimension(:) :: gstime
      integer(i_kind), allocatable, dimension(:) :: idx_window
   end type gnssro_type

   ! bufr information data structure
   type bufr_info_type
      character(len = datelength) :: analysis_time
      integer(i_kind) :: analysis_epochtime_in_mins
      integer(i_kind) :: nobs_max  ! estimate for maximum number of observations contained in bufr file
      integer(i_kind) :: nobs  ! effective number of observatios contained in bufr file, nobs <= nobs_max
   end type

contains

   subroutine read_write_gnssro(input_file_name, file_output_info)
      character(len = *), intent(in) :: input_file_name
      type(output_info_type), intent(in) :: file_output_info
      type(gnssro_type) :: gnssro_data
      type(bufr_info_type) :: gnssro_bufr_info
      integer :: idx_window
      character(:), allocatable :: output_file_name
      call get_buffer_information(trim(adjustl(input_file_name)), gnssro_bufr_info)
      call allocate_gnssro_data_array(gnssro_data, gnssro_bufr_info)
      call read_gnssro_data(trim(adjustl(input_file_name)), gnssro_data, gnssro_bufr_info)
      call assign_gnssro_data_to_time_window(gnssro_data, gnssro_bufr_info, file_output_info)
      do idx_window = 1, file_output_info%n_windows
         call write_gnssro_data(gnssro_data, gnssro_bufr_info, file_output_info, idx_window)
      enddo
      call deallocate_gnssro_data_array(gnssro_data)
   end subroutine read_write_gnssro


   subroutine get_buffer_information(input_file_name, gnssro_bufr_info)
      character(len = *), intent(in) :: input_file_name
      type(bufr_info_type), intent(out) :: gnssro_bufr_info
      integer(i_kind), parameter :: lnbufr = 10  ! the bufr library expects a unit number in [0-99] -> can't use newunit
      character(len = 8) :: subset
      integer(i_kind) :: idate, iret, levs
      integer, dimension(6) :: iadate5
      integer(i_kind) :: maxobs, ireadmg, ireadsb
      real(r_kind), dimension(n1ahdr) :: bfr1ahdr
      real(r_kind), dimension(50, maxlevs) :: data1b
      ! open file and connect to buffer library
      open(unit = lnbufr, file = input_file_name, form = 'unformatted')
      call openbf(lnbufr, 'IN', lnbufr)
      call datelen(datelength)
      ! obtain analysis time
      call readmg(lnbufr, subset, idate, iret)
      if (iret /= 0) then
         write(6, *) 'READ_GNSSRO: can not open gnssro file!'
         stop
      end if
      write(*, fmt = '(a,i10)') input_file_name // ' file date is: ', idate
      iadate5(1) = idate / 1000000
      iadate5(2) = (idate - iadate5(1) * 1000000) / 10000
      iadate5(3) = (idate - iadate5(1) * 1000000 - iadate5(2) * 10000) / 100
      iadate5(4) = idate - iadate5(1) * 1000000 - iadate5(2) * 10000 - iadate5(3) * 100
      iadate5(5) = 0
      call w3fs21(iadate5, gnssro_bufr_info%analysis_epochtime_in_mins)
      write(gnssro_bufr_info%analysis_time, dateformat) idate
      ! estimate total number of observations
      maxobs = 0
      do while (ireadmg(lnbufr, subset, idate) == 0)
         do while(ireadsb(lnbufr) == 0)
            call ufbint(lnbufr, bfr1ahdr, n1ahdr, 1, iret, hdr1a)
            call ufbseq(lnbufr, data1b, 50, maxlevs, levs, 'ROSEQ1')
            maxobs = maxobs + levs
         enddo
      end do
      call closbf(lnbufr)  ! this also closes the frotran file unit
      gnssro_bufr_info%nobs_max = maxobs
   end subroutine


   subroutine allocate_gnssro_data_array(gnssro_data, gnssro_bufr_info)
      type(gnssro_type), intent(out) :: gnssro_data  ! intent(out) automatically deallocates previously allocated arguments
      type(bufr_info_type), intent(in) :: gnssro_bufr_info
      integer(i_kind) :: maxobs
      maxobs = gnssro_bufr_info%nobs_max
      allocate(gnssro_data%said(maxobs))
      allocate(gnssro_data%siid(maxobs))
      allocate(gnssro_data%sclf(maxobs))
      allocate(gnssro_data%ptid(maxobs))
      allocate(gnssro_data%recn(maxobs))
      allocate(gnssro_data%asce(maxobs))
      allocate(gnssro_data%ogce(maxobs))
      allocate(gnssro_data%time(maxobs))
      allocate(gnssro_data%epochtime(maxobs))
      allocate(gnssro_data%datetime(maxobs))
      allocate(gnssro_data%lat(maxobs))
      allocate(gnssro_data%lon(maxobs))
      allocate(gnssro_data%rfict(maxobs))
      allocate(gnssro_data%azim(maxobs))
      allocate(gnssro_data%geoid(maxobs))
      allocate(gnssro_data%msl_alt(maxobs))
      allocate(gnssro_data%ref(maxobs))
      allocate(gnssro_data%refoe_gsi(maxobs))
      allocate(gnssro_data%bend_ang(maxobs))
      allocate(gnssro_data%impact_para(maxobs))
      allocate(gnssro_data%bndoe_gsi(maxobs))
      allocate(gnssro_data%gstime(maxobs))
      allocate(gnssro_data%idx_window(maxobs))
   end subroutine


   subroutine read_gnssro_data(input_file_name, gnssro_data, gnssro_bufr_info)
      use utils_mod, only: get_julian_time
      character(len = *), intent(in) :: input_file_name
      type(gnssro_type), intent(inout) :: gnssro_data
      type(bufr_info_type), intent(inout) :: gnssro_bufr_info
      integer(i_kind), parameter :: lnbufr = 10
      character(len = 8) :: subset
      integer(i_kind) :: idate, iret
      integer(i_kind) :: ireadmg, ireadsb
      real(r_kind), dimension(n1ahdr) :: bfr1ahdr
      real(r_kind), dimension(1) :: qfro
      integer(i_kind), dimension(6) :: idate5
      real(r_kind) :: pcc, roc, geoid, timeo, gstime
      integer(i_kind) :: said, siid, ptid, sclf, ogce, minobs, nib, asce
      integer(i_64) :: epochtime
      character(len = ndatetime) :: datetime
      integer :: refflag, bendflag
      integer(i_kind), dimension(mxib) :: ibit
      integer(i_kind) :: i, k, m
      real(r_kind), dimension(maxlevs) :: nreps_this_ROSEQ2
      integer(i_kind) :: nreps_ROSEQ1, levs, levsr, nrec, ndata, ndata0, nreps_ROSEQ2_int
      real(r_kind), dimension(50, maxlevs) :: data1b, data2a
      real(r_kind)  :: rlat, rlon, azim, height, ref, ref_error, ref_pccf
      real(r_kind) :: freq_chk, freq, impact, bend, bend_error, bend_pccf, obsErr
      logical :: good
      logical, parameter :: GlobalModel = .true.  ! temporary

      ! initialize counters
      ndata = 0
      nrec = 0
      ! open file and attach buffer library to it
      open(unit = lnbufr, file = input_file_name, form = 'unformatted')
      call openbf(lnbufr, 'IN', lnbufr)
      call datelen(datelength)
      call readmg(lnbufr, subset, idate, iret)
      ! read data
      do while(ireadmg(lnbufr, subset, idate) == 0)
         read_loop:  do while(ireadsb(lnbufr) == 0)
            ! Read / decode data in subset (profile)
            call ufbint(lnbufr, bfr1ahdr, n1ahdr, 1, iret, hdr1a)
            call ufbint(lnbufr, qfro, 1, 1, iret, nemo)
            ! observation time in minutes
            idate5(1) = bfr1ahdr(1)  ! year
            idate5(2) = bfr1ahdr(2)  ! month
            idate5(3) = bfr1ahdr(3)  ! day
            idate5(4) = bfr1ahdr(4)  ! hour
            idate5(5) = bfr1ahdr(5)  ! minute
            idate5(6) = 0  ! seconds
            pcc  = bfr1ahdr(6)  ! profile per cent confidence
            roc  = bfr1ahdr(7)  ! Earth local radius of curvature
            said = bfr1ahdr(8)  ! Satellite identifier
            siid = bfr1ahdr(9)  ! Satellite instrument
            ptid = bfr1ahdr(10)  ! Platform transmitter ID number
            geoid= bfr1ahdr(11)  ! Geoid undulation
            sclf = bfr1ahdr(12)  ! Satellite classification
            ogce = bfr1ahdr(13)  ! Identification of originating/generating centre
            call w3fs21(idate5, minobs)
            timeo = real(minobs - gnssro_bufr_info%analysis_epochtime_in_mins, r_kind) / 60.0
            call get_julian_time(idate5(1), idate5(2), idate5(3), idate5(4), idate5(5), idate5(6), gstime, epochtime)
            write(datetime, '(i4, a, i2.2, a, i2.2, a, i2.2, a, i2.2, a, i2.2, a)')  &
               idate5(1), '-', idate5(2), '-', idate5(3), 'T', idate5(4), ':', idate5(5), ':', idate5(6), 'Z'
            ! check if values are in valid range
            ! earth radius of curvature
            if (roc > 6450000.0_r_kind .or. roc < 6250000.0_r_kind .or. geoid > 200_r_kind .or. geoid < -200._r_kind) then
               if (verbose) write(6, *) 'READ_GNSSRO: profile fails georeality check, skip this report'
               cycle read_loop
            endif
            ! profile check: (1) CDAAC processing - cosmic-1, cosmic-2, sacc, cnofs, kompsat5
            if (((said >= 740) .and. (said <= 745)) .or. ((said >= 750) .and. (said <= 755)) &
               .or. (said == 820) .or. (said == 786) .or. (said == 825) .or. ogce == 60) then  !CDAAC processing
               if(pcc == 0.0) then
                  if (verbose) write(6, *) 'READ_GNSSRO: bad profile 0.0% confidence said=', said, 'ptid=', ptid, &
                     ' SKIP this report'
                  cycle read_loop
               endif
            endif
            ! profile check: (2) GRAS SAF processing - metopa-c, oceansat2, megha-tropiques, sacd 
            bendflag = 0
            refflag  = 0
            if ((said >= 3 .and. said <= 5) .or. (said == 421) .or. (said == 440) .or. (said == 821)) then 
               call upftbv(lnbufr, nemo, qfro, mxib, ibit, nib)
               if(nib > 0) then
                  do i = 1, nib
                     if(ibit(i) == 5) then  ! bending angle
                        bendflag = 1
                        if (verbose) write(6, *) 'READ_GNSSRO: bad profile said=', said, 'ptid=', ptid, ' SKIP this report'
                        cycle read_loop
                     endif
                     if(ibit(i)== 6) then  ! refractivity
                        refflag = 1
                        exit
                     endif
                  enddo
               endif 
            endif
            ! check associated with ascending flag
            asce = 0
            call upftbv(lnbufr, nemo, qfro, mxib, ibit, nib)
            if (nib > 0) then
               do i = 1, nib
                  if(ibit(i) == 3) then
                     asce = 1
                     exit
                  endif
               enddo
            end if
            ! read further data
            call ufbint(lnbufr, nreps_this_ROSEQ2, 1, maxlevs, nreps_ROSEQ1, '{ROSEQ2}')
            call ufbseq(lnbufr, data1b, 50, maxlevs,levs, 'ROSEQ1') 
            call ufbseq(lnbufr, data2a, 50, maxlevs, levsr, 'ROSEQ3') ! refractivity
            nrec = nrec + 1
            ndata0 = ndata
            do k = 1, levs
               rlat = data1b(1, k)  ! earth relative latitude (degrees)
               rlon = data1b(2, k)  ! earth relative longitude (degrees)
               azim = data1b(3, k)
               height = data2a(1, k)
               ref = data2a(2, k)
               ref_error = data2a(4, k)
               ref_pccf = data2a(6, k)
               ! Loop over number of replications of ROSEQ2 nested inside this particular replication of ROSEQ1
               nreps_ROSEQ2_int = nreps_this_ROSEQ2(k)
               do i = 1, nreps_ROSEQ2_int
                  m = (6 * i) - 2
                  freq_chk = data1b(m, k) ! frequency (hertz)
                  if(nint(freq_chk) .ne. 0) cycle ! do not want non-zero freq., go on to next replication of ROSEQ2
                  freq = data1b(m, k)
                  impact = data1b(m + 1, k)  ! impact parameter (m)
                  bend = data1b(m + 2, k)  ! bending angle (rad)
                  bend_error = data1b(m + 4, k)  ! RMSE in bending angle (rad)
               enddo
               bend_pccf = data1b((6 * nreps_ROSEQ2_int) + 4, k)  ! percent confidence for this ROSEQ1 replication
               ! check if newly read quantities are in valid range
               ! height and latitude / longitude
               good = .true. 
               if (height < 0._r_kind .or. height > 100000._r_kind .or. abs(rlat) > 90._r_kind .or. &
                  abs(rlon) > 360._r_kind) then
                  good = .false.
                  if (verbose) write(6, *) 'READ_GNSSRO: obs fails georeality check, said=', said, 'ptid=', ptid
               endif
               ! bending angle and impact parameter
               if (bend >= 1.e+9_r_kind .or. bend <= 0._r_kind .or. impact >= 1.e+9_r_kind .or. impact < roc .or. &
                  bendflag == 1 ) then
                  good = .false.
                  if (verbose) write(6, *) 'READ_GNSSRO: obs bend/impact is invalid, said=', said, 'ptid=', ptid
               endif
               ! reffractivity
               if (ref >= 1.e+9_r_kind .or. ref <= 0._r_kind .or. refflag == 1 ) then
               ref = r_missing
               endif
               ! azimuth
               if (abs(azim) > 360._r_kind .or. azim < 0._r_kind) then
                  azim = r_missing
               endif
               ! append data if values are in valid range
               if (good) then
                  ndata = ndata + 1 
                  gnssro_data%recn(ndata) = nrec
                  gnssro_data%lat(ndata) = rlat
                  gnssro_data%lon(ndata) = rlon
                  gnssro_data%time(ndata) = timeo
                  gnssro_data%epochtime(ndata) = epochtime
                  gnssro_data%gstime(ndata) = gstime
                  gnssro_data%datetime(ndata) = datetime
                  gnssro_data%said(ndata) = said
                  gnssro_data%siid(ndata) = siid
                  gnssro_data%sclf(ndata) = sclf
                  gnssro_data%asce(ndata) = asce
                  gnssro_data%ptid(ndata) = ptid
                  gnssro_data%ogce(ndata) = ogce
                  gnssro_data%ref(ndata) = ref
                  gnssro_data%msl_alt(ndata) = height
                  gnssro_data%bend_ang(ndata) = bend
                  gnssro_data%bndoe_gsi(ndata) = bend_error
                  gnssro_data%impact_para(ndata) = impact
                  gnssro_data%rfict(ndata) = roc
                  gnssro_data%geoid(ndata) = geoid
                  gnssro_data%azim(ndata) = azim
                  call bendingangle_err_gsi(rlat, impact - roc, obsErr, ogce, said)
                  gnssro_data%bndoe_gsi(ndata) = obsErr
                  if (ref > r_missing)  then
                     call refractivity_err_gsi(rlat, height, GlobalModel, obsErr)
                     gnssro_data%refoe_gsi(ndata) = obsErr
                  else
                     gnssro_data%refoe_gsi(ndata) = r_missing
                  end if
               end if
            end do ! end of k loop
            if (ndata == ndata0) nrec = nrec - 1
         enddo read_loop
      end do
      call closbf(lnbufr)
      gnssro_bufr_info%nobs = ndata
      if (nrec == 0) then
         write(6, *) "Error. No valid observations found. Cannot create NetCDF ouput."
         stop 2
      endif
   end subroutine


   subroutine assign_gnssro_data_to_time_window(gnssro_data, gnssro_bufr_info, file_output_info)
      ! Assigns each observation to a time window index in [1, gnssro_bufr_info%n_windows].
      ! The assignment to a time window is based on the observation time (gnssro_data%gstime).
      use utils_mod, only: da_advance_time, da_get_time_slots
      use define_mod, only: dtime_min, dtime_max
      type(gnssro_type), intent(inout) :: gnssro_data
      type(bufr_info_type), intent(in) :: gnssro_bufr_info
      type(output_info_type), intent(in) :: file_output_info
      integer(i_kind) :: n_windows
      integer(i_kind) :: ndata
      character(:), allocatable :: analysis_time
      character(len = 14) :: tmin_string, tmax_string
      real(r_kind), dimension(0 : file_output_info%n_windows) :: time_slots
      real(r_kind) :: obs_time
      integer :: idx_obs, j
      ndata = gnssro_bufr_info%nobs
      analysis_time = gnssro_bufr_info%analysis_time  ! analysis time based on 6h bufr file
      n_windows = file_output_info%n_windows
      ! initialize window index with value outside of valid time range
      gnssro_data%idx_window = -1
      if (n_windows > 1) then  ! in case the output is split into time windows
         call da_advance_time(analysis_time, dtime_min, tmin_string)  ! initial time of bufr file
         call da_advance_time(analysis_time, dtime_max, tmax_string)  ! final time of bufr file
         call da_get_time_slots(n_windows, tmin_string, tmax_string, time_slots)  ! time windows contained in bufr file
         do idx_obs = 1, ndata
            ! identify the time window that contains obs_time
            obs_time = gnssro_data%gstime(idx_obs)  ! julian time format of gstime is identical to time_slots
            do j = 1, n_windows
               ! strictly speaking, the logical expression should contain one <= and one < condition (otherwise the assignment
               ! at window boundaries is ambiguous). The first and last time windows require a <= at the window start and end,
               ! respectively. The use of two <= statements accommodates this and implies that observations at intermediate
               ! domain boundaries are assigned to the preceeding window.
               if (time_slots(j-1) <= obs_time .and. obs_time <= time_slots(j)) then
                  gnssro_data%idx_window(idx_obs) = j
                  exit
               endif
            enddo
         enddo
      else  ! in case the output is not split into time windows
         gnssro_data%idx_window(1 : ndata) = 1  ! all valid obs have the same index
      endif
   end subroutine


   subroutine get_output_file_name(gnssro_bufr_info, file_output_info, idx_window, output_file_name)
      use define_mod, only: half_bufr_interval
      use utils_mod, only: da_advance_time
      type(bufr_info_type), intent(in) :: gnssro_bufr_info
      type(output_info_type), intent(in) :: file_output_info
      integer(i_kind), intent(in) :: idx_window
      character(:), allocatable, intent(out) :: output_file_name
      character(len = 14) :: delta_time, output_file_date_long  ! delta_time has to be at least 3 characters long
      character(:), allocatable :: output_file_date, analysis_time
      analysis_time = gnssro_bufr_info%analysis_time  ! analysis time based on 6h bufr files
      if (file_output_info%n_windows > 1) then  ! multiple time windows
         ! obtain the central time for this time window by adding the appropriate multiple of the window length to the
         ! initial time of the 6h bufr interval
         write(delta_time, '(i2, a)') (file_output_info%window_length_in_h * (idx_window - 1)) - half_bufr_interval, 'h'
         call da_advance_time(analysis_time, trim(adjustl(delta_time)), output_file_date_long)
         output_file_date = output_file_date_long(1:10)
      else  ! single time window: central time corresponds to 6h bufr file analysis time
         output_file_date = analysis_time
      endif
      output_file_name = trim(adjustl(file_output_info%output_dir)) // 'gnssro_obs_' // output_file_date // '.h5'
   end subroutine


   subroutine write_gnssro_data(gnssro_data, gnssro_bufr_info, file_output_info, idx_window)
      type(gnssro_type), intent(in) :: gnssro_data
      type(bufr_info_type), intent(in) :: gnssro_bufr_info
      type(output_info_type), intent(in) :: file_output_info
      integer(i_kind), intent(in) :: idx_window
      logical, dimension(gnssro_bufr_info%nobs_max) :: is_in_window
      integer(i_kind) :: ndata
      integer :: idx_min_time, idx_max_time
      integer :: ncid, nlocs_dimid, grpid_metadata, grpid_obsvalue, grpid_obserror
      integer :: varid_lat, varid_lon, varid_time, varid_epochtime, varid_recn, varid_sclf, varid_ptid, varid_said
      integer :: varid_siid, varid_asce, varid_ogce, varid_msl, varid_impp, varid_imph, varid_azim, varid_geoid, varid_rfict
      integer :: varid_ref, varid_refoe, varid_bnd, varid_bndoe
      character(:), allocatable :: output_file_name

      ! identify observations in current time window
      is_in_window = (gnssro_data%idx_window == idx_window)
      ndata = count(is_in_window, kind = i_kind)

      ! create netcdf file and enter define mode
      call get_output_file_name(gnssro_bufr_info, file_output_info, idx_window, output_file_name)
      call check(nf90_create(trim(adjustl(output_file_name)), NF90_NETCDF4, ncid))

      ! create dimension and descriptive global attribute
      call check(nf90_def_dim(ncid, 'nlocs', ndata, nlocs_dimid))
      call check(nf90_put_att(ncid, NF90_GLOBAL, 'nlocs', ndata))  ! analogous to netcdf_mod

      ! write other global attributes (again analogous to netcdf_mod)
      idx_min_time = minloc(gnssro_data%epochtime, mask = is_in_window, dim = 1)
      idx_max_time = maxloc(gnssro_data%epochtime, mask = is_in_window, dim = 1)
      call check(nf90_put_att(ncid, NF90_GLOBAL, 'ioda_version', 'fortran generated ioda2 file'))
      call check(nf90_put_att(ncid, NF90_GLOBAL, 'min_datetime', gnssro_data%datetime(idx_min_time)))
      call check(nf90_put_att(ncid, NF90_GLOBAL, 'max_datetime', gnssro_data%datetime(idx_max_time)))

      ! create groups
      call check(nf90_def_grp(ncid, 'MetaData', grpid_metadata))
      call check(nf90_def_grp(ncid, 'ObsValue', grpid_obsvalue))
      call check(nf90_def_grp(ncid, 'ObsError', grpid_obserror))

      ! create variables, write their attributes and fill value
      ! MetaData/latitude
      call check(nf90_def_var(grpid_metadata, "latitude", NF90_FLOAT, nlocs_dimid, varid_lat))
      call check(nf90_put_att(grpid_metadata, varid_lat, "units", "degree_north"))
      call check(nf90_def_var_fill(grpid_metadata, varid_lat, 0, real(r_missing)))
      ! MetaData/longitude
      call check(nf90_def_var(grpid_metadata, "longitude", NF90_FLOAT, nlocs_dimid, varid_lon))
      call check(nf90_put_att(grpid_metadata, varid_lon, "units", "degree_east"))
      call check(nf90_def_var_fill(grpid_metadata, varid_lon, 0, real(r_missing)))
      ! MetaData/time
      call check(nf90_def_var(grpid_metadata, "time", NF90_FLOAT, nlocs_dimid, varid_time))
      call check(nf90_put_att(grpid_metadata, varid_time, "longname", "time offset to analysis time"))
      call check(nf90_put_att(grpid_metadata, varid_time, "units", "hour"))
      call check(nf90_def_var_fill(grpid_metadata, varid_time, 0, real(r_missing)))
      ! MetaData/dateTime
      call check(nf90_def_var(grpid_metadata, "dateTime", NF90_INT64, nlocs_dimid, varid_epochtime))
      call check(nf90_put_att(grpid_metadata, varid_epochtime, "units", "seconds since 1970-01-01T00:00:00Z"))
      call check(nf90_def_var_fill(grpid_metadata, varid_epochtime, 0, i64_missing))
      ! MetaData/record_number
      call check(nf90_def_var(grpid_metadata, "record_number", NF90_INT, nlocs_dimid, varid_recn))
      call check(nf90_put_att(grpid_metadata, varid_recn, "longname", "GNSS RO profile identifier"))
      call check(nf90_put_att(grpid_metadata, varid_recn, "units",  "1"))
      call check(nf90_def_var_fill(grpid_metadata, varid_recn, 0, i_missing))
      ! MetaData/gnss_sat_class
      call check(nf90_def_var(grpid_metadata, "gnss_sat_class", NF90_INT, nlocs_dimid, varid_sclf))
      call check(nf90_put_att(grpid_metadata, varid_sclf, "longname", &
         "GNSS satellite classification, e.g., 401=GPS, 402=GLONASS"))
      call check(nf90_put_att(grpid_metadata, varid_sclf, "units", "1"))
      call check(nf90_def_var_fill(grpid_metadata, varid_sclf, 0, i_missing))
      ! MetaData/reference_sat_id
      call check(nf90_def_var(grpid_metadata, "reference_sat_id", NF90_INT, nlocs_dimid, varid_ptid))
      call check(nf90_put_att(grpid_metadata, varid_ptid, "longname", "GNSS satellite transmitter identifier (1-32)"))
      call check(nf90_put_att(grpid_metadata, varid_ptid, "units", "1"))
      call check(nf90_def_var_fill(grpid_metadata, varid_ptid, 0, i_missing))
      ! MetaData/occulting_sat_id
      call check(nf90_def_var(grpid_metadata, "occulting_sat_id", NF90_INT, nlocs_dimid, varid_said))
      call check(nf90_put_att(grpid_metadata, varid_said, "longname",&
         "Low Earth Orbit satellite identifier, e.g., COSMIC2=750-755"))
      call check(nf90_put_att(grpid_metadata, varid_said, "units", "1"))
      call check(nf90_def_var_fill(grpid_metadata, varid_said, 0, i_missing))
      ! MetaData/occulting_sat_is
      call check(nf90_def_var(grpid_metadata, "occulting_sat_is", NF90_INT, nlocs_dimid, varid_siid))
      call check(nf90_put_att(grpid_metadata, varid_siid, "longname", "satellite instrument"))
      call check(nf90_put_att(grpid_metadata, varid_siid, "units", "1"))
      call check(nf90_def_var_fill(grpid_metadata, varid_siid, 0, i_missing))
      ! MetaData/ascending_flag
      call check(nf90_def_var(grpid_metadata, "ascending_flag", NF90_INT, nlocs_dimid, varid_asce))
      call check(nf90_put_att(grpid_metadata, varid_asce, "longname", "the original occultation ascending/descending flag"))
      call check(nf90_put_att(grpid_metadata, varid_asce, "valid_range", int((/ 0, 1 /))))
      call check(nf90_put_att(grpid_metadata, varid_asce, "flag_values", int((/ 0, 1 /))))
      call check(nf90_put_att(grpid_metadata, varid_asce, "flag_meanings", "descending ascending"))
      call check(nf90_put_att(grpid_metadata, varid_asce, "units", "1"))
      call check(nf90_def_var_fill(grpid_metadata, varid_asce, 0, i_missing))
      ! MetaData/process_center
      call check(nf90_def_var(grpid_metadata, "process_center", NF90_INT, nlocs_dimid, varid_ogce))
      call check( nf90_put_att(grpid_metadata, varid_ogce, "longname", &
         "originally data processing_center, e.g., 60 for UCAR, 94 for DMI, 78 for GFZ"))
      call check(nf90_put_att(grpid_metadata, varid_ogce, "units", "1"))
      call check(nf90_def_var_fill(grpid_metadata, varid_ogce, 0, i_missing))
      ! ObsValue/refractivity
      call check(nf90_def_var(grpid_obsvalue, "refractivity", NF90_FLOAT, nlocs_dimid, varid_ref))
      call check(nf90_put_att(grpid_obsvalue, varid_ref, "longname", "Atmospheric refractivity"))
      call check(nf90_put_att(grpid_obsvalue, varid_ref, "units", "N"))
      call check(nf90_put_att(grpid_obsvalue, varid_ref, "valid_range", real((/ 0.0, 500.0 /))))
      call check(nf90_def_var_fill(grpid_obsvalue, varid_ref, 0, real(r_missing)))
      ! ObsError/refractivity
      call check(nf90_def_var(grpid_obserror, "refractivity", NF90_FLOAT, nlocs_dimid, varid_refoe))
      call check(nf90_put_att(grpid_obserror, varid_refoe, "longname", "Input error in atmospheric refractivity"))
      call check(nf90_put_att(grpid_obserror, varid_refoe, "units", "N"))
      call check(nf90_put_att(grpid_obserror, varid_refoe, "valid_range", real((/ 0.0, 10.0 /))))
      call check(nf90_def_var_fill(grpid_obserror, varid_refoe, 0, real(r_missing)))
      ! MetaData/altitude
      call check(nf90_def_var(grpid_metadata, "altitude", NF90_FLOAT, nlocs_dimid, varid_msl))
      call check(nf90_put_att(grpid_metadata, varid_msl, "longname", "Geometric altitude"))
      call check(nf90_put_att(grpid_metadata, varid_msl, "units", "m"))
      call check(nf90_def_var_fill(grpid_metadata, varid_msl, 0, real(r_missing)))
      ! ObsValue/bending_angle
      call check(nf90_def_var(grpid_obsvalue, "bending_angle", NF90_FLOAT, nlocs_dimid, varid_bnd))
      call check(nf90_put_att(grpid_obsvalue, varid_bnd, "longname", "Bending Angle"))
      call check(nf90_put_att(grpid_obsvalue, varid_bnd, "units", "radian"))
      call check(nf90_put_att(grpid_obsvalue, varid_bnd, "valid_range", real((/ -0.001, 0.08 /))))
      call check(nf90_def_var_fill(grpid_obsvalue, varid_bnd, 0, real(r_missing)))
      ! ObsError/bending_angle
      call check(nf90_def_var(grpid_obserror, "bending_angle", NF90_FLOAT, nlocs_dimid, varid_bndoe))
      call check(nf90_put_att(grpid_obserror, varid_bndoe, "longname", "Input error in Bending Angle"))
      call check(nf90_put_att(grpid_obserror, varid_bndoe, "units", "radian"))
      call check(nf90_put_att(grpid_obserror, varid_bndoe, "valid_range", real((/ 0.0, 0.008 /))))
      call check(nf90_def_var_fill(grpid_obserror, varid_bndoe, 0, real(r_missing)))
      ! MetaData/impact_parameter
      call check(nf90_def_var(grpid_metadata, "impact_parameter", NF90_FLOAT, nlocs_dimid, varid_impp))
      call check(nf90_put_att(grpid_metadata, varid_impp, "longname", "distance from centre of curvature"))
      call check(nf90_put_att(grpid_metadata, varid_impp, "units", "m"))
      call check(nf90_put_att(grpid_metadata, varid_impp, "valid_range", real((/ 6200000.0, 6600000.0 /))))
      call check(nf90_def_var_fill(grpid_metadata, varid_impp, 0, real(r_missing)))
      ! MetaData/impact_height
      call check(nf90_def_var(grpid_metadata, "impact_height", NF90_FLOAT, nlocs_dimid, varid_imph))
      call check(nf90_put_att(grpid_metadata, varid_imph, "longname", "distance from mean sea level"))
      call check(nf90_put_att(grpid_metadata, varid_imph, "units", "m"))
      call check(nf90_put_att(grpid_metadata, varid_imph, "valid_range", real((/ 0.0, 200000.0 /))))
      call check(nf90_def_var_fill(grpid_metadata, varid_imph, 0, real(r_missing)))
      ! MetaData/sensor_azimuth_angle
      call check(nf90_def_var(grpid_metadata, "sensor_azimuth_angle", NF90_FLOAT, nlocs_dimid, varid_azim))
      call check(nf90_put_att(grpid_metadata, varid_azim, "longname", "GNSS->LEO line of sight"))
      call check(nf90_put_att(grpid_metadata, varid_azim, "units", "degree"))
      call check(nf90_put_att(grpid_metadata, varid_azim, "valid_range", real((/ 0.0, 360.0 /))))
      call check(nf90_def_var_fill(grpid_metadata, varid_azim, 0, real(r_missing)))
      ! MetaData/geoid_height_above_reference_ellipsoid
      call check(nf90_def_var(grpid_metadata, "geoid_height_above_reference_ellipsoid", NF90_FLOAT, nlocs_dimid, varid_geoid))
      call check(nf90_put_att(grpid_metadata, varid_geoid, "longname", "Geoid height above WGS-84 ellipsoid"))
      call check(nf90_put_att(grpid_metadata, varid_geoid, "units", "m"))
      call check(nf90_put_att(grpid_metadata, varid_geoid, "valid_range", real((/ -200.0, 200.0 /))))
      call check(nf90_def_var_fill(grpid_metadata, varid_geoid, 0, real(r_missing)))
      ! MetaData/earth_radius_of_curvature
      call check(nf90_def_var(grpid_metadata, "earth_radius_of_curvature", NF90_FLOAT, nlocs_dimid, varid_rfict))
      call check(nf90_put_att(grpid_metadata, varid_rfict, "longname", "Earth’s local radius of curvature"))
      call check(nf90_put_att(grpid_metadata, varid_rfict, "units", "m"))
      call check(nf90_put_att(grpid_metadata, varid_rfict, "valid_range", real((/ 6200000.0, 6600000.0 /))))
      call check(nf90_def_var_fill(grpid_metadata, varid_rfict, 0, real(r_missing)))

      ! write variables in current time window
      call check(nf90_enddef(ncid))  ! enters write mode
      call check(nf90_put_var(grpid_obsvalue, varid_ref, pack(gnssro_data%ref, is_in_window)))
      call check(nf90_put_var(grpid_obserror, varid_refoe, pack(gnssro_data%refoe_gsi, is_in_window)))
      call check(nf90_put_var(grpid_obsvalue, varid_bnd, pack(gnssro_data%bend_ang, is_in_window)))
      call check(nf90_put_var(grpid_obserror, varid_bndoe, pack(gnssro_data%bndoe_gsi, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_lat, pack(gnssro_data%lat, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_lon, pack(gnssro_data%lon, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_time, pack(gnssro_data%time, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_epochtime, pack(gnssro_data%epochtime, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_recn, pack(gnssro_data%recn, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_said, pack(gnssro_data%said, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_siid, pack(gnssro_data%siid, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_ptid, pack(gnssro_data%ptid, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_sclf, pack(gnssro_data%sclf, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_asce, pack(gnssro_data%asce, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_ogce, pack(gnssro_data%ogce, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_msl, pack(gnssro_data%msl_alt, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_impp, pack(gnssro_data%impact_para, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_imph, &
         pack(gnssro_data%impact_para - gnssro_data%rfict - gnssro_data%geoid, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_azim, pack(gnssro_data%azim, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_geoid, pack(gnssro_data%geoid, is_in_window)))
      call check(nf90_put_var(grpid_metadata, varid_rfict, pack(gnssro_data%rfict, is_in_window)))

      ! close file
      call check(nf90_close(ncid))
   end subroutine


   subroutine deallocate_gnssro_data_array(gnssro_data)
      type(gnssro_type), intent(inout) :: gnssro_data
      deallocate(gnssro_data%said)
      deallocate(gnssro_data%siid)
      deallocate(gnssro_data%sclf)
      deallocate(gnssro_data%ptid)
      deallocate(gnssro_data%recn)
      deallocate(gnssro_data%asce)
      deallocate(gnssro_data%ogce)
      deallocate(gnssro_data%time)
      deallocate(gnssro_data%epochtime)
      deallocate(gnssro_data%datetime)
      deallocate(gnssro_data%lat)
      deallocate(gnssro_data%lon)
      deallocate(gnssro_data%rfict)
      deallocate(gnssro_data%azim)
      deallocate(gnssro_data%geoid)
      deallocate(gnssro_data%msl_alt)
      deallocate(gnssro_data%ref)
      deallocate(gnssro_data%refoe_gsi)
      deallocate(gnssro_data%bend_ang)
      deallocate(gnssro_data%impact_para)
      deallocate(gnssro_data%bndoe_gsi)
      deallocate(gnssro_data%gstime)
      deallocate(gnssro_data%idx_window)
   end subroutine


   subroutine check(status)
      integer, intent ( in) :: status
      if(status /= nf90_noerr) then 
         print *, trim(nf90_strerror(status))
         stop "Stopped"
      end if
  end subroutine check  


   subroutine  refractivity_err_gsi(obsLat, obsZ, GlobalModel, obsErr)
      real(r_kind),  intent(in)  :: obsLat,  obsZ
      real(r_kind),  intent(out) :: obsErr
      logical,       intent(in)  :: GlobalModel
      real(r_kind)               :: obsZ_km

      obsZ_km  = obsZ / 1000.0
      if( GlobalModel ) then ! for global
         if( obsLat>= 20.0 .or.obsLat<= -20.0 ) then
            obsErr=-1.321_r_kind+0.341_r_kind*obsZ_km-0.005_r_kind*obsZ_km**2
         else
            if(obsZ_km > 10.0) then
               obsErr=2.013_r_kind-0.060_r_kind*obsZ_km+0.0045_r_kind*obsZ_km**2
            else
               obsErr=-1.18_r_kind+0.058_r_kind*obsZ_km+0.025_r_kind*obsZ_km**2
            endif
         endif
         obsErr = 1.0_r_kind/abs(exp(obsErr))
      else ! for regional 
         if( obsLat >= 20.0 .or.obsLat <= -20.0 ) then
            if (obsZ_km > 10.00) then
               obsErr =-1.321_r_kind+0.341_r_kind*obsZ_km-0.005_r_kind*obsZ_km**2
            else
               obsErr =-1.2_r_kind+0.065_r_kind*obsZ_km+0.021_r_kind*obsZ_km**2
            endif
         else
            if(obsZ_km > 10.00) then
               obsErr =2.013_r_kind-0.120_r_kind*obsZ_km+0.0065_r_kind*obsZ_km**2
            else
               obsErr =-1.19_r_kind+0.03_r_kind*obsZ_km+0.023_r_kind*obsZ_km**2
            endif
         endif
         obsErr = 1.0_r_kind/abs(exp(obsErr))
      endif
   end subroutine refractivity_err_gsi


   subroutine  bendingangle_err_gsi(obsLat, obsZ,  obsErr, ogce, said)
      real(r_kind), intent(in)   :: obsLat,  obsZ
      integer(i_kind), intent(in) :: ogce, said
      real(r_kind), intent(out)  :: obsErr
      real(r_kind)               :: obsZ_km

      obsZ_km  = obsZ / 1000.0
      if((said==41).or.(said==722).or.(said==723).or.(said==42).or.&
         (said>=3.and.said<=5).or.(said==821.or.(said==421)).or.(said==440).or.(said==43) .or. (ogce/=60) ) then
         if( abs(obsLat)>= 40.00 ) then
            if(obsZ_km>12.) then
               obsErr=0.19032_r_kind+0.287535_r_kind*obsZ_km-0.00260813_r_kind*obsZ_km**2
            else
               obsErr=-3.20978_r_kind+1.26964_r_kind*obsZ_km-0.0622538_r_kind*obsZ_km**2
            endif
         else
            if(obsZ_km>18.) then
               obsErr=-1.87788_r_kind+0.354718_r_kind*obsZ_km-0.00313189_r_kind*obsZ_km**2
            else
               obsErr=-2.41024_r_kind+0.806594_r_kind*obsZ_km-0.027257_r_kind*obsZ_km**2
            endif
         endif
         obsErr = 0.001_r_kind/abs(exp(obsErr))
      else !!!! CDAAC processing
         if( abs(obsLat)>= 40.00 ) then
            if (obsZ_km > 12.00) then
               obsErr=-0.685627_r_kind+0.377174_r_kind*obsZ_km-0.00421934_r_kind*obsZ_km**2
            else
               obsErr=-3.27737_r_kind+1.20003_r_kind*obsZ_km-0.0558024_r_kind*obsZ_km**2
            endif
         else
            if(obsZ_km >18.00) then
               obsErr=-2.73867_r_kind+0.447663_r_kind*obsZ_km-0.00475603_r_kind*obsZ_km**2
            else
               obsErr=-3.45303_r_kind+0.908216_r_kind*obsZ_km-0.0293331_r_kind*obsZ_km**2
            endif
         endif
         obsErr = 0.001_r_kind/abs(exp(obsErr))
      endif
   end subroutine bendingangle_err_gsi


   !!!!!! SUBROUTINE W3FS21 was copied from GSI/src/libs/w3nco_v2.0.6/w3fs21.f and iw3jdn.f
   SUBROUTINE W3FS21(IDATE, NMIN)
      INTEGER  IDATE(5)
      INTEGER  NMIN
      INTEGER  IYEAR, NDAYS, IJDN
      INTEGER  JDN78
      DATA  JDN78 / 2443510 /
      
      NMIN  = 0
      IYEAR = IDATE(1)
      IF (IYEAR.LE.99) THEN
         IF (IYEAR.LT.78) THEN
            IYEAR = IYEAR + 2000
         ELSE
            IYEAR = IYEAR + 1900
         ENDIF
      ENDIF

      ! COMPUTE JULIAN DAY NUMBER FROM YEAR, MONTH, DAY
      IJDN  = IDATE(3) - 32075      &
         + 1461 * (IYEAR + 4800 + (IDATE(2) - 14) / 12) / 4  &
         + 367 * (IDATE(2)- 2 - (IDATE(2) -14) / 12 * 12) / 12   &
         - 3 * ((IYEAR + 4900 + (IDATE(2) - 14) / 12) / 100) / 4

      ! SUBTRACT JULIAN DAY NUMBER OF JAN 1,1978 TO GET THE
      ! NUMBER OF DAYS BETWEEN DATES
      NDAYS = IJDN - JDN78
      NMIN = NDAYS * 1440 + IDATE(4) * 60 + IDATE(5)
      RETURN
   END SUBROUTINE W3FS21

end module gnssro_bufr2ioda
