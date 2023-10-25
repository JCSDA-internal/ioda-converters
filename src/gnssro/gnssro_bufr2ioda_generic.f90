!!!--------  README --------------------------------------------------------------------
!  this is a temporary routine to generate netcdf4 file
!  for jedi/ufo/gnssro/ operator test
!  Copyright UCAR 2022
!  Author: Hailing Zhang
!  Last upate: APR 10 2023

!!!---------  to run   -----------------------------------------------------------------
!  ./gnssro_bufr2ioda2 yyyymmddhh $bufrfile_input $netcdffile_output
!
! Return codes:
!  0 - Success.
!  1 - Unrecoverable system or logical error.
!  2 - All provided observations are invalid.  Cannot create NetCDF IODA file.

program gnssro_bufr2ioda2
   use, intrinsic :: iso_fortran_env
   use netcdf
   implicit none

! output obs data stucture
   integer   :: ncid
   integer   :: nobs_dimid, nlocs_dimid, nvars_dimid, nrecs_dimid
   integer   :: varid_lat, varid_lon, varid_epochtime
   integer   :: varid_said, varid_siid, varid_ptid, varid_sclf, varid_asce, varid_ogce
   integer   :: varid_recn
   integer   :: varid_geoid, varid_rfict
   integer   :: varid_ref, varid_msl
   integer   :: varid_bnd, varid_impp, varid_imph, varid_azim
   integer   :: nlev_dimid
   integer   :: varid_geo_temp, varid_geo_pres, varid_geo_shum, varid_geo_geop, varid_geo_geop_sfc
   integer   :: grpid_metadata, grpid_obsvalue
   integer   :: deflate_level
   character(len=256)       :: infile, outfile
   character, dimension(8)  :: subset
   character(len=10)        :: anatime
   integer(int32)           :: i, k, m, ireadmg, ireadsb, said, siid, ptid, sclf, asce, ogce
   integer(int32)           :: lnbufr = 10
   integer(int32)           :: nread, ndata, nvars, nrec, ndata0
   integer(int32)           :: idate5(6), idate
   integer(int64)           :: epochtime

   logical                   :: good, outside
   integer                   :: refflag, bendflag
   integer(int32), parameter :: mxib = 31
   integer(int32)            :: ibit(mxib), nib
   integer(int32), parameter :: maxlevs = 500
   integer(int32), parameter :: n1ahdr = 13
   integer(int32)            :: maxobs
   type gnssro_type
      integer(int32), allocatable, dimension(:)    :: said
      integer(int32), allocatable, dimension(:)    :: siid
      integer(int32), allocatable, dimension(:)    :: sclf
      integer(int32), allocatable, dimension(:)    :: ptid
      integer(int32), allocatable, dimension(:)    :: recn
      integer(int32), allocatable, dimension(:)    :: asce
      integer(int32), allocatable, dimension(:)    :: ogce
      integer(int64), allocatable, dimension(:)    :: epochtime
      real(real64), allocatable, dimension(:)     :: lat
      real(real64), allocatable, dimension(:)     :: lon
      real(real64), allocatable, dimension(:)     :: rfict
      real(real64), allocatable, dimension(:)     :: azim
      real(real64), allocatable, dimension(:)     :: geoid
      real(real64), allocatable, dimension(:)     :: msl_alt
      real(real64), allocatable, dimension(:)     :: ref
      real(real64), allocatable, dimension(:)     :: bend_ang
      real(real64), allocatable, dimension(:)     :: impact_para
   end type gnssro_type

   type(gnssro_type) :: gnssro_data

   real(real64), dimension(n1ahdr)      :: bfr1ahdr
   real(real64), dimension(50, maxlevs) :: data1b
   real(real64), dimension(50, maxlevs) :: data2a
   real(real64), dimension(maxlevs)     :: nreps_this_ROSEQ2
   integer(int32)                       :: iret, levs, levsr, nreps_ROSEQ1, nreps_ROSEQ2_int
   real(real64) :: pcc, qfro(1), usage, dlat, dlat_earth, dlon, dlon_earth, freq_chk, azim
   real(real64) :: height, rlat, rlon, ref, bend, impact, roc, geoid, bend_pccf, ref_pccf
   real(real64)    :: r_missing
   integer(int32)  :: i_missing
   integer(int64)  :: i64_missing

   logical, parameter :: GlobalModel = .true. ! temporary

   character(10) nemo
   character(80) hdr1a

   data hdr1a/'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID SIID PTID GEODU SCLF OGCE'/
   data nemo/'QFRO'/

   i_missing = huge(i_missing)
   i64_missing = huge(i64_missing)
   r_missing = huge(i_missing)

   nrec = 0
   ndata = 0
   nvars = 2
   maxobs = 0
   deflate_level = 6

   call getarg(1, anatime)
   call getarg(2, infile)
   call getarg(3, outfile)

   open (lnbufr, file=trim(infile), form='unformatted')
   call openbf(lnbufr, 'IN', lnbufr)
   call datelen(10)
   call readmg(lnbufr, subset, idate, iret)
   if (iret /= 0) then
      write (6, *) 'READ_GNSSRO: can not open gnssro file!'
      stop
   end if

! loop over all message to estimate maxobs
   do while (ireadmg(lnbufr, subset, idate) == 0)
      read_loop_countmaxobs: do while (ireadsb(lnbufr) == 0)
         call ufbint(lnbufr, bfr1ahdr, n1ahdr, 1, iret, hdr1a)
         call ufbseq(lnbufr, data1b, 50, maxlevs, levs, 'ROSEQ1')
         maxobs = maxobs + levs
      end do read_loop_countmaxobs
   end do

   allocate (gnssro_data%said(maxobs))
   allocate (gnssro_data%siid(maxobs))
   allocate (gnssro_data%sclf(maxobs))
   allocate (gnssro_data%ptid(maxobs))
   allocate (gnssro_data%recn(maxobs))
   allocate (gnssro_data%asce(maxobs))
   allocate (gnssro_data%ogce(maxobs))
   allocate (gnssro_data%epochtime(maxobs))
   allocate (gnssro_data%lat(maxobs))
   allocate (gnssro_data%lon(maxobs))
   allocate (gnssro_data%rfict(maxobs))
   allocate (gnssro_data%azim(maxobs))
   allocate (gnssro_data%geoid(maxobs))
   allocate (gnssro_data%msl_alt(maxobs))
   allocate (gnssro_data%ref(maxobs))
   allocate (gnssro_data%bend_ang(maxobs))
   allocate (gnssro_data%impact_para(maxobs))

!rewind lnbufr
   call closbf(lnbufr)
   open (lnbufr, file=trim(infile), form='unformatted')
   call openbf(lnbufr, 'IN', lnbufr)
   call datelen(10)
   call readmg(lnbufr, subset, idate, iret)

   do while (ireadmg(lnbufr, subset, idate) == 0)
      read_loop: do while (ireadsb(lnbufr) == 0)
!    Read/decode data in subset (profile)
         call ufbint(lnbufr, bfr1ahdr, n1ahdr, 1, iret, hdr1a)
         call ufbint(lnbufr, qfro, 1, 1, iret, nemo)
!    observation time in minutes
         idate5(1) = bfr1ahdr(1) ! year
         idate5(2) = bfr1ahdr(2) ! month
         idate5(3) = bfr1ahdr(3) ! day
         idate5(4) = bfr1ahdr(4) ! hour
         idate5(5) = bfr1ahdr(5) ! minute
         idate5(6) = 0 ! seconds
         pcc = bfr1ahdr(6)        ! profile per cent confidence
         roc = bfr1ahdr(7)        ! Earth local radius of curvature
         said = bfr1ahdr(8)        ! Satellite identifier
         siid = bfr1ahdr(9)        ! Satellite instrument
         ptid = bfr1ahdr(10)       ! Platform transmitter ID number
         geoid = bfr1ahdr(11)       ! Geoid undulation
         sclf = bfr1ahdr(12)       ! Satellite classification
         ogce = bfr1ahdr(13)       ! Identification of originating/generating centre

         call epochtimecalculator(idate5, epochtime)  ! calculate epochtime since January 1 1970

         if (roc > 6450000.0_real64 .or. roc < 6250000.0_real64 .or.       &
            &   geoid > 200_real64 .or. geoid < -200._real64) then
            write (6, *) 'READ_GNSSRO: profile fails georeality check, skip this report'
            cycle read_loop
         end if

         if (pcc == 0.0) then
            write (6, *) 'READ_GNSSRO: bad profile 0.0% confidence said=', said, 'ptid=', ptid, ' SKIP this report'
            cycle read_loop
         end if

         bendflag = 0
         refflag = 0
         call upftbv(lnbufr, nemo, qfro, mxib, ibit, nib)
         if (nib > 0) then
            do i = 1, nib
               if (ibit(i) == 5 .or. ibit(i) == 1 .or. ibit(i) == 4) then  ! bending angle
                  bendflag = 1
                  write (6, *) 'READ_GNSSRO: bad profile said=', said, 'ptid=', ptid, ' SKIP this report'
                  cycle read_loop
               end if
               if (ibit(i) == 6 .or. ibit(i) == 1 .or. ibit(i) == 4) then  ! refractivity
                  refflag = 1
                  exit
               end if
            end do
         end if

         asce = 0
         call upftbv(lnbufr, nemo, qfro, mxib, ibit, nib)
         if (nib > 0) then
            do i = 1, nib
               if (ibit(i) == 3) then
                  asce = 1
                  exit
               end if
            end do
         end if

         call ufbint(lnbufr, nreps_this_ROSEQ2, 1, maxlevs, nreps_ROSEQ1, '{ROSEQ2}')
         call ufbseq(lnbufr, data1b, 50, maxlevs, levs, 'ROSEQ1')
         call ufbseq(lnbufr, data2a, 50, maxlevs, levsr, 'ROSEQ3') ! refractivity

         nrec = nrec + 1
         ndata0 = ndata

         do k = 1, levs
            rlat = data1b(1, k)  ! earth relative latitude (degrees)
            rlon = data1b(2, k)  ! earth relative longitude (degrees)
            azim = data1b(3, k)
            height = data2a(1, k)
            ref = data2a(2, k)
            ref_pccf = data2a(6, k)

!           Loop over number of replications of ROSEQ2 nested inside this particular replication of ROSEQ1
            nreps_ROSEQ2_int = nreps_this_ROSEQ2(k)
            do i = 1, nreps_ROSEQ2_int
               m = (6*i) - 2
               freq_chk = data1b(m, k)      ! frequency (hertz)
               if (nint(freq_chk) .ne. 0) cycle ! do not want non-zero freq., go on to next replication of ROSEQ2
               impact = data1b(m + 1, k)      ! impact parameter (m)
               bend = data1b(m + 2, k)        ! bending angle (rad)
            end do

            bend_pccf = data1b((6*nreps_ROSEQ2_int) + 4, k)  ! percent confidence for this ROSEQ1 replication
            good = .true.

            if (height < 0._real64 .or. height > 100000._real64 .or.           &
               & abs(rlat) > 90._real64 .or. abs(rlon) > 360._real64) then
               good = .false.
               write (6, *) 'READ_GNSSRO: obs fails georeality check, said=', said, 'ptid=', ptid
            end if
            if (bend >= 1.e+9_real64 .or. bend <= 0._real64 .or. impact >= 1.e+9_real64 .or. impact < roc .or. bendflag == 1) then
               good = .false.
               write (6, *) 'READ_GNSSRO: obs bend/impact is invalid, said=', said, 'ptid=', ptid
            end if

            if (ref >= 1.e+9_real64 .or. ref <= 0._real64 .or. refflag == 1) then
               ref = r_missing
            end if

            if (abs(azim) > 360._real64 .or. azim < 0._real64) then
               azim = r_missing
            end if

            if (good) then
               ndata = ndata + 1
               gnssro_data%recn(ndata) = nrec
               gnssro_data%lat(ndata) = rlat
               gnssro_data%lon(ndata) = rlon
               gnssro_data%epochtime(ndata) = epochtime
               gnssro_data%said(ndata) = said
               gnssro_data%siid(ndata) = siid
               gnssro_data%sclf(ndata) = sclf
               gnssro_data%asce(ndata) = asce
               gnssro_data%ptid(ndata) = ptid
               gnssro_data%ogce(ndata) = ogce
               gnssro_data%ref(ndata) = ref
               gnssro_data%msl_alt(ndata) = height
               gnssro_data%bend_ang(ndata) = bend
               gnssro_data%impact_para(ndata) = impact
               gnssro_data%rfict(ndata) = roc
               gnssro_data%geoid(ndata) = geoid
               gnssro_data%azim(ndata) = azim
            end if

         end do ! end of k loop

         if (ndata == ndata0) nrec = nrec - 1

      end do read_loop
   end do

   call closbf(lnbufr)
   if (nrec == 0) then
      write (6, *) 'Error. No valid observations found. Cannot create NetCDF ouput.'
      stop 2
   end if

   call check(nf90_create(trim(outfile), NF90_NETCDF4, ncid))
   call check(nf90_def_dim(ncid, 'Location', ndata, nlocs_dimid))
   call check(nf90_put_att(ncid, NF90_GLOBAL, 'date_time', anatime))
   call check(nf90_put_att(ncid, NF90_GLOBAL, 'ioda_version', 'Fortran generated ioda file'))
   call check(nf90_def_grp(ncid, 'MetaData', grpid_metadata))
   call check(nf90_def_grp(ncid, 'ObsValue', grpid_obsvalue))

   call check(nf90_def_var(grpid_metadata, "latitude", NF90_FLOAT, nlocs_dimid, varid_lat))
   call check(nf90_def_var_fill(grpid_metadata, varid_lat, 0, real(r_missing)))
   call check(nf90_def_var_deflate(grpid_metadata, varid_lat,       &
                                   & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_lat, "units", "degree_north"))

   call check(nf90_def_var(grpid_metadata, "longitude", NF90_FLOAT, nlocs_dimid, varid_lon))
   call check(nf90_def_var_fill(grpid_metadata, varid_lon, 0, real(r_missing)))
   call check(nf90_def_var_deflate(grpid_metadata, varid_lon,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_lon, "units", "degree_east"))

   call check(nf90_def_var(grpid_metadata, "dateTime", NF90_INT64, nlocs_dimid, varid_epochtime))
   call check(nf90_def_var_fill(grpid_metadata, varid_epochtime, 0, i64_missing))
   call check(nf90_def_var_deflate(grpid_metadata, varid_epochtime, &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_epochtime, "units", "seconds since 1970-01-01T00:00:00Z"))

   call check(nf90_def_var(grpid_metadata, "sequenceNumber", NF90_INT, nlocs_dimid, varid_recn))
   call check(nf90_def_var_fill(grpid_metadata, varid_recn, 0, i_missing))
   call check(nf90_def_var_deflate(grpid_metadata, varid_recn,      &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_recn, "longname", "GNSS RO profile identifier"))
   call check(nf90_put_att(grpid_metadata, varid_recn, "units", "1"))

   call check(nf90_def_var(grpid_metadata, "satelliteConstellationRO", NF90_INT, nlocs_dimid, varid_sclf))
   call check(nf90_def_var_fill(grpid_metadata, varid_sclf, 0, i_missing))

   call check(nf90_def_var_deflate(grpid_metadata, varid_sclf,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_sclf, "longname", &
      & "GNSS satellite classification, e.g., 401=GPS, 402=GLONASS"))
   call check(nf90_put_att(grpid_metadata, varid_sclf, "units", "1"))

   call check(nf90_def_var(grpid_metadata, "satelliteTransmitterId", NF90_INT, nlocs_dimid, varid_ptid))
   call check(nf90_def_var_fill(grpid_metadata, varid_ptid, 0, i_missing))
   call check(nf90_def_var_deflate(grpid_metadata, varid_ptid,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_ptid, "longname", "GNSS satellite transmitter identifier (1-32)"))
   call check(nf90_put_att(grpid_metadata, varid_ptid, "units", "1"))

   call check(nf90_def_var(grpid_metadata, "satelliteIdentifier", NF90_INT, nlocs_dimid, varid_said))
   call check(nf90_def_var_fill(grpid_metadata, varid_said, 0, i_missing))
   call check(nf90_def_var_deflate(grpid_metadata, varid_said,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_said, "longname", &
      & "Low Earth Orbit satellite identifier, e.g., COSMIC2=750-755"))
   call check(nf90_put_att(grpid_metadata, varid_said, "units", "1"))

   call check(nf90_def_var(grpid_metadata, "occulting_sat_is", NF90_INT, nlocs_dimid, varid_siid))
   call check(nf90_def_var_deflate(grpid_metadata, varid_siid,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_siid, "longname", "satellite instrument"))
   call check(nf90_put_att(grpid_metadata, varid_siid, "units", "1"))
   call check(nf90_def_var_fill(grpid_metadata, varid_siid, 0, i_missing))

   call check(nf90_def_var(grpid_metadata, "satelliteAscendingFlag", NF90_INT, nlocs_dimid, varid_asce))
   call check(nf90_def_var_deflate(grpid_metadata, varid_asce,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_asce, "longname", "the original occultation ascending/descending flag"))
   call check(nf90_put_att(grpid_metadata, varid_asce, "valid_range", int((/0, 1/))))
   call check(nf90_put_att(grpid_metadata, varid_asce, "flag_values", int((/0, 1/))))
   call check(nf90_put_att(grpid_metadata, varid_asce, "flag_meanings", "descending ascending"))
   call check(nf90_put_att(grpid_metadata, varid_asce, "units", "1"))
   call check(nf90_def_var_fill(grpid_metadata, varid_asce, 0, i_missing))

   call check(nf90_def_var(grpid_metadata, "process_center", NF90_INT, nlocs_dimid, varid_ogce))
   call check(nf90_def_var_deflate(grpid_metadata, varid_ogce,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_ogce, "longname", "originally data processing_center, &
                                                        &e.g., 60 for UCAR, 94 for DMI, 78 for GFZ"))
   call check(nf90_put_att(grpid_metadata, varid_ogce, "units", "1"))
   call check(nf90_def_var_fill(grpid_metadata, varid_ogce, 0, i_missing))

   call check(nf90_def_var(grpid_obsvalue, "atmosphericRefractivity", NF90_FLOAT, nlocs_dimid, varid_ref))
   call check(nf90_def_var_deflate(grpid_obsvalue, varid_ref,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_obsvalue, varid_ref, "longname", "Atmospheric refractivity"))
   call check(nf90_put_att(grpid_obsvalue, varid_ref, "units", "N"))
   call check(nf90_put_att(grpid_obsvalue, varid_ref, "valid_range", real((/0.0, 500.0/))))
   call check(nf90_def_var_fill(grpid_obsvalue, varid_ref, 0, real(r_missing)))

   call check(nf90_def_var(grpid_metadata, "height", NF90_FLOAT, nlocs_dimid, varid_msl))
   call check(nf90_def_var_deflate(grpid_metadata, varid_msl,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_msl, "longname", "Geometric altitude"))
   call check(nf90_put_att(grpid_metadata, varid_msl, "units", "m"))
   call check(nf90_def_var_fill(grpid_metadata, varid_msl, 0, real(r_missing)))

   call check(nf90_def_var(grpid_obsvalue, "bendingAngle", NF90_FLOAT, nlocs_dimid, varid_bnd))
   call check(nf90_def_var_deflate(grpid_obsvalue, varid_bnd,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_obsvalue, varid_bnd, "longname", "Bending Angle"))
   call check(nf90_put_att(grpid_obsvalue, varid_bnd, "units", "radian"))
   call check(nf90_put_att(grpid_obsvalue, varid_bnd, "valid_range", real((/-0.001, 0.08/))))
   call check(nf90_def_var_fill(grpid_obsvalue, varid_bnd, 0, real(r_missing)))

   call check(nf90_def_var(grpid_metadata, "impactParameterRO", NF90_FLOAT, nlocs_dimid, varid_impp))
   call check(nf90_def_var_deflate(grpid_metadata, varid_impp,      &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_impp, "longname", "distance from centre of curvature"))
   call check(nf90_put_att(grpid_metadata, varid_impp, "units", "m"))
   call check(nf90_put_att(grpid_metadata, varid_impp, "valid_range", real((/6200000.0, 6600000.0/))))
   call check(nf90_def_var_fill(grpid_metadata, varid_impp, 0, real(r_missing)))

   call check(nf90_def_var(grpid_metadata, "impactHeightRO", NF90_FLOAT, nlocs_dimid, varid_imph))
   call check(nf90_def_var_deflate(grpid_metadata, varid_imph,      &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_imph, "longname", "distance from mean sea level"))
   call check(nf90_put_att(grpid_metadata, varid_imph, "units", "m"))
   call check(nf90_put_att(grpid_metadata, varid_imph, "valid_range", real((/0.0, 200000.0/))))
   call check(nf90_def_var_fill(grpid_metadata, varid_imph, 0, real(r_missing)))

   call check(nf90_def_var(grpid_metadata, "sensorAzimuthAngle", NF90_FLOAT, nlocs_dimid, varid_azim))
   call check(nf90_def_var_deflate(grpid_metadata, varid_azim,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_azim, "longname", "GNSS->LEO line of sight"))
   call check(nf90_put_att(grpid_metadata, varid_azim, "units", "degree"))
   call check(nf90_put_att(grpid_metadata, varid_azim, "valid_range", real((/0.0, 360.0/))))
   call check(nf90_def_var_fill(grpid_metadata, varid_azim, 0, real(r_missing)))

   call check(nf90_def_var(grpid_metadata, "geoidUndulation", NF90_FLOAT, nlocs_dimid, varid_geoid))
   call check(nf90_def_var_deflate(grpid_metadata, varid_geoid,     &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_geoid, "longname", "Geoid height above WGS-84 ellipsoid"))
   call check(nf90_put_att(grpid_metadata, varid_geoid, "units", "m"))
   call check(nf90_put_att(grpid_metadata, varid_geoid, "valid_range", real((/-200.0, 200.0/))))
   call check(nf90_def_var_fill(grpid_metadata, varid_geoid, 0, real(r_missing)))

   call check(nf90_def_var(grpid_metadata, "earthRadiusCurvature", NF90_FLOAT, nlocs_dimid, varid_rfict))
   call check(nf90_def_var_deflate(grpid_metadata, varid_rfict,     &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_rfict, "longname", "Earth’s local radius of curvature"))
   call check(nf90_put_att(grpid_metadata, varid_rfict, "units", "m"))
   call check(nf90_put_att(grpid_metadata, varid_rfict, "valid_range", real((/6200000.0, 6600000.0/))))
   call check(nf90_def_var_fill(grpid_metadata, varid_rfict, 0, real(r_missing)))

   call check(nf90_enddef(ncid))

   call check(nf90_put_var(grpid_obsvalue, varid_ref, gnssro_data%ref(1:ndata)))
   call check(nf90_put_var(grpid_obsvalue, varid_bnd, gnssro_data%bend_ang(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_lat, gnssro_data%lat(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_lon, gnssro_data%lon(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_epochtime, gnssro_data%epochtime(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_recn, gnssro_data%recn(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_said, gnssro_data%said(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_siid, gnssro_data%siid(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_ptid, gnssro_data%ptid(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_sclf, gnssro_data%sclf(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_asce, gnssro_data%asce(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_ogce, gnssro_data%ogce(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_msl, gnssro_data%msl_alt(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_impp, gnssro_data%impact_para(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_imph, gnssro_data%impact_para(1:ndata)   &
                                      &                - gnssro_data%rfict(1:ndata)         &
                                      &                - gnssro_data%geoid(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_azim, gnssro_data%azim(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_geoid, gnssro_data%geoid(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_rfict, gnssro_data%rfict(1:ndata)))
   call check(nf90_close(ncid))

   deallocate (gnssro_data%said)
   deallocate (gnssro_data%siid)
   deallocate (gnssro_data%sclf)
   deallocate (gnssro_data%ptid)
   deallocate (gnssro_data%recn)
   deallocate (gnssro_data%asce)
   deallocate (gnssro_data%ogce)
   deallocate (gnssro_data%epochtime)
   deallocate (gnssro_data%lat)
   deallocate (gnssro_data%lon)
   deallocate (gnssro_data%rfict)
   deallocate (gnssro_data%azim)
   deallocate (gnssro_data%geoid)
   deallocate (gnssro_data%msl_alt)
   deallocate (gnssro_data%ref)
   deallocate (gnssro_data%bend_ang)
   deallocate (gnssro_data%impact_para)

contains
   subroutine check(status)
      integer, intent(in) :: status

      if (status /= nf90_noerr) then
         print *, trim(nf90_strerror(status))
         stop "Stopped"
      end if

   end subroutine check

!!!!!!________________________________________________________

!-------------------------------------------------------------
! written by H. ZHANG based w3nco_v2.0.6/w3fs21.f and iw3jdn.f
! calculating epoch time since January 1, 1970
   SUBROUTINE epochtimecalculator(IDATE, EPOCHTIME)
      INTEGER, DIMENSION(6) :: IDATE
      INTEGER ::  NMIN
      INTEGER ::   IYEAR, NDAYS, IJDN
      INTEGER(INT64) :: epochtime
      INTEGER, PARAMETER ::    JDN1970 = 2440588

      NMIN = 0
      IYEAR = IDATE(1)
      IF (IYEAR .LE. 99) THEN
         IF (IYEAR .LT. 78) THEN
            IYEAR = IYEAR + 2000
         ELSE
            IYEAR = IYEAR + 1900
         END IF
      END IF
!   COMPUTE JULIAN DAY NUMBER FROM YEAR, MONTH, DAY
      IJDN = IDATE(3) - 32075 &
             + 1461*(IYEAR + 4800 + (IDATE(2) - 14)/12)/4 &
             + 367*(IDATE(2) - 2 - (IDATE(2) - 14)/12*12)/12 &
             - 3*((IYEAR + 4900 + (IDATE(2) - 14)/12)/100)/4
!   SUBTRACT JULIAN DAY NUMBER OF JAN 1,1970 TO GET THE
!   NUMBER OF DAYS BETWEEN DATES
      NDAYS = IJDN - JDN1970
      NMIN = NDAYS*1440 + IDATE(4)*60 + IDATE(5)
      EPOCHTIME = NMIN*60 + IDATE(6)
   END SUBROUTINE epochtimecalculator

end program
