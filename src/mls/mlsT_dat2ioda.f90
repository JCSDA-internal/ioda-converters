
 program mlsT_dat2ioda
!------------------------------------------------------------------------------!
! Return codes:
!  0 - Success.
!  1 - Unrecoverable system or logical error.
!  2 - All provided observations are invalid.  Cannot create NetCDF IODA file.
!------------------------------------------------------------------------------!
! Modules

!  use def_grid
   use mls_module
   use netcdf
   use, intrinsic :: iso_fortran_env
!------------------------------------------------------------------------------!
   implicit none
!------------------------------------------------------------------------------!
! Input

! Which instrument: mlsT, saberT, mlsh2o, mlso3, Nmlst, Nmlso3 or Nmlsh2o?

   character(len=7) :: instvar = "mlsT"

   integer, parameter :: max_path_len=120
   integer, parameter :: max_dtg_len=10

   character(len=max_path_len) :: c_ob_ff
   character(len=max_dtg_len)  :: cdtg_an,cycle_hr,assim_win,version,endian
!  character(len=max_path_len)  :: cdtg_an,cycle_hr,assim_win,version

   real :: t_assim_win

   logical :: lmore = .false.

!------------------------------------------------------------------------------!
! Output obs data stucture

   integer   :: ncid
   integer   :: nobs_dimid, nlocs_dimid, nvars_dimid, nrecs_dimid
   integer   :: varid_lat, varid_lon, varid_epochtime
   integer   :: varid_said, varid_siid, varid_ptid, varid_sclf, varid_asce, varid_ogce
   integer   :: varid_qcfg, varid_tinc
   integer   :: varid_recn
   integer   :: varid_geoid, varid_rfict
   integer   :: varid_ref, varid_msl
   integer   :: varid_bnd, varid_impp, varid_imph, varid_azim
   integer   :: nlev_dimid
   integer   :: grpid_metadata, grpid_obsvalue
   integer   :: deflate_level
   character(len=256)       :: infile, outfile, runCheck
   logical                  :: addChecks
   character, dimension(8)  :: subset
   character(len=10)        :: anatime
   integer(int32)           :: i, k, m, ireadmg, ireadsb, said, siid, ptid, sclf, asce, ogce, qcflag, tinc
   integer(int32)           :: lnbufr = 10
   integer(int32)           :: nread, ndata, nvars, nrec, ndata0
   integer(int32)           :: idate5(6), idate
   integer(int64)           :: epochtime

   integer(int32), parameter :: mxib = 31
   integer(int32)            :: ibit(mxib), nib
   integer(int32), parameter :: maxlevs = 500
   integer(int32), parameter :: n1ahdr = 15
   integer(int32)            :: maxobs
   type gnssro_type
      integer(int32), allocatable, dimension(:)    :: said
      integer(int32), allocatable, dimension(:)    :: siid
      integer(int32), allocatable, dimension(:)    :: sclf
      integer(int32), allocatable, dimension(:)    :: ptid
      integer(int32), allocatable, dimension(:)    :: recn
      integer(int32), allocatable, dimension(:)    :: asce
      integer(int32), allocatable, dimension(:)    :: ogce
      integer(int32), allocatable, dimension(:)    :: qcflag
      integer(int32), allocatable, dimension(:)    :: tinc
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
   real(real64) :: qfro(1), usage, dlat, dlat_earth, dlon, dlon_earth, freq_chk, azim
   real(real64) :: height, rlat, rlon, ref, bend, impact, roc, geoid
   real(real64)    :: r_missing
   integer(int32)  :: i_missing
   integer(int64)  :: i64_missing

   logical, parameter :: GlobalModel = .true. ! temporary

   character(10) nemo
   character(80) hdr1a

   data hdr1a/'YEAR MNTH DAYS HOUR MINU SECO PCCF ELRC SAID SIID PTID GEODU SCLF OGCE TISE'/
   data nemo/'QFRO'/

!------------------------------------------------------------------------------!

! 1. Parse arguments
! ------------------

   call parse_arguments(cdtg_an,assim_win,c_ob_ff,version,endian,lmore)

! 2. Use environment variables to pass the file version and the path to the file
! ------------------------------------------------------------------------------

   if (instvar(1:4) .eq. 'mlsT') then
       call set_ev('MLS_LOC',c_ob_ff)
       call set_ev('MLS_VER',version)
   endif

! Set endian big/small through the enviroment
! --------------------------------------------

   if (instvar(1:4) .eq. 'mlsT') then
       call set_ev('F_UFMTENDIAN',endian)
   endif

! 3. Read the file using the reader routine
! -----------------------------------------

   read (assim_win,*) t_assim_win

   if (instvar(1:4) .eq. 'mlsT') then
       call read_mlsT_files(cdtg_an,t_assim_win)
   else
       write(*,*) " Platform ",TRIM(instvar)," not implemented"
       write(*,*) " Possible choices are: mlsT"
   endif

   STOP '9999'
!------------------------------------------------------------------------------!

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

   call check(nf90_def_var(grpid_metadata, "timeOffset", NF90_INT, nlocs_dimid, varid_tinc))
   call check(nf90_def_var_fill(grpid_metadata, varid_tinc, 0, i_missing))
   call check(nf90_def_var_deflate(grpid_metadata, varid_tinc,      &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_tinc, "longname", "time increment from the start time of the occultation"))
   call check(nf90_put_att(grpid_metadata, varid_tinc, "units", "second"))

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

   call check(nf90_def_var(grpid_metadata, "instrumentIdentifier", NF90_INT, nlocs_dimid, varid_siid))
   call check(nf90_def_var_deflate(grpid_metadata, varid_siid,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_siid, "longname", "satellite instrument"))
   call check(nf90_put_att(grpid_metadata, varid_siid, "units", "1"))
   call check(nf90_def_var_fill(grpid_metadata, varid_siid, 0, i_missing))

   call check(nf90_def_var(grpid_metadata, "qualityFlags", NF90_INT, nlocs_dimid, varid_qcfg))
   call check(nf90_def_var_deflate(grpid_metadata, varid_qcfg,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_qcfg, "longname", "RO QC flags based on the 16-digit binary table of WMO"))

   call check(nf90_def_var(grpid_metadata, "satelliteAscendingFlag", NF90_INT, nlocs_dimid, varid_asce))
   call check(nf90_def_var_deflate(grpid_metadata, varid_asce,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_asce, "longname", "the original occultation ascending/descending flag"))
   call check(nf90_put_att(grpid_metadata, varid_asce, "valid_range", int((/0, 1/))))
   call check(nf90_put_att(grpid_metadata, varid_asce, "flag_values", int((/0, 1/))))
   call check(nf90_put_att(grpid_metadata, varid_asce, "flag_meanings", "descending ascending"))
   call check(nf90_put_att(grpid_metadata, varid_asce, "units", "1"))
   call check(nf90_def_var_fill(grpid_metadata, varid_asce, 0, i_missing))

   call check(nf90_def_var(grpid_metadata, "dataProviderOrigin", NF90_INT, nlocs_dimid, varid_ogce))
   call check(nf90_def_var_deflate(grpid_metadata, varid_ogce,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_ogce, "longname", "originally data processing_center, &
                                                        &e.g., 60 for UCAR, 94 for DMI, 254 for Eumesat, 78 for GFZ"))
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
   call check(nf90_put_var(grpid_metadata, varid_tinc, gnssro_data%tinc(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_recn, gnssro_data%recn(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_qcfg, gnssro_data%qcflag(1:ndata)))
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
   deallocate (gnssro_data%qcflag)
   deallocate (gnssro_data%epochtime)
   deallocate (gnssro_data%tinc)
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
!------------------------------------------------------------------------------!

 Subroutine parse_arguments(cdtg_an,assim_win,c_ob_ff,version,endian,lmore)
!------------------------------------------------------------------------------!
      implicit none

      character(len=*), intent(out)  :: cdtg_an,assim_win,c_ob_ff,version,endian

      logical, intent(out) :: lmore

      integer, external :: iargc
      integer :: numarg,i
      logical :: lhelp
      logical :: lcdtg_an,lassim_win,lc_ob_ff,lversion,lendian

      character (len=512) :: harg
!------------------------------------------------------------------------------!

! 1.  Parse arguments
! -------------------

      numarg = iargc()

      IF (numarg == 0) CALL help

      i = 1
      lhelp      = .FALSE.
      lcdtg_an   = .FALSE.
      lassim_win = .FALSE.
      lc_ob_ff   = .FALSE.
      lversion   = .FALSE.
      lmore      = .FALSE.

      DO WHILE ( i <= numarg)

         CALL GETARG(i, harg)

         IF (harg == "-h" .OR. harg == "--h") THEN
             lhelp = .TRUE.
         ELSE IF (harg == "-help" .OR. harg == "--help") THEN
             lhelp = .TRUE.
         ELSE IF (harg == "-debug" .OR. harg == "--debug") THEN
             lmore = .TRUE.
         ELSEIF (harg == "-v" .OR. harg == "-version" .OR. harg == "--version") THEN
             i = i + 1
             CALL GETARG(i, harg)
             version = TRIM (harg)
             lversion = .TRUE.
         ELSE IF (harg == "-date" .OR. harg == "--date" ) THEN
              i = i + 1
              CALL GETARG(i, harg)
              cdtg_an = TRIM (harg)
              lcdtg_an = .TRUE.
         ELSE IF (harg == "-window" .OR. harg == "--window" ) THEN
              i = i + 1
              CALL GETARG(i, harg)
              assim_win = TRIM (harg)
              lassim_win = .TRUE.
         ELSE IF (harg == "-i" .OR. harg == "--i" .OR. &
                 (harg == "-input" .OR. harg == "--input" ) THEN
              i = i + 1
              CALL GETARG(i, harg)
              c_ob_ff = TRIM(harg)
              lc_ob_ff = .TRUE.
      ELSE IF (harg == "-endian" .OR. harg == "--endian") THEN
              i = i + 1
              CALL GETARG(i, harg)
              endian = TRIM(harg)
              lendian = .TRUE.
         ENDIF

         i = i + 1

      ENDDO

! 4.  Print help and exit
! -----------------------

      IF (lhelp) CALL help

! 3.  If version is not passed, set to 0
! --------------------------------------

      IF (.NOT. lversion) THEN
          version = "0"
      ENDIF

! 4.  If endian is not passes, set to big
! --------------------------------------

      IF (.NOT. lendian) THEN
          endian = "big"
      ENDIF


! 5.  Check required arguments
! ----------------------------

      IF (.NOT. lcdtg_an .OR. .NOT. lassim_win .OR. .NOT. lc_ob_ff) CALL help

! 6.  END
! -------

      RETURN

 END SUBROUTINE parse_arguments
!------------------------------------------------------------------------------!

 SUBROUTINE help
!------------------------------------------------------------------------------!
      IMPLICIT none
      CHARACTER (len=512) :: cmd
!------------------------------------------------------------------------------!

      CALL GETARG(0, cmd)

      WRITE (0,'(/,A,/,A)') "USAGE: ","------"

      WRITE (0,'(/,1X,4A,/,28X,3A)') trim (cmd), &
              " [-help -debug] -date analysis_dtg -window assimilation_time_window -input path_to_files -v file_version -endian little/big"

      WRITE (0,'(/,1X,A)') &
       "To convert MLS intermediate binary files (*.dat) into IODA files, with:"

      WRITE (0,'(/,1X,A)') &
       "- analysis_date as CCYYMMDDHH"
      WRITE (0,'(/,1X,A)') &
       "- assimilation_time_window in hours"
      WRITE (0,'(/,1X,A)') &
       "- path_to_files is the path to the directory holding the input files (256 characters max)"
      WRITE (0,'(/,2(1X,A))') &
      "- version is the version of the input file processing", &
      "expect one number, eg: 4, ommit if not known"
      WRITE (0,'(/,1X,A)') &
      "- endian of the binary file to read (big on Narwhal)"

      WRITE (0,'(/,A)') "------"

      STOP

 END SUBROUTINE help
!------------------------------------------------------------------------------!
 end program
