
 program mlsT_dat2ioda
!------------------------------------------------------------------------------!
! Return codes:
!  0 - Success.
!  1 - Urecnoverable system or logical error.
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

! 4. Construct the output file name
! ---------------------------------

   outfile = "obs."//TRIM(instvar)//"."//TRIM(cdtg_in)//".PT"//assim_win//"H.nc4"

! 5.  Write out at IODA format  
! ----------------------------

   CALL mlsT_write2ioda(outfile,cdtg_in,maxprof,maxlev, &
                        mls_val,mls_err,mls_p,mls_lat,mls_lon,mls_dt, &
                        nmls,nlev,instvar)

! 5.  END
! -------

   STOP '9999'

 END PROGRAM mlsT_dat2ioda
!------------------------------------------------------------------------------!

 SUBROUTINE mlsT_write2ioda(outfile,cdtg_in,maxprof,maxlev, &
                            mls_val,mls_err,mls_p,mls_lat,mls_lon,mls_dt, &
                            nmls,nlev,instvar)

!------------------------------------------------------------------------------!

   implicit none

   character(len=128), intent(in) :: outfile ! Output file name
   character(len=10),  intent(in) :: cdtg_in ! analysis time

!------------------------------------------------------------------------------!
! Input obs data
  integer, intent(in) :: maxprof,maxlev
  real, intent(in) ::    mls_val(maxprof,maxlev)  ! T(K), O3(ppmv), or H2O(ppmv)
  real, intent(in) ::    mls_err(maxprof,maxlev)  ! error stdev 
  real, intent(in) ::    mls_p(maxlev)     ! pressure (hPa)
  real, intent(in) ::    mls_lat(maxprof)  ! latitude (deg)
  real, intent(in) ::    mls_lon(maxprof)  ! longitude (deg)
  integer, intent(in) :: mls_dt(maxprof)   ! time offset in seconds from anaysis time
  integer, intent(in) :: nmls              ! number of mls measurements
  integer, intent(in) :: nlev 

   character(len=7), intent(in) :: instvar
!------------------------------------------------------------------------------!
! Output obs data stucture

   integer   :: ncid
   integer   :: nobs_dimid, nlocs_dimid, nvars_dimid, recns_dimid
   integer   :: varid_lat, varid_lon, varid_pres
   integer   :: varid_epochtime
   integer   :: varid_tinc, varid_recn
   integer   :: varid_obsvalue, varid_obserror, varid_preqc

   integer   :: grpid_metadata, grpid_obsvalue, grpid_obserror, grpid_preqc

   integer, parameter :: deflate_level = 6

   real(real64)    :: r_missing
   integer(int32)  :: i_missing
   integer(int64)  :: i64_missing

!------------------------------------------------------------------------------!

  character(len=10) :: anatime

  integer(int32) :: ndata, irec

  integer(int32), allocatable, dimension(:)   :: recn
  integer(int32), allocatable, dimension(:)   :: toffset
  integer(int32), allocatable, dimension(:)   :: preqc

  real(real64), allocatable, dimension(:)     :: longitude
  real(real64), allocatable, dimension(:)     :: latitude
  real(real64), allocatable, dimension(:)     :: pressure
  real(real64), allocatable, dimension(:)     :: obsvalue
  real(real64), allocatable, dimension(:)     :: obserror

!------------------------------------------------------------------------------!

    ! data are saved as data mls*nlev data points
    ! data with same recn value belongs to the same profile

    ndata = nmls*nlev
    anatime = cdtg_an

    allocate (toffset(ndata))
    allocate (lat(ndata))
    allocate (lon(ndata))

    allocate (pressure(ndata))

    allocate (obsvalue(ndata))
    allocate (obserror(ndata))

    allocate (preqc(ndata))
    allocate (recn(ndata))


    irec = 1 ! How many profiles (should be = nmls)

    do n = 1, nmls

       do k = 1, nlev

          latitude(n+k-1)  = mls_lat(n)
          longitude(n+k-1) = mls_lon(n)
          pressure(n+k-1)  = mls_p(n)*100.  ! Pa
          toffset(n+k-1)   = mls_dt(n)

          obsvalue(n+k-1)  = mls_val(n,k)
          obserror(n+k-1)  = mls_err(n,k)

          preqc(n+k-1)     = 0

          recn(n+nlev-1)   = irec

       end do 

       irec = irec + 1

    end do

!------------------------------------------------------------------------------!

! Open output file
   call check(nf90_create(trim(outfile), NF90_NETCDF4, ncid))

! Write file dimensions
   call check(nf90_def_dim(ncid, 'Location', ndata, nlocs_dimid))

! Write attributes
   call check(nf90_put_att(ncid, NF90_GLOBAL, 'date_time', anatime))
   call check(nf90_put_att(ncid, NF90_GLOBAL, 'ioda_version', 'Fortran generated ioda file'))

! Define groups
   call check(nf90_def_grp(ncid, 'MetaData', grpid_metadata))
   call check(nf90_def_grp(ncid, 'ObsValue', grpid_obsvalue))
   call check(nf90_def_grp(ncid, 'ObsError', grpid_obserror))
   call check(nf90_def_grp(ncid, 'PreQC',    grpid_preqc))

! Define variables in Metadata group
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
   call check(nf90_put_att(grpid_metadata, varid_tinc, "longname", "time increment from the analysis time"))
   call check(nf90_put_att(grpid_metadata, varid_tinc, "units", "second"))

   call check(nf90_def_var(grpid_metadata, "sequenceNumber", NF90_INT, nlocs_dimid, varid_recn))
   call check(nf90_def_var_fill(grpid_metadata, varid_recn, 0, i_missing))
   call check(nf90_def_var_deflate(grpid_metadata, varid_recn,      &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_metadata, varid_recn, "longname", "profile identifier"))
   call check(nf90_put_att(grpid_metadata, varid_recn, "units", "1"))

   call check(nf90_def_var(grpid_metadata, "pressure", NF90_FLOAT, nlocs_dimid, varid_pres))
   call check(nf90_def_var_deflate(grpid_metadata, varid_pres,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))

   call check(nf90_put_att(grpid_metadata, varid_pres, "longname", "Pressure"))
   call check(nf90_put_att(grpid_metadata, varid_pres, "units", "Pa"))
   call check(nf90_put_att(grpid_metadata, varid_pres, "valid_range", real((/saber_pmin*100.,mls_pmax*100./))))
   call check(nf90_def_var_fill(grpid_metadata, varid_pres, 0, real(r_missing)))

! Define obsvalue variable
   call check(nf90_def_var(grpid_obsvalue, "Temperature", NF90_FLOAT, nlocs_dimid, varid_obsvalue))
   call check(nf90_def_var_deflate(grpid_obsvalue, varid_obsvalue,       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_obsvalue, varid_obsvalue, "longname", "Temperature"))
   call check(nf90_put_att(grpid_obsvalue, varid_obsvalue, "units", "K"))
   call check(nf90_put_att(grpid_obsvalue, varid_obsvalue, "valid_range", real((/0., 0.08/))))

   call check(nf90_def_var_fill(grpid_obsvalue, varid_obsvalue, 0, real(r_missing)))

! Define obserror variable
   call check(nf90_def_var(grpid_obserror, "Temperature", NF90_FLOAT, nlocs_dimid, varid_obserror))
   call check(nf90_def_var_deflate(grpid_obserror, varid_obserror       &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_obserror, varid_obserror, "longname", "Temperature"))
   call check(nf90_put_att(grpid_obserror, varid_obserror, "units", "K"))
   call check(nf90_put_att(grpid_obserror, varid_obserror, "valid_range", real((/0., 0.08/))))

   call check(nf90_def_var_fill(grpid_obserror, varid_obserror, 0, real(r_missing)))

! Define preqc variable
   call check(nf90_def_var(grpid_preqc, "Temperature", NF90_INT32, nlocs_dimid, varid_preqc))
   call check(nf90_def_var_deflate(grpid_preqc, varid_preqc,      &
                                    & shuffle=1, deflate=1, deflate_level=deflate_level))
   call check(nf90_put_att(grpid_preqc, varid_preqc, "longname", "Temperature"))
   call check(nf90_put_att(grpid_preqc, varid_preqc, "units", "K"))
   call check(nf90_put_att(grpid_preqc, varid_preqc, "valid_range", real((/0., 30/))))

   call check(nf90_def_var_fill(grpid_preqc, varid_preqc, 0, real(i_missing)))

! End variable definition mode
   call check(nf90_enddef(ncid))

! Write data
   call check(nf90_put_var(grpid_obsvalue, varid_obsvalue, obsvalue(1:ndata)))
   call check(nf90_put_var(grpid_obserror, varid_obserror, obserror(1:ndata)))
   call check(nf90_put_var(grpid_preqc,    varid_preqc, preqc(1:ndata)))

   call check(nf90_put_var(grpid_metadata, varid_lat,  latitude(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_lon,  longitude(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_pres, pressure(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_tinc, tinc(1:ndata)))
   call check(nf90_put_var(grpid_metadata, varid_recn, recn(1:ndata)))

! Close ouput file
   call check(nf90_close(ncid))

! Deallocate memory

    deallocate (latitude)

    deallocate (longitude)

    deallocate (pressure)

    deallocate (toffset)

    deallocate (obsvalue)
    deallocate (obserror)

    deallocate (preqc)
    deallocate (recn)

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
                  harg == "-input" .OR. harg == "--input" ) THEN
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
