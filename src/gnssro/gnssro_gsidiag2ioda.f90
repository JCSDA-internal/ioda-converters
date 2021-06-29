!--- Hailing Zhang 2021-05-20
!--- This is to further process the GSI Diag netcdf file
!    from the JCSDA GSI branch feature/jedi_gdas
!    it 1) reorders the randomly distributed observations
!          and output observations in profile record numbers,
!       2) adds variable descriptions, and
!       3) output observation and geoval(optional) files seperately.
!-----------------------------------------------------------------
!-- TO COMPILE standalone executable
!ifort -o  gnssro_gsidiag2ioda.exe
!gnssro_gsidiag2ioda.f90 -L${NETCDF}/lib -lnetcdf -lnetcdff
!-lm -I${NETCDF}/include -I${NETCDF}/include
!-- TO USE
! gnssro_gsidiag2ioda.exe  yyyymmddhh $gsidiag_input 1  !!!! 1
! to write geoval/0 not

program gnssro_gsidiag_full2small
use netcdf
implicit none
integer, parameter :: i_kind   = selected_int_kind(8)
integer, parameter :: r_single = selected_real_kind(4) 

character(len=256)   :: filename_in, obsname_out, geoname_out
integer   :: ncid_in, ncid_out, ncid_out2
integer   :: nobs_dimid, nlevs_dimid, nlevs1_dimid
integer   :: nlevs, nlevs1, nobs, nlocs, nrecs
integer   :: nlocs_dimid,nvars_dimid,nrecs_dimid, nlocs_dimid2
integer   :: varid_meta,varid_geoval_nlevs,varid_geoval_nlevs1  
integer   :: varid_lat, varid_lon, varid_time
integer   :: varid_lat2,varid_lon2,varid_time2 
integer   :: varid_geoid, varid_rfict
integer   :: varid_rec, varid_said,varid_siid,varid_ogce,varid_ptid,varid_sclf,varid_asce
integer   :: varid_imph,varid_impp, varid_azim 
integer   :: varid_bnd,varid_preqc, varid_gsihofx,varid_obserr,varid_obserr_adjust,varid_obserr_final
integer   :: varid_geo_virtemp,varid_geo_airtemp,varid_geo_pres,varid_geo_shum,varid_geo_geop
integer   :: varid_geo_pres1, varid_geo_geop1
integer   :: varid_geo_geop_sfc
integer   :: istart(2), icount(2)
real(r_single)            :: zero_single
real(r_single), parameter :: huge_single = huge(zero_single) -1
character(len=10)         :: anatime
character(len=1)          :: geovalwrite
integer(i_kind)           :: anatime_i

type gpsro_type
      integer,  allocatable  :: said(:)
      integer,  allocatable  :: siid(:)
      integer,  allocatable  :: ogce(:)
      integer,  allocatable  :: sclf(:)
      integer,  allocatable  :: ptid(:)
      integer,  allocatable  :: asce(:)
      integer,  allocatable  :: record_number(:)

      real(r_single), allocatable  :: lat(:)
      real(r_single), allocatable  :: lon(:)
      real(r_single), allocatable  :: time(:)
      real(r_single), allocatable  :: geoid(:)
      real(r_single), allocatable  :: rfict(:)
      real(r_single), allocatable  :: azim(:)
      real(r_single), allocatable  :: impact_height(:)
      real(r_single), allocatable  :: impact_parameter(:)
      real(r_single), allocatable  :: bnd_obs(:)
      integer,        allocatable  :: bnd_preqc(:)
      real(r_single), allocatable  :: bnd_obserr(:)
      real(r_single), allocatable  :: bnd_gsihofx(:)
      real(r_single), allocatable  :: bnd_obserr_final(:)
      real(r_single), allocatable  :: bnd_obserr_adjust(:)
      real(r_single), allocatable  :: surface_geopotential_height(:)
      real(r_single), allocatable  :: air_temperature(:,:)
      real(r_single), allocatable  :: virtual_temperature(:,:)
      real(r_single), allocatable  :: specific_humidity(:,:)
      real(r_single), allocatable  :: geopotential_height(:,:)
      real(r_single), allocatable  :: air_pressure(:,:)
      real(r_single), allocatable  :: geopotential_height_levels(:,:)
      real(r_single), allocatable  :: air_pressure_levels(:,:)
end type gpsro_type

type(gpsro_type) :: gpsro_data
type(gpsro_type) :: gpsro_subset
type(gpsro_type) :: gpsro_reorder
integer          :: nsub, i, j, k,substart, subend,narg

narg=command_argument_count()
call getarg(1,anatime)
call getarg(2,filename_in)
! optional 3rd argument: "1" means geoval will be written
if (narg == 3) then
  call getarg(3,geovalwrite)
else
  geovalwrite="1"
end if
read(anatime,'(i10)') anatime_i

obsname_out = './gnssro_obs_'//trim(anatime)//'.nc4'

call check(nf90_open(trim(filename_in), nf90_nowrite, ncid_in))
call check(nf90_inq_dimid(ncid_in, "nobs", nobs_dimid))
call check(nf90_inq_dimid(ncid_in, "air_temperature_arr_dim", nlevs_dimid))
call check(nf90_inq_dimid(ncid_in, "air_pressure_levels_arr_dim", nlevs1_dimid))
call check(nf90_inquire_dimension(ncid_in, nobs_dimid,   len=nobs))
call check(nf90_inquire_dimension(ncid_in, nlevs_dimid,  len=nlevs))
call check(nf90_inquire_dimension(ncid_in, nlevs1_dimid, len=nlevs1))

allocate(gpsro_data%lat(nobs))
allocate(gpsro_data%lon(nobs))
allocate(gpsro_data%time(nobs))
allocate(gpsro_data%asce(nobs))
allocate(gpsro_data%azim(nobs))
allocate(gpsro_data%geoid(nobs))
allocate(gpsro_data%rfict(nobs))
allocate(gpsro_data%impact_height(nobs))
allocate(gpsro_data%impact_parameter(nobs))
allocate(gpsro_data%said(nobs))
allocate(gpsro_data%siid(nobs))
allocate(gpsro_data%ogce(nobs))
allocate(gpsro_data%ptid(nobs))
allocate(gpsro_data%sclf(nobs))
allocate(gpsro_data%record_number(nobs))
allocate(gpsro_data%bnd_obs(nobs))
allocate(gpsro_data%bnd_preqc(nobs))
allocate(gpsro_data%bnd_obserr(nobs))
allocate(gpsro_data%bnd_obserr_final(nobs))
allocate(gpsro_data%bnd_obserr_adjust(nobs))
allocate(gpsro_data%bnd_gsihofx(nobs))

call check(NF90_INQ_VARID(ncid_in, 'record_number@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%record_number, (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in, 'latitude@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%lat(:), (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in, 'longitude@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%lon(:), (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'time@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%time(:), (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'impact_height@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%impact_height, (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'impact_parameter@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%impact_parameter, (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'earth_radius_of_curvature@MetaData',varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%rfict, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,'geoid_height_above_reference_ellipsoid@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%geoid, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'occulting_sat_id@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%said, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'occulting_sat_is@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%siid, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'process_center@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%ogce, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'reference_sat_id', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%ptid, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'gnss_sat_class@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%sclf, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'sensor_azimuth_angle@MetaData',varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%azim, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'ascending_flag@MetaData', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%asce, (/1/), (/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'bending_angle@ObsValue', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%bnd_obs, (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'bending_angle@PreQC', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%bnd_preqc, (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'bending_angle@GsiFinalObsErrorInv',varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%bnd_obserr_final, (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'bending_angle@GsiAdjustObsErrorInv',varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%bnd_obserr_adjust, (/1/),(/nobs/)))
call check(NF90_INQ_VARID(ncid_in,     'bending_angle@GsiHofX', varid_meta))
call check(NF90_GET_VAR(ncid_in, varid_meta, gpsro_data%bnd_gsihofx, (/1/),(/nobs/)))

where(gpsro_data%bnd_obserr_adjust>0.0 )
  gpsro_data%bnd_obserr_adjust = 1.0/gpsro_data%bnd_obserr_adjust
elsewhere
  gpsro_data%bnd_obserr_adjust = huge_single
endwhere

where(gpsro_data%bnd_obserr_final>0.0 )
  gpsro_data%bnd_obserr_final  = 1.0/gpsro_data%bnd_obserr_final
elsewhere
  gpsro_data%bnd_obserr_final  = huge_single
endwhere
gpsro_data%bnd_obserr   = gpsro_data%bnd_obserr_adjust

if ( geovalwrite == "1" )  then
   geoname_out='./gnssro_geoval_'//trim(anatime)//'.nc4'
   allocate(gpsro_data%air_temperature(nlevs,nobs))
   allocate(gpsro_data%virtual_temperature(nlevs,nobs))
   allocate(gpsro_data%specific_humidity(nlevs,nobs))
   allocate(gpsro_data%geopotential_height(nlevs,nobs))
   allocate(gpsro_data%geopotential_height_levels(nlevs1,nobs))
   allocate(gpsro_data%air_pressure(nlevs,nobs))
   allocate(gpsro_data%air_pressure_levels(nlevs1,nobs))
   allocate(gpsro_data%surface_geopotential_height(nobs))
   call check(NF90_INQ_VARID(ncid_in, 'surface_geopotential_height', varid_meta))
   call check(NF90_GET_VAR(ncid_in, varid_meta,gpsro_data%surface_geopotential_height, (/1/), (/nobs/)))
   call check(NF90_INQ_VARID(ncid_in, 'air_temperature', varid_geoval_nlevs)) 
   call check(NF90_GET_VAR(ncid_in, varid_geoval_nlevs,gpsro_data%air_temperature, start=(/1,1/), count=(/nlevs,nobs/))) 
   call check(NF90_INQ_VARID(ncid_in, 'virtual_temperature', varid_geoval_nlevs))   
   call check(NF90_GET_VAR(ncid_in,varid_geoval_nlevs,gpsro_data%virtual_temperature, start=(/1,1/),count=(/nlevs,nobs/)))  
   call check(NF90_INQ_VARID(ncid_in, 'specific_humidity', varid_meta))
   call check(NF90_GET_VAR(ncid_in, varid_meta,  gpsro_data%specific_humidity,start=(/1,1/), count=(/nlevs,nobs/)))
   call check(NF90_INQ_VARID(ncid_in, 'air_pressure', varid_geoval_nlevs))
   call check(NF90_GET_VAR(ncid_in, varid_geoval_nlevs, gpsro_data%air_pressure,start=(/1,1/), count=(/nlevs, nobs/)))
   call check(NF90_INQ_VARID(ncid_in, 'air_pressure_levels', varid_geoval_nlevs1))
   call check(NF90_GET_VAR(ncid_in, varid_geoval_nlevs1,gpsro_data%air_pressure_levels, start=(/1,1/), count=(/nlevs1, nobs/)))
   call check(NF90_INQ_VARID(ncid_in, 'geopotential_height', varid_geoval_nlevs))
   call check(NF90_GET_VAR(ncid_in, varid_geoval_nlevs,gpsro_data%geopotential_height, start=(/1,1/), count=(/nlevs,nobs/)))
   call check(NF90_INQ_VARID(ncid_in, 'geopotential_height_levels',varid_geoval_nlevs1))
   call check(NF90_GET_VAR(ncid_in, varid_geoval_nlevs1,gpsro_data%geopotential_height_levels, start=(/1,1/), &
   count=(/nlevs1, nobs/)))

   allocate(gpsro_subset%air_temperature(nlevs,nobs))
   allocate(gpsro_subset%virtual_temperature(nlevs,nobs))
   allocate(gpsro_subset%specific_humidity(nlevs,nobs))
   allocate(gpsro_subset%geopotential_height(nlevs,nobs))
   allocate(gpsro_subset%geopotential_height_levels(nlevs1,nobs))
   allocate(gpsro_subset%air_pressure(nlevs,nobs))
   allocate(gpsro_subset%air_pressure_levels(nlevs1,nobs))
   allocate(gpsro_subset%surface_geopotential_height(nobs))

end if

allocate(gpsro_subset%lat(nobs))
allocate(gpsro_subset%lon(nobs))
allocate(gpsro_subset%time(nobs))
allocate(gpsro_subset%asce(nobs))
allocate(gpsro_subset%azim(nobs))
allocate(gpsro_subset%geoid(nobs))
allocate(gpsro_subset%rfict(nobs))
allocate(gpsro_subset%impact_height(nobs))
allocate(gpsro_subset%impact_parameter(nobs))
allocate(gpsro_subset%said(nobs))
allocate(gpsro_subset%siid(nobs))
allocate(gpsro_subset%ogce(nobs))
allocate(gpsro_subset%ptid(nobs))
allocate(gpsro_subset%sclf(nobs))
allocate(gpsro_subset%record_number(nobs))
allocate(gpsro_subset%bnd_obs(nobs))
allocate(gpsro_subset%bnd_preqc(nobs))
allocate(gpsro_subset%bnd_obserr(nobs))
allocate(gpsro_subset%bnd_obserr_adjust(nobs))
allocate(gpsro_subset%bnd_obserr_final(nobs))
allocate(gpsro_subset%bnd_gsihofx(nobs))

nsub     = 0
nrecs    = maxval(gpsro_data%record_number(:))

do j = 1,  maxval(gpsro_data%record_number(:))
  do i = 1, nobs
    if ( gpsro_data%record_number(i) .eq. j )  then
        nsub=nsub+1
        gpsro_subset%lat(nsub) = gpsro_data%lat(i)
        gpsro_subset%lon(nsub) = gpsro_data%lon(i)
        gpsro_subset%time(nsub) = gpsro_data%time(i)
        if (gpsro_subset%time(nsub) .eq. -3.00) gpsro_subset%time(nsub) = -2.999
        gpsro_subset%impact_height(nsub)    = gpsro_data%impact_height(i)
        gpsro_subset%impact_parameter(nsub) = gpsro_data%impact_parameter(i)
        gpsro_subset%geoid(nsub) = gpsro_data%geoid(i)
        gpsro_subset%rfict(nsub) = gpsro_data%rfict(i)
        gpsro_subset%said(nsub)  = gpsro_data%said(i)
        gpsro_subset%siid(nsub)  = gpsro_data%siid(i)
        gpsro_subset%ogce(nsub)  = gpsro_data%ogce(i)
        gpsro_subset%ptid(nsub)  = gpsro_data%ptid(i)
        gpsro_subset%sclf(nsub)  = gpsro_data%sclf(i)
        gpsro_subset%record_number(nsub) = gpsro_data%record_number(i)
        gpsro_subset%azim(nsub) = gpsro_data%azim(i)
        gpsro_subset%asce(nsub) = gpsro_data%asce(i)
        gpsro_subset%bnd_obs(nsub) = gpsro_data%bnd_obs(i)
        gpsro_subset%bnd_preqc(nsub) = gpsro_data%bnd_preqc(i)
        gpsro_subset%bnd_obserr(nsub) = gpsro_data%bnd_obserr(i)
        gpsro_subset%bnd_obserr_final(nsub) =gpsro_data%bnd_obserr_final(i)
        gpsro_subset%bnd_obserr_adjust(nsub) = gpsro_data%bnd_obserr_adjust(i)
        gpsro_subset%bnd_gsihofx(nsub)  = gpsro_data%bnd_gsihofx(i)
        if ( geovalwrite == "1" )  then
          gpsro_subset%surface_geopotential_height(nsub)  = gpsro_data%surface_geopotential_height(i)
          gpsro_subset%air_temperature(:,nsub)            = gpsro_data%air_temperature(:,i)
          gpsro_subset%virtual_temperature(:,nsub)        = gpsro_data%virtual_temperature(:,i)
          gpsro_subset%specific_humidity(:,nsub)          = gpsro_data%specific_humidity(:, i )
          gpsro_subset%air_pressure(:,nsub)               = gpsro_data%air_pressure(:, i )
          gpsro_subset%air_pressure_levels(:,nsub)        = gpsro_data%air_pressure_levels(:, i )
          gpsro_subset%geopotential_height(:,nsub)        = gpsro_data%geopotential_height(:, i )
          gpsro_subset%geopotential_height_levels(:,nsub) = gpsro_data%geopotential_height_levels(:,i)
        endif

     end if
  end do
end do

call check( nf90_create(trim(obsname_out), nf90_clobber, ncid_out))
call check( nf90_put_att(ncid_out, NF90_GLOBAL, 'date_time', anatime_i) )
call check( nf90_def_dim(ncid_out, 'nlocs', nsub,  nlocs_dimid) )
call check( nf90_def_dim(ncid_out, 'nrecs', nrecs, nrecs_dimid) )
call check( nf90_def_var(ncid_out, "latitude@MetaData",        NF90_FLOAT, nlocs_dimid, varid_lat) )
call check( nf90_def_var(ncid_out, "longitude@MetaData",       NF90_FLOAT, nlocs_dimid, varid_lon) )
call check( nf90_def_var(ncid_out, "time@MetaData",            NF90_FLOAT, nlocs_dimid, varid_time) )
call check( nf90_def_var(ncid_out, "impact_height@MetaData",   NF90_FLOAT, nlocs_dimid, varid_imph))
call check( nf90_put_att(ncid_out, varid_imph, "longname", "distance from mean sea level" ))
call check( nf90_put_att(ncid_out, varid_imph, "units", "Meters" ))
call check( nf90_put_att(ncid_out, varid_imph, "valid_range", "0 - 200 km" ))
call check( nf90_def_var(ncid_out, "impact_parameter@MetaData",NF90_FLOAT, nlocs_dimid, varid_impp))
call check( nf90_put_att(ncid_out, varid_impp, "longname", "distance from centre of curvature" ))
call check( nf90_put_att(ncid_out, varid_impp, "units", "Meters" ))
call check( nf90_put_att(ncid_out, varid_impp, "valid_range", "6200 - 6600 km" ))
call check( nf90_def_var(ncid_out, "geoid_height_above_reference_ellipsoid@MetaData",    NF90_FLOAT, nlocs_dimid, varid_geoid))
call check( nf90_put_att(ncid_out, varid_geoid, "longname", "Geoid height above WGS-84 ellipsoid" ))
call check( nf90_put_att(ncid_out, varid_geoid, "units", "Meters" ))
call check( nf90_put_att(ncid_out, varid_geoid, "valid_range", "-200 - 200 m" ))
call check( nf90_def_var(ncid_out, "earth_radius_of_curvature@MetaData", NF90_FLOAT, nlocs_dimid, varid_rfict))
call check( nf90_put_att(ncid_out, varid_rfict, "longname", "Earth’s local radius of curvature" ))
call check( nf90_put_att(ncid_out, varid_rfict, "units", "Meters" ))
call check( nf90_put_att(ncid_out, varid_rfict, "valid_range", "6200 - 6600 km" ))
call check( nf90_def_var(ncid_out, "sensor_azimuth_angle@MetaData",NF90_FLOAT, nlocs_dimid, varid_azim))
call check( nf90_put_att(ncid_out, varid_azim, "longname", "GNSS->LEO line of sight" ))
call check( nf90_put_att(ncid_out, varid_azim, "units", "Degree" ))
call check( nf90_put_att(ncid_out, varid_azim, "valid_range", "0 - 360 degree" ))
call check( nf90_def_var(ncid_out, "record_number@MetaData",    NF90_INT,nlocs_dimid, varid_rec))
call check( nf90_put_att(ncid_out, varid_rec, "longname", "GNSS RO profile identifier" ))
call check( nf90_def_var(ncid_out, "occulting_sat_id@MetaData", NF90_INT, nlocs_dimid, varid_said))
call check( nf90_put_att(ncid_out, varid_said, "longname", "Low Earth Orbit satellite identifier, e.g., COSMIC2=750-755" ))
call check( nf90_def_var(ncid_out, "occulting_sat_is@MetaData", NF90_INT, nlocs_dimid, varid_siid))
call check( nf90_put_att(ncid_out, varid_siid, "longname", "satellite instrument" ))
call check( nf90_def_var(ncid_out, "process_center@MetaData", NF90_INT, nlocs_dimid, varid_ogce))
call check( nf90_put_att(ncid_out, varid_ogce, "longname", "originally data processing_center,   e.g., 60 for UCAR, 94 for DMI" ))
call check( nf90_def_var(ncid_out, "reference_sat_id@MetaData", NF90_INT, nlocs_dimid, varid_ptid))
call check( nf90_put_att(ncid_out, varid_ptid, "longname", "GNSS satellite transmitter identifier (1-32)" ))
call check( nf90_def_var(ncid_out, "gnss_sat_class@MetaData",   NF90_INT, nlocs_dimid, varid_sclf))
call check( nf90_put_att(ncid_out, varid_sclf, "longname", "GNSS satellite classification, e.g, 401=GPS, 402=GLONASS" ))
call check( nf90_def_var(ncid_out, "ascending_flag@MetaData",   NF90_INT, nlocs_dimid, varid_asce))
call check( nf90_put_att(ncid_out, varid_asce, "longname", "the original occultation ascending/descending flag" ))
call check( nf90_def_var(ncid_out, "bending_angle@ObsValue",    NF90_FLOAT, nlocs_dimid, varid_bnd))
call check( nf90_put_att(ncid_out, varid_bnd, "longname",  "Bending Angle" ))
call check( nf90_put_att(ncid_out, varid_bnd, "units", "Radians" )) 
call check( nf90_put_att(ncid_out, varid_bnd, "valid_range", "-0.001 - 0.08 Radians" ))
call check( nf90_def_var(ncid_out, "bending_angle@ObsError",    NF90_FLOAT, nlocs_dimid, varid_obserr))
call check( nf90_put_att(ncid_out, varid_obserr, "longname", "GSI final observation error" ))
call check( nf90_put_att(ncid_out, varid_obserr, "units", "Radians" ))
call check( nf90_put_att(ncid_out, varid_obserr, "valid_range", "0 - 0.008 Radians" ))
call check( nf90_def_var(ncid_out, "bending_angle@GsiAdjustObsError", NF90_FLOAT, nlocs_dimid, varid_obserr_adjust))
call check( nf90_put_att(ncid_out, varid_obserr_adjust, "longname", "GSI adjust observation error" ))
call check( nf90_put_att(ncid_out, varid_obserr_adjust, "units", "Radians" ))
call check( nf90_put_att(ncid_out, varid_obserr_adjust, "valid_range", "0 - 0.008 Radians" ))
call check( nf90_def_var(ncid_out, "bending_angle@GsiFinalObsError", NF90_FLOAT, nlocs_dimid, varid_obserr_final))
call check( nf90_put_att(ncid_out, varid_obserr_final, "longname", "GSI final observation error (inflated)" ))
call check( nf90_put_att(ncid_out, varid_obserr_final, "units", "Radians" ))
call check( nf90_put_att(ncid_out, varid_obserr_final, "valid_range", "0 - 0.008 Radians" ))
call check( nf90_def_var(ncid_out, "bending_angle@PreQC",              NF90_INT,nlocs_dimid, varid_preqc))
call check( nf90_put_att(ncid_out, varid_preqc, "longname", "GSI output QC flags" ))
call check( nf90_put_att(ncid_out, varid_preqc, "valid_range", "1 - 5" ))
call check( nf90_def_var(ncid_out, "bending_angle@GsiHofX", NF90_FLOAT, nlocs_dimid, varid_gsihofx))
call check( nf90_enddef(ncid_out) )

if ( geovalwrite =="1" ) then
call check( nf90_create(trim(geoname_out), nf90_clobber, ncid_out2))
call check( nf90_put_att(ncid_out2, NF90_GLOBAL, 'date_time', anatime_i) )
call check( nf90_def_dim(ncid_out2, 'nlocs',  nsub,     nlocs_dimid2) )
call check( nf90_def_dim(ncid_out2, 'nlevs',  nlevs,    nlevs_dimid) )
call check( nf90_def_dim(ncid_out2, 'nlevs1', nlevs1,   nlevs1_dimid) )
call check( nf90_def_var(ncid_out2, "latitude",        NF90_FLOAT, nlocs_dimid, varid_lat2) )
call check( nf90_def_var(ncid_out2, "longitude",       NF90_FLOAT, nlocs_dimid, varid_lon2) )
call check( nf90_def_var(ncid_out2, "time",            NF90_FLOAT, nlocs_dimid, varid_time2) )
call check( nf90_def_var(ncid_out2, "surface_geopotential_height",     NF90_FLOAT, nlocs_dimid2, varid_geo_geop_sfc) )
call check( nf90_def_var(ncid_out2, "air_temperature",             NF90_FLOAT, (/nlevs_dimid,nlocs_dimid2/), varid_geo_airtemp) )
call check( nf90_def_var(ncid_out2, "virtual_temperature",         NF90_FLOAT, (/nlevs_dimid,nlocs_dimid2/), varid_geo_virtemp) )
call check( nf90_def_var(ncid_out2, "specific_humidity",           NF90_FLOAT, (/nlevs_dimid,nlocs_dimid2/), varid_geo_shum) )
call check( nf90_def_var(ncid_out2, "air_pressure",                NF90_FLOAT, (/nlevs_dimid,nlocs_dimid2/), varid_geo_pres))
call check( nf90_def_var(ncid_out2, "air_pressure_levels",         NF90_FLOAT, (/nlevs1_dimid,nlocs_dimid2/), varid_geo_pres1))
call check( nf90_def_var(ncid_out2, "geopotential_height",         NF90_FLOAT, (/nlevs_dimid,nlocs_dimid2/),  varid_geo_geop))
call check( nf90_def_var(ncid_out2, "geopotential_height_levels",  NF90_FLOAT, (/nlevs1_dimid,nlocs_dimid2/), varid_geo_geop1))
call check( nf90_enddef(ncid_out2) )
end if

call check( nf90_put_var(ncid_out, varid_lat,   gpsro_subset%lat(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_lon,   gpsro_subset%lon(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_time,  gpsro_subset%time(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_imph,  gpsro_subset%impact_height(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_impp,  gpsro_subset%impact_parameter(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_geoid, gpsro_subset%geoid(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_rfict, gpsro_subset%rfict(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_azim,  gpsro_subset%azim(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_rec,   gpsro_subset%record_number(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_said,  gpsro_subset%said(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_siid,  gpsro_subset%siid(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_ogce,  gpsro_subset%ogce(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_ptid,  gpsro_subset%ptid(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_sclf,  gpsro_subset%sclf(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_asce,  gpsro_subset%asce(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_bnd,   gpsro_subset%bnd_obs(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_preqc, gpsro_subset%bnd_preqc(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_obserr,        gpsro_subset%bnd_obserr(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_obserr_final,  gpsro_subset%bnd_obserr_final(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_obserr_adjust, gpsro_subset%bnd_obserr_adjust(1:nsub)) )
call check( nf90_put_var(ncid_out, varid_gsihofx,gpsro_subset%bnd_gsihofx(1:nsub)) )
call check( nf90_close(ncid_out) )

if ( geovalwrite == "1" ) then
   call check( nf90_put_var(ncid_out2, varid_lat2,   gpsro_subset%lat(1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_lon2,   gpsro_subset%lon(1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_time2,  gpsro_subset%time(1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_geo_geop_sfc,gpsro_subset%surface_geopotential_height(1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_geo_airtemp,gpsro_subset%air_temperature(1:nlevs,1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_geo_virtemp,gpsro_subset%virtual_temperature(1:nlevs,1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_geo_shum,gpsro_subset%specific_humidity(1:nlevs,1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_geo_pres,gpsro_subset%air_pressure(1:nlevs,1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_geo_geop,gpsro_subset%geopotential_height(1:nlevs,1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_geo_pres1,gpsro_subset%air_pressure_levels(1:nlevs1,1:nsub)) )
   call check( nf90_put_var(ncid_out2, varid_geo_geop1,gpsro_subset%geopotential_height_levels(1:nlevs1,1:nsub)) )
   call check( nf90_close(ncid_out2) )
end if

deallocate(gpsro_data%lat)
deallocate(gpsro_data%lon)
deallocate(gpsro_data%time)
deallocate(gpsro_data%asce)
deallocate(gpsro_data%azim)
deallocate(gpsro_data%geoid)
deallocate(gpsro_data%rfict)
deallocate(gpsro_data%impact_height)
deallocate(gpsro_data%impact_parameter)
deallocate(gpsro_data%said)
deallocate(gpsro_data%siid)
deallocate(gpsro_data%ogce)
deallocate(gpsro_data%ptid)
deallocate(gpsro_data%sclf)
deallocate(gpsro_data%record_number)
deallocate(gpsro_data%bnd_obs)
deallocate(gpsro_data%bnd_preqc)
deallocate(gpsro_data%bnd_obserr)
deallocate(gpsro_data%bnd_obserr_final)
deallocate(gpsro_data%bnd_obserr_adjust)
deallocate(gpsro_data%bnd_gsihofx)

deallocate(gpsro_subset%lat)
deallocate(gpsro_subset%lon)
deallocate(gpsro_subset%time)
deallocate(gpsro_subset%asce)
deallocate(gpsro_subset%azim)
deallocate(gpsro_subset%geoid)
deallocate(gpsro_subset%rfict)
deallocate(gpsro_subset%impact_height)
deallocate(gpsro_subset%impact_parameter)
deallocate(gpsro_subset%said)
deallocate(gpsro_subset%siid)
deallocate(gpsro_subset%ogce)
deallocate(gpsro_subset%ptid)
deallocate(gpsro_subset%sclf)
deallocate(gpsro_subset%record_number)
deallocate(gpsro_subset%bnd_obs)
deallocate(gpsro_subset%bnd_preqc)
deallocate(gpsro_subset%bnd_obserr)
deallocate(gpsro_subset%bnd_obserr_adjust)
deallocate(gpsro_subset%bnd_obserr_final)
deallocate(gpsro_subset%bnd_gsihofx)

if ( geovalwrite == "1" )  then
   deallocate(gpsro_data%air_temperature)
   deallocate(gpsro_data%virtual_temperature)
   deallocate(gpsro_data%specific_humidity)
   deallocate(gpsro_data%geopotential_height)
   deallocate(gpsro_data%geopotential_height_levels)
   deallocate(gpsro_data%air_pressure)
   deallocate(gpsro_data%air_pressure_levels)
   deallocate(gpsro_data%surface_geopotential_height)
   deallocate(gpsro_subset%air_temperature)
   deallocate(gpsro_subset%virtual_temperature)
   deallocate(gpsro_subset%specific_humidity)
   deallocate(gpsro_subset%geopotential_height)
   deallocate(gpsro_subset%geopotential_height_levels)
   deallocate(gpsro_subset%air_pressure)
   deallocate(gpsro_subset%air_pressure_levels)
   deallocate(gpsro_subset%surface_geopotential_height)
end if

contains
 subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
     print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if

  end subroutine check


end program  gnssro_gsidiag_full2small
