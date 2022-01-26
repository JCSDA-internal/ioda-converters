module define_mod

use kinds, only: r_kind, i_kind, i_llong
use ufo_vars_mod, only: var_ps, var_prs, var_u, var_v, var_ts, var_tv, var_q, var_tb
use netcdf, only: nf90_float, nf90_int, nf90_char, nf90_int64

implicit none

real(r_kind),    parameter :: t_kelvin          = 273.15
real(r_kind),    parameter :: missing_r         = -999.0
integer(i_kind), parameter :: StrLen            = 512
integer(i_kind), parameter :: missing_i         = -999
integer(i_kind), parameter :: not_use           = 100
integer(i_kind), parameter :: itrue             = 1
integer(i_kind), parameter :: ifalse            = 0
integer(i_kind), parameter :: nstring           = 50
integer(i_kind), parameter :: ndatetime         = 20
integer(i_kind), parameter :: nobtype           = 7  ! number of ob types
integer(i_kind), parameter :: n_ncdim           = 3  ! total numner of nc dimensions
integer(i_kind), parameter :: n_ncgrp           = 5  ! total numner of nc groups
integer(i_kind), parameter :: nvar_met          = 6
integer(i_kind), parameter :: nvar_info         = 9  ! number of metadata
integer(i_kind), parameter :: nsen_info         = 7  ! number of sensor metadata
integer(i_kind), parameter :: ninst_geo         = 1
integer(i_kind), parameter :: ninst             = 17
integer(i_kind), parameter :: write_nc_conv     = 1
integer(i_kind), parameter :: write_nc_radiance = 2
integer(i_kind), parameter :: write_nc_radiance_geo = 3
character(len=3), parameter :: dtime_min = '-3h'
character(len=3), parameter :: dtime_max = '+3h'

! variables for defining observation types and met variables each type has
character(len=nstring), dimension(nobtype) :: obtype_list = &
   (/                 &
      'sondes      ', &
      'aircraft    ', &
      'sfc         ', &
      'satwind     ', &  !AMV winds from prepbufr file
      'satwnd      ', &  !AMV winds from bufr file
      'profiler    ', &
      'ascat       '  &
   /)

character(len=nstring), dimension(nvar_met) :: name_var_met = &
   (/           &
      var_u,    &
      var_v,    &
      var_ts,   &
      var_tv,   &
      var_q,    &
      var_ps    &
   /)

! variable flags for var_u, var_v, var_ts, var_tv, var_q, var_ps
integer(i_kind), dimension(nvar_met,nobtype) :: vflag = reshape ( &
   (/                                               &
      itrue, itrue, itrue,  itrue,  itrue,  ifalse, & ! sonde
      itrue, itrue, itrue,  itrue,  itrue,  ifalse, & ! aircraft
      itrue, itrue, itrue,  itrue,  itrue,  itrue,  & ! sfc
      itrue, itrue, ifalse, ifalse, ifalse, ifalse, & ! satwind
      itrue, itrue, ifalse, ifalse, ifalse, ifalse, & ! satwnd
      itrue, itrue, ifalse, ifalse, ifalse, ifalse, & ! profiler
      itrue, itrue, ifalse, ifalse, ifalse, ifalse  & ! ascat
   /), (/nvar_met,nobtype/) )

character(len=nstring), dimension(nvar_met) :: unit_var_met = &
   (/           &
      'm/s   ', &
      'm/s   ', &
      'K     ', &
      'K     ', &
      'kg/kg ', &
      'Pa    '  &
   /)

! variables for defining radiance instrument types
character(len=nstring), dimension(ninst) :: inst_list = &
   (/                     &
      'amsua_n15       ', &
      'amsua_n18       ', &
      'amsua_n19       ', &
      'amsua_metop-a   ', &
      'amsua_metop-b   ', &
      'amsua_metop-c   ', &
!      'airs_aqua       ', &
      'amsua_aqua      ', &
      'mhs_n18         ', &
      'mhs_n19         ', &
      'mhs_metop-a     ', &
      'mhs_metop-b     ', &
      'mhs_metop-c     ', &
      'iasi_metop-a    ', &
      'iasi_metop-b    ', &
      'iasi_metop-c    ', &
      'cris_npp        ', &
      'cris_n20        '  &
   /)

character(len=nstring), dimension(ninst_geo) :: geoinst_list = &
   (/                     &
      'ahi_himawari8   '  &
   /)
! variables for outputing netcdf files
character(len=nstring), dimension(n_ncdim) :: name_ncdim = &
   (/               &
      'nvars     '  &
    , 'nlocs     '  &
    , 'nstring   '  &
!    , 'ndatetime '  &
   /)
character(len=nstring), dimension(n_ncgrp) :: name_ncgrp = &
   (/               &
      'MetaData  ', &
      'ObsValue  ', &
      'ObsError  ', &
      'PreQC     ', &
      'ObsType   '  &
   /)
character(len=nstring), dimension(nvar_info) :: name_var_info = &
   (/                      &
      'air_pressure     ', &
      'height           ', &
      'station_elevation', &
      'latitude         ', &
      'longitude        ', &
      'dateTime         ', &
      'datetime         ', &
      'station_id       ', &
      'variable_names   '  &
   /)

! conv info flags for name_var_info
! air_pressure, height, station_elevation, latitude, longitude, dateTime, datetime, station_id, variable_names
integer(i_kind), dimension(nvar_info,nobtype) :: iflag_conv = reshape ( &
   (/ &
      itrue, itrue,  itrue,  itrue,  itrue,  itrue,  ifalse,  itrue,  itrue,  & ! sonde
      itrue, itrue,  itrue,  itrue,  itrue,  itrue,  ifalse,  itrue,  itrue,  & ! aircraft
      itrue, itrue,  itrue,  itrue,  itrue,  itrue,  ifalse,  itrue,  itrue,  & ! sfc
      itrue, itrue,  ifalse, itrue,  itrue,  itrue,  ifalse,  itrue,  itrue,  & ! satwind
      itrue, ifalse, ifalse, itrue,  itrue,  itrue,  ifalse,  itrue,  itrue,  & ! satwnd
      itrue, itrue,  itrue,  itrue,  itrue,  itrue,  ifalse,  itrue,  itrue,  & ! profiler
      itrue, ifalse, itrue,  itrue,  itrue,  itrue,  ifalse,  itrue,  itrue   & ! ascat
   /), (/nvar_info,nobtype/) )

! radiance info flags for name_var_info
! air_pressure, height, station_elevation, latitude, longitude, dateTime, datetime, station_id, variable_names
integer(i_kind), dimension(nvar_info) :: iflag_radiance = &
   (/ &
      ifalse, ifalse, ifalse, itrue, itrue, itrue, ifalse, ifalse, ifalse &
   /)

integer(i_kind), dimension(nvar_info) :: type_var_info = &
   (/             &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_int64, &
      nf90_char,  &
      nf90_char,  &
      nf90_char   &
   /)
character(len=nstring), dimension(2,nvar_info) :: dim_var_info = reshape ( &
   (/                             &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'ndatetime ', 'nlocs     ', &
      'nstring   ', 'nlocs     ', &
      'nstring   ', 'nvars     '  &
   /), (/2, nvar_info/) )
character(len=nstring), dimension(nsen_info) :: name_sen_info = &
   (/                         &
      'solar_azimuth_angle ', &
      'scan_position       ', &
      'sensor_azimuth_angle', &
      'solar_zenith_angle  ', &
      'sensor_zenith_angle ', &
      'sensor_view_angle   ', &
      'sensor_channel      '  &
   /)
integer(i_kind), dimension(nsen_info) :: type_sen_info = &
   (/             &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_int    &
   /)
character(len=nstring), dimension(2,nsen_info) :: dim_sen_info = reshape ( &
   (/                             &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nlocs     ', 'null      ', &
      'nvars     ', 'null      '  &
   /), (/2,nsen_info/) )

! variables for storing data
type xfield_type
   real(r_kind)       :: val          ! observation value
   integer(i_kind)    :: qm           ! observation quality marker
   real(r_kind)       :: err          ! observational error
   integer(i_kind)    :: rptype       ! report type
end type xfield_type

type xdata_type
   integer(i_kind)                                     :: nvars
   integer(i_kind)                                     :: nrecs
   integer(i_kind)                                     :: nlocs
   character(len=ndatetime)                            :: min_datetime
   character(len=ndatetime)                            :: max_datetime
   integer(i_kind),        allocatable, dimension(:)   :: var_idx
   type (xfield_type),     allocatable, dimension(:,:) :: xfield
   real(r_kind),           allocatable, dimension(:,:) :: xinfo_float
   integer(i_llong),       allocatable, dimension(:,:) :: xinfo_int
   integer(i_kind),        allocatable, dimension(:,:) :: xinfo_int64
   character(len=nstring), allocatable, dimension(:,:) :: xinfo_char
   real(r_kind),           allocatable, dimension(:,:) :: xseninfo_float
   integer(i_kind),        allocatable, dimension(:,:) :: xseninfo_int
   character(len=nstring), allocatable, dimension(:,:) :: xseninfo_char
end type xdata_type

type(xdata_type), allocatable, dimension(:,:) :: xdata  ! dim 1: number of ob types
                                                        ! dim 2: number of time slots

contains

subroutine set_obtype_conv(t29, obtype)

! https://www.emc.ncep.noaa.gov/BUFRLIB/tables/CodeFlag_0_STDv33_LOC7.html#055008

! assign conventional obtype name based on data dump report type (t29)
! obtype names here should be consistent with those defined in obtype_list

   implicit none

   integer(i_kind),  intent(in)  :: t29
   character(len=*), intent(out) :: obtype

   obtype = 'unknown'

   select case(t29)
      case (11, 12, 13, 22, 23, 31)
         obtype = 'sondes'
         !select case (kx)
         !case (120, 122, 132, 220, 222, 232)
         !   obtype = 'sondes'
         !case (221)
         !   obtype = 'pilot'
         !end select
      case (41)
         ! kx case (130:131, 133, 230:231, 233)
         obtype     = 'aircraft'
      case (522, 523)
         obtype     = 'sfc' ! 'ship'
      case (531, 532, 561, 562, 534)
         obtype     = 'sfc' ! 'buoy'
      case (511, 514, 540) ! mesonet 540
         ! kx case (181, 281)
         obtype     = 'sfc' ! 'synop'
      case (512)
         ! kx case (187, 287)
         obtype     = 'sfc' ! 'metar'
      case (63)
         ! kx case (242:246, 252:253, 255)
         obtype     = 'satwind'
      case (581, 582, 583, 584)
         ! ERS 581, QuikSCAT 582, WindSat 583, ASCAT 584
         obtype     = 'ascat'
      !case (74)
      !   obtype     = 'gpspw'
      !case (71, 73, 75, 76, 77)
      ! t29=73/kx=229 (Wind profiler originating in PIBAL bulletins (tropical and European)
      ! t29=77/kx=126 (Multi-Agency Profiler (MAP) RASS temperatures)
      case (71, 73, 75, 76)
         obtype     = 'profiler'
      !case (571, 65)
      !   obtype = 'ssmir' ! ssmi retrieval
   end select

end subroutine set_obtype_conv

subroutine set_name_satellite(satid, satellite)

! https://www.emc.ncep.noaa.gov/BUFRLIB/tables/CodeFlag_0_STDv33_LOC7.html#001007

! assign satellite name based on BUFR SAID (0-01-007)

   implicit none

   integer(i_kind),  intent(in)  :: satid
   character(len=*), intent(out) :: satellite

   satellite = 'unknown'

   select case ( satid )
      case (   3 ); satellite = 'metop-b'
      case (   4 ); satellite = 'metop-a'
      case (   5 ); satellite = 'metop-c'
      case ( 206 ); satellite = 'n15'
      case ( 207 ); satellite = 'n16'
      case ( 208 ); satellite = 'n17'
      case ( 209 ); satellite = 'n18'
      case ( 223 ); satellite = 'n19'
      case ( 224 ); satellite = 'npp'
      case ( 225 ); satellite = 'n20'
      case ( 226 ); satellite = 'n21'
      case ( 783 ); satellite = 'terra'
      case ( 784 ); satellite = 'aqua'
   end select

end subroutine set_name_satellite

subroutine set_name_sensor(instid, sensor)

! https://www.emc.ncep.noaa.gov/BUFRLIB/tables/CodeFlag_0_STDv33_LOC7.html#002019

! assign sensor name based on BUFR SIID (0-02-019) Satellite instruments

   implicit none

   integer(i_kind),  intent(in)  :: instid
   character(len=*), intent(out) :: sensor

   sensor = 'unknown'

   select case ( instid )
      case ( 570 ); sensor = 'amsua'
      case ( 574 ); sensor = 'amsub'
      case ( 203 ); sensor = 'mhs'
      case ( 617 ); sensor = 'abi'
      case ( 297 ); sensor = 'ahi'
      case ( 207 ); sensor = 'seviri'
      case ( 420 ); sensor = 'airs'
      case ( 620 ); sensor = 'cris'
      case ( 221 ); sensor = 'iasi'
      case ( 389 ); sensor = 'modis'
      case ( 616 ); sensor = 'viirs'
   end select

end subroutine set_name_sensor

subroutine set_brit_obserr(name_inst, nchan, obserrors)

! set brightness temperature observation errors

! For now it is a temporary subroutine to assign observation errors for AMSU-A, MHS, and IASI.
! Values are based on
! https://github.com/comgsi/fix/blob/master/global_satinfo.txt

   implicit none

   character(len=*), intent(in)  :: name_inst  ! instrument name eg. amsua_n15
   integer(i_kind),  intent(in)  :: nchan      ! channel number
   real(r_kind),     intent(out) :: obserrors(nchan)

   obserrors(:) = missing_r

   if ( name_inst(1:5) == 'amsua' ) then
      select case ( trim(name_inst) )
         case ( 'amsua_n15' )
            obserrors = (/ 3.0, 2.2, 2.0, 0.6, 0.3, 0.23, 0.25, 0.275, 0.34, 0.4, 0.6, 1.0, 1.5, 2.0, 3.5 /)
         case ( 'amsua_n18' )
            obserrors = (/ 2.5, 2.2, 2.0, 0.55, 0.3, 0.23, 0.23, 0.25, 0.25, 0.35, 0.4, 0.55, 0.8, 3.0, 3.5 /)
         case ( 'amsua_n19' )
            obserrors = (/ 2.5, 2.2, 2.0, 0.55, 0.3, 0.23, 0.23, 0.25, 0.25, 0.35, 0.4, 0.55, 0.8, 3.0, 3.5 /)
         case ( 'amsua_metop-b' )
            obserrors = (/ 2.5, 2.2, 2.0, 0.55, 0.3, 0.23, 0.23, 0.25, 0.25, 0.35, 0.4, 0.55, 0.8, 3.0, 3.5 /)
         case ( 'amsua_metop-a' )
            obserrors = (/ 2.5, 2.2, 2.0, 0.55, 0.3, 0.23, 0.23, 0.25, 0.25, 0.35, 0.4, 0.55, 0.8, 3.0, 3.5 /)
         case ( 'amsua_metop-c' )
            obserrors = (/ 2.5, 2.2, 2.0, 0.55, 0.3, 0.23, 0.23, 0.25, 0.25, 0.35, 0.4, 0.55, 0.8, 3.0, 3.5 /)
         case ( 'amsua_aqua' )
            obserrors = (/ 2.5, 2.0, 2.0, 0.5, 0.4, 0.4, 0.5, 0.3, 0.35, 0.35, 0.45, 1.0, 1.5, 2.5, 2.5 /)
         case default
            return
      end select
   else if ( name_inst(1:3) == 'mhs' ) then
      select case ( trim(name_inst) )
         case ( 'mhs_n18' )
            obserrors = (/ 2.5, 2.5, 2.5, 2.0, 2.0 /)
         case ( 'mhs_n19' )
            obserrors = (/ 2.5, 2.5, 2.5, 2.0, 2.0 /)
         case ( 'mhs_metop-a' )
            obserrors = (/ 2.5, 2.5, 2.5, 2.0, 2.0 /)
         case ( 'mhs_metop-b' )
            obserrors = (/ 2.5, 2.5, 2.5, 2.0, 2.0 /)
         case ( 'mhs_metop-c' )
            obserrors = (/ 2.5, 2.5, 2.5, 2.0, 2.0 /)
         case default
            return
      end select

   else if ( name_inst(1:4) == 'iasi' ) then
      select case ( trim(name_inst) )
         case ( 'iasi_metop-a' )
            obserrors = (/ 1.38, 0.81, 0.75, 0.79, 0.72, 0.74, 0.68, 0.72, 0.65, 0.65, 0.65, 0.69, 0.64, 0.64, 0.65, 0.67, 0.62, 0.61, 0.62, 0.64, 0.59, 0.76, 1.22, 0.78, 0.64, 0.62, 0.61, 0.69, 0.65, 0.59, 0.61, 0.59, 0.68, 0.62, 0.68, 4.38, 3.05, 2.31, 1.56, 1.33, 1.58, 0.93, 1.67, 0.72, 0.57, 0.58, 0.55, 0.68, 0.59, 0.68, 0.59, 0.65, 0.58, 0.62, 0.64, 0.58, 0.64, 0.55, 0.64, 0.50, 0.82, 0.59, 0.62, 0.51, 0.64, 0.52, 0.51, 0.51, 0.76, 0.52, 0.57, 0.55, 0.69, 0.58, 0.65, 0.61, 0.59, 0.64, 0.76, 0.72, 1.05, 0.75, 0.51, 0.65, 1.30, 0.69, 0.93, 1.49, 1.12, 0.68, 0.66, 0.67, 0.59, 0.59, 0.69, 0.67, 0.64, 0.62, 0.72, 0.69, 0.66, 0.79, 0.78, 0.74, 0.88, 0.77, 0.88, 0.86, 1.00, 0.87, 0.85, 0.88, 0.84, 0.84, 0.84, 0.80, 0.80, 0.87, 0.98, 0.52, 0.65, 0.69, 0.61, 0.60, 0.67, 0.79, 0.62, 0.66, 0.70, 0.65, 0.62, 0.61, 0.62, 0.53, 0.60, 0.68, 0.95, 0.63, 0.97, 0.65, 0.98, 0.58, 0.73, 0.65, 0.85, 0.99, 0.76, 0.85, 0.97, 0.77, 0.62, 0.63, 1.21, 1.41, 1.55, 1.78, 1.35, 1.14, 1.69, 1.79, 1.46, 1.63, 1.94, 2.01, 1.24, 1.76, 1.26, 1.47, 1.90, 1.66, 2.13, 1.49, 1.52, 1.55, 1.96, 2.31, 2.33, 2.32, 2.31, 2.33, 2.23, 2.33, 1.84, 2.29, 2.28, 2.28, 2.28, 2.26, 2.26, 2.26, 2.27, 2.24, 2.23, 2.24, 2.26, 2.28, 2.28, 2.30, 2.15, 2.31, 2.37, 2.27, 2.29, 2.29, 2.23, 2.28, 2.32, 2.32, 2.31, 2.32, 2.32, 2.31, 2.31, 2.28, 2.29, 2.28, 2.26, 2.29, 2.27, 2.26, 2.25, 2.27, 2.24, 2.21, 2.24, 2.17, 2.18, 2.17, 2.21, 1.99, 2.16, 2.20, 2.13, 2.12, 2.13, 2.10, 2.12, 2.11, 2.09, 2.09, 2.08, 2.09, 2.04, 2.04, 2.10, 2.01, 2.05, 2.03, 2.06, 1.98, 1.95, 1.94, 1.91, 1.70, 1.76, 1.77, 1.83, 2.04, 1.91, 1.99, 1.99, 2.07, 2.02, 2.04, 2.10, 2.06, 2.18, 2.21, 2.24, 2.23, 2.23, 1.98, 2.20, 2.18, 2.18, 2.21, 2.23, 2.24, 2.24, 2.25, 1.80, 2.24, 1.73, 1.73, 2.27, 1.67, 2.21, 1.72, 2.23, 2.23, 2.23, 2.24, 2.23, 2.12, 2.17, 1.74, 2.02, 1.88, 1.67, 1.73, 1.83, 1.82, 1.73, 1.83, 2.19, 1.84, 1.89, 1.60, 1.71, 1.86, 1.85, 1.84, 1.87, 1.91, 1.52, 1.95, 1.87, 1.89, 1.91, 1.91, 1.93, 1.90, 1.91, 1.90, 1.89, 1.89, 1.91, 1.90, 1.91, 1.91, 1.91, 1.93, 1.94, 1.91, 1.92, 1.77, 1.91, 1.95, 1.19, 1.96, 1.98, 1.94, 1.55, 1.91, 1.92, 1.92, 1.97, 1.93, 1.99, 1.86, 1.12, 1.93, 1.92, 1.95, 1.85, 1.84, 1.91, 1.12, 1.82, 1.82, 1.95, 1.24, 1.94, 1.96, 1.21, 1.83, 1.96, 1.36, 1.96, 1.82, 1.92, 1.68, 1.93, 1.23, 1.96, 1.93, 1.86, 1.41, 1.16, 1.60, 1.25, 1.20, 1.65, 1.66, 1.87, 1.94, 1.96, 1.91, 1.25, 1.93, 1.91, 1.70, 0.99, 1.81, 1.92, 1.95, 1.50, 1.47, 1.15, 1.58, 1.18, 1.82, 1.13, 1.83, 1.91, 1.26, 1.27, 1.91, 1.45, 1.60, 1.29, 1.94, 1.94, 1.23, 1.95, 1.21, 1.94, 1.86, 1.90, 1.33, 1.75, 2.02, 1.98, 2.03, 1.83, 1.50, 2.04, 2.02, 1.90, 2.00, 2.02, 1.95, 1.93, 1.95, 1.95, 1.99, 2.00, 1.94, 1.96, 1.86, 1.92, 1.88, 1.86, 1.84, 1.87, 1.77, 1.89, 1.89, 1.88, 1.94, 1.82, 1.79, 1.86, 2.06, 2.33, 1.88, 1.86, 1.81, 1.80, 1.80, 1.86, 1.90, 2.00, 2.06, 2.10, 2.20, 2.00, 2.16, 1.98, 1.80, 1.80, 1.85, 1.75, 2.04, 2.19, 2.14, 2.19, 1.86, 2.10, 2.11, 2.18, 2.03, 2.28, 2.19, 2.26, 2.26, 2.21, 2.21, 2.26, 2.33, 2.27, 2.21, 2.12, 2.23, 2.26, 2.25, 1.88, 2.26, 2.24, 2.36, 2.29, 2.35, 2.30, 2.27, 2.08, 2.05, 2.27, 2.28, 2.27, 2.28, 1.97, 2.25, 2.25, 2.25, 2.31, 2.28, 2.27, 2.13, 2.24, 2.28, 2.28, 2.41, 2.34, 9.32, 2.28, 2.38, 2.27, 2.27, 2.39, 2.11, 2.09, 2.10, 2.06, 2.12, 2.08, 2.00, 1.93, 2.02, 2.55, 1.54, 1.64, 1.51, 1.55, 2.82, 2.92, 2.55, 2.37, 1.85, 1.60, 1.72, 1.74, 1.79, 1.90, 1.94, 2.00, 2.04, 2.08, 2.12, 2.13, 2.16, 2.18, 2.18, 2.20, 2.20, 2.41, 2.39, 2.38, 2.40, 2.42, 2.41, 2.43, 2.45, 2.43, 2.45, 2.43, 2.40, 2.44, 2.40, 2.42, 2.43, 2.45, 2.45, 2.45, 2.46, 2.45, 2.45, 2.43, 2.51, 2.48, 2.48, 2.53, 2.46, 2.49, 2.50, 2.50, 2.50, 2.52, 2.52, 2.54, 2.50, 2.48, 2.50, 2.55, 2.50, 2.48, 2.50, 2.50, 2.52, 2.52, 2.48, 2.50, 2.50, 2.52, 2.46, 2.53, 9.00 /)
         case ( 'iasi_metop-b' )
            obserrors = (/ 1.38, 0.81, 0.75, 0.79, 0.72, 0.74, 0.68, 0.72, 0.65, 0.65, 0.65, 0.69, 0.64, 0.64, 0.65, 0.67, 0.62, 0.61, 0.62, 0.64, 0.59, 0.76, 1.22, 0.78, 0.64, 0.62, 0.61, 0.69, 0.65, 0.59, 0.61, 0.59, 0.68, 0.62, 0.68, 4.38, 3.05, 2.31, 1.56, 1.33, 1.58, 0.93, 1.67, 0.72, 0.57, 0.58, 0.55, 0.68, 0.59, 0.68, 0.59, 0.65, 0.58, 0.62, 0.64, 0.58, 0.64, 0.55, 0.64, 0.50, 0.82, 0.59, 0.62, 0.51, 0.64, 0.52, 0.51, 0.51, 0.76, 0.52, 0.57, 0.55, 0.69, 0.58, 0.65, 0.61, 0.59, 0.64, 0.76, 0.72, 1.05, 0.75, 0.51, 0.65, 1.30, 0.69, 0.93, 1.49, 1.12, 0.68, 0.66, 0.67, 0.59, 0.59, 0.69, 0.67, 0.64, 0.62, 0.72, 0.69, 0.66, 0.79, 0.78, 0.74, 0.88, 0.77, 0.88, 0.86, 1.00, 0.87, 0.85, 0.88, 0.84, 0.84, 0.84, 0.80, 0.80, 0.87, 0.98, 0.52, 0.65, 0.69, 0.61, 0.60, 0.67, 0.79, 0.62, 0.66, 0.70, 0.65, 0.62, 0.61, 0.62, 0.53, 0.60, 0.68, 0.95, 0.63, 0.97, 0.65, 0.98, 0.58, 0.73, 0.65, 0.85, 0.99, 0.76, 0.85, 0.97, 0.77, 0.62, 0.63, 1.21, 1.41, 1.55, 1.78, 1.35, 1.14, 1.69, 1.79, 1.46, 1.63, 1.94, 2.01, 1.24, 1.76, 1.26, 1.47, 1.90, 1.66, 2.13, 1.49, 1.52, 1.55, 1.96, 2.31, 2.33, 2.32, 2.31, 2.33, 2.23, 2.33, 1.84, 2.29, 2.28, 2.28, 2.28, 2.26, 2.26, 2.26, 2.27, 2.24, 2.23, 2.24, 2.26, 2.28, 2.28, 2.30, 2.15, 2.31, 2.37, 2.27, 2.29, 2.29, 2.23, 2.28, 2.32, 2.32, 2.31, 2.32, 2.32, 2.31, 2.31, 2.28, 2.29, 2.28, 2.26, 2.29, 2.27, 2.26, 2.25, 2.27, 2.24, 2.21, 2.24, 2.17, 2.18, 2.17, 2.21, 1.99, 2.16, 2.20, 2.13, 2.12, 2.13, 2.10, 2.12, 2.11, 2.09, 2.09, 2.08, 2.09, 2.04, 2.04, 2.10, 2.01, 2.05, 2.03, 2.06, 1.98, 1.95, 1.94, 1.91, 1.70, 1.76, 1.77, 1.83, 2.04, 1.91, 1.99, 1.99, 2.07, 2.02, 2.04, 2.10, 2.06, 2.18, 2.21, 2.24, 2.23, 2.23, 1.98, 2.20, 2.18, 2.18, 2.21, 2.23, 2.24, 2.24, 2.25, 1.80, 2.24, 1.73, 1.73, 2.27, 1.67, 2.21, 1.72, 2.23, 2.23, 2.23, 2.24, 2.23, 2.12, 2.17, 1.74, 2.02, 1.88, 1.67, 1.73, 1.83, 1.82, 1.73, 1.83, 2.19, 1.84, 1.89, 1.60, 1.71, 1.86, 1.85, 1.84, 1.87, 1.91, 1.52, 1.95, 1.87, 1.89, 1.91, 1.91, 1.93, 1.90, 1.91, 1.90, 1.89, 1.89, 1.91, 1.90, 1.91, 1.91, 1.91, 1.93, 1.94, 1.91, 1.92, 1.77, 1.91, 1.95, 1.19, 1.96, 1.98, 1.94, 1.55, 1.91, 1.92, 1.92, 1.97, 1.93, 1.99, 1.86, 1.12, 1.93, 1.92, 1.95, 1.85, 1.84, 1.91, 1.12, 1.82, 1.82, 1.95, 1.24, 1.94, 1.96, 1.21, 1.83, 1.96, 1.36, 1.96, 1.82, 1.92, 1.68, 1.93, 1.23, 1.96, 1.93, 1.86, 1.41, 1.16, 1.60, 1.25, 1.20, 1.65, 1.66, 1.87, 1.94, 1.96, 1.91, 1.25, 1.93, 1.91, 1.70, 0.99, 1.81, 1.92, 1.95, 1.50, 1.47, 1.15, 1.58, 1.18, 1.82, 1.13, 1.83, 1.91, 1.26, 1.27, 1.91, 1.45, 1.60, 1.29, 1.94, 1.94, 1.23, 1.95, 1.21, 1.94, 1.86, 1.90, 1.33, 1.75, 2.02, 1.98, 2.03, 1.83, 1.50, 2.04, 2.02, 1.90, 2.00, 2.02, 1.95, 1.93, 1.95, 1.95, 1.99, 2.00, 1.94, 1.96, 1.86, 1.92, 1.88, 1.86, 1.84, 1.87, 1.77, 1.89, 1.89, 1.88, 1.94, 1.82, 1.79, 1.86, 2.06, 2.33, 1.88, 1.86, 1.81, 1.80, 1.80, 1.86, 1.90, 2.00, 2.06, 2.10, 2.20, 2.00, 2.16, 1.98, 1.80, 1.80, 1.85, 1.75, 2.04, 2.19, 2.14, 2.19, 1.86, 2.10, 2.11, 2.18, 2.03, 2.28, 2.19, 2.26, 2.26, 2.21, 2.21, 2.26, 2.33, 2.27, 2.21, 2.12, 2.23, 2.26, 2.25, 1.88, 2.26, 2.24, 2.36, 2.29, 2.35, 2.30, 2.27, 2.08, 2.05, 2.27, 2.28, 2.27, 2.28, 1.97, 2.25, 2.25, 2.25, 2.31, 2.28, 2.27, 2.13, 2.24, 2.28, 2.28, 2.41, 2.34, 9.32, 2.28, 2.38, 2.27, 2.27, 2.39, 2.11, 2.09, 2.10, 2.06, 2.12, 2.08, 2.00, 1.93, 2.02, 2.55, 1.54, 1.64, 1.51, 1.55, 2.82, 2.92, 2.55, 2.37, 1.85, 1.60, 1.72, 1.74, 1.79, 1.90, 1.94, 2.00, 2.04, 2.08, 2.12, 2.13, 2.16, 2.18, 2.18, 2.20, 2.20, 2.41, 2.39, 2.38, 2.40, 2.42, 2.41, 2.43, 2.45, 2.43, 2.45, 2.43, 2.40, 2.44, 2.40, 2.42, 2.43, 2.45, 2.45, 2.45, 2.46, 2.45, 2.45, 2.43, 2.51, 2.48, 2.48, 2.53, 2.46, 2.49, 2.50, 2.50, 2.50, 2.52, 2.52, 2.54, 2.50, 2.48, 2.50, 2.55, 2.50, 2.48, 2.50, 2.50, 2.52, 2.52, 2.48, 2.50, 2.50, 2.52, 2.46, 2.53, 9.00 /)
         case ( 'iasi_metop-c' )
            obserrors = (/ 1.38, 0.81, 0.75, 0.79, 0.72, 0.74, 0.68, 0.72, 0.65, 0.65, 0.65, 0.69, 0.64, 0.64, 0.65, 0.67, 0.62, 0.61, 0.62, 0.64, 0.59, 0.76, 1.22, 0.78, 0.64, 0.62, 0.61, 0.69, 0.65, 0.59, 0.61, 0.59, 0.68, 0.62, 0.68, 4.38, 3.05, 2.31, 1.56, 1.33, 1.58, 0.93, 1.67, 0.72, 0.57, 0.58, 0.55, 0.68, 0.59, 0.68, 0.59, 0.65, 0.58, 0.62, 0.64, 0.58, 0.64, 0.55, 0.64, 0.50, 0.82, 0.59, 0.62, 0.51, 0.64, 0.52, 0.51, 0.51, 0.76, 0.52, 0.57, 0.55, 0.69, 0.58, 0.65, 0.61, 0.59, 0.64, 0.76, 0.72, 1.05, 0.75, 0.51, 0.65, 1.30, 0.69, 0.93, 1.49, 1.12, 0.68, 0.66, 0.67, 0.59, 0.59, 0.69, 0.67, 0.64, 0.62, 0.72, 0.69, 0.66, 0.79, 0.78, 0.74, 0.88, 0.77, 0.88, 0.86, 1.00, 0.87, 0.85, 0.88, 0.84, 0.84, 0.84, 0.80, 0.80, 0.87, 0.98, 0.52, 0.65, 0.69, 0.61, 0.60, 0.67, 0.79, 0.62, 0.66, 0.70, 0.65, 0.62, 0.61, 0.62, 0.53, 0.60, 0.68, 0.95, 0.63, 0.97, 0.65, 0.98, 0.58, 0.73, 0.65, 0.85, 0.99, 0.76, 0.85, 0.97, 0.77, 0.62, 0.63, 1.21, 1.41, 1.55, 1.78, 1.35, 1.14, 1.69, 1.79, 1.46, 1.63, 1.94, 2.01, 1.24, 1.76, 1.26, 1.47, 1.90, 1.66, 2.13, 1.49, 1.52, 1.55, 1.96, 2.31, 2.33, 2.32, 2.31, 2.33, 2.23, 2.33, 1.84, 2.29, 2.28, 2.28, 2.28, 2.26, 2.26, 2.26, 2.27, 2.24, 2.23, 2.24, 2.26, 2.28, 2.28, 2.30, 2.15, 2.31, 2.37, 2.27, 2.29, 2.29, 2.23, 2.28, 2.32, 2.32, 2.31, 2.32, 2.32, 2.31, 2.31, 2.28, 2.29, 2.28, 2.26, 2.29, 2.27, 2.26, 2.25, 2.27, 2.24, 2.21, 2.24, 2.17, 2.18, 2.17, 2.21, 1.99, 2.16, 2.20, 2.13, 2.12, 2.13, 2.10, 2.12, 2.11, 2.09, 2.09, 2.08, 2.09, 2.04, 2.04, 2.10, 2.01, 2.05, 2.03, 2.06, 1.98, 1.95, 1.94, 1.91, 1.70, 1.76, 1.77, 1.83, 2.04, 1.91, 1.99, 1.99, 2.07, 2.02, 2.04, 2.10, 2.06, 2.18, 2.21, 2.24, 2.23, 2.23, 1.98, 2.20, 2.18, 2.18, 2.21, 2.23, 2.24, 2.24, 2.25, 1.80, 2.24, 1.73, 1.73, 2.27, 1.67, 2.21, 1.72, 2.23, 2.23, 2.23, 2.24, 2.23, 2.12, 2.17, 1.74, 2.02, 1.88, 1.67, 1.73, 1.83, 1.82, 1.73, 1.83, 2.19, 1.84, 1.89, 1.60, 1.71, 1.86, 1.85, 1.84, 1.87, 1.91, 1.52, 1.95, 1.87, 1.89, 1.91, 1.91, 1.93, 1.90, 1.91, 1.90, 1.89, 1.89, 1.91, 1.90, 1.91, 1.91, 1.91, 1.93, 1.94, 1.91, 1.92, 1.77, 1.91, 1.95, 1.19, 1.96, 1.98, 1.94, 1.55, 1.91, 1.92, 1.92, 1.97, 1.93, 1.99, 1.86, 1.12, 1.93, 1.92, 1.95, 1.85, 1.84, 1.91, 1.12, 1.82, 1.82, 1.95, 1.24, 1.94, 1.96, 1.21, 1.83, 1.96, 1.36, 1.96, 1.82, 1.92, 1.68, 1.93, 1.23, 1.96, 1.93, 1.86, 1.41, 1.16, 1.60, 1.25, 1.20, 1.65, 1.66, 1.87, 1.94, 1.96, 1.91, 1.25, 1.93, 1.91, 1.70, 0.99, 1.81, 1.92, 1.95, 1.50, 1.47, 1.15, 1.58, 1.18, 1.82, 1.13, 1.83, 1.91, 1.26, 1.27, 1.91, 1.45, 1.60, 1.29, 1.94, 1.94, 1.23, 1.95, 1.21, 1.94, 1.86, 1.90, 1.33, 1.75, 2.02, 1.98, 2.03, 1.83, 1.50, 2.04, 2.02, 1.90, 2.00, 2.02, 1.95, 1.93, 1.95, 1.95, 1.99, 2.00, 1.94, 1.96, 1.86, 1.92, 1.88, 1.86, 1.84, 1.87, 1.77, 1.89, 1.89, 1.88, 1.94, 1.82, 1.79, 1.86, 2.06, 2.33, 1.88, 1.86, 1.81, 1.80, 1.80, 1.86, 1.90, 2.00, 2.06, 2.10, 2.20, 2.00, 2.16, 1.98, 1.80, 1.80, 1.85, 1.75, 2.04, 2.19, 2.14, 2.19, 1.86, 2.10, 2.11, 2.18, 2.03, 2.28, 2.19, 2.26, 2.26, 2.21, 2.21, 2.26, 2.33, 2.27, 2.21, 2.12, 2.23, 2.26, 2.25, 1.88, 2.26, 2.24, 2.36, 2.29, 2.35, 2.30, 2.27, 2.08, 2.05, 2.27, 2.28, 2.27, 2.28, 1.97, 2.25, 2.25, 2.25, 2.31, 2.28, 2.27, 2.13, 2.24, 2.28, 2.28, 2.41, 2.34, 9.32, 2.28, 2.38, 2.27, 2.27, 2.39, 2.11, 2.09, 2.10, 2.06, 2.12, 2.08, 2.00, 1.93, 2.02, 2.55, 1.54, 1.64, 1.51, 1.55, 2.82, 2.92, 2.55, 2.37, 1.85, 1.60, 1.72, 1.74, 1.79, 1.90, 1.94, 2.00, 2.04, 2.08, 2.12, 2.13, 2.16, 2.18, 2.18, 2.20, 2.20, 2.41, 2.39, 2.38, 2.40, 2.42, 2.41, 2.43, 2.45, 2.43, 2.45, 2.43, 2.40, 2.44, 2.40, 2.42, 2.43, 2.45, 2.45, 2.45, 2.46, 2.45, 2.45, 2.43, 2.51, 2.48, 2.48, 2.53, 2.46, 2.49, 2.50, 2.50, 2.50, 2.52, 2.52, 2.54, 2.50, 2.48, 2.50, 2.55, 2.50, 2.48, 2.50, 2.50, 2.52, 2.52, 2.48, 2.50, 2.50, 2.52, 2.46, 2.53, 9.00 /)
         case default
            return
      end select
   else
      return
   end if

end subroutine set_brit_obserr

end module define_mod
