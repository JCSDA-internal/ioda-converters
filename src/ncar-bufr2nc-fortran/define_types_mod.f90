module define_types_mod

use kinds, only: r_kind, i_kind
use ufo_vars_mod, only: var_ps, var_prs, var_u, var_v, var_ts, var_tv, var_q, var_tb
use netcdf, only: nf90_float, nf90_int, nf90_char

implicit none

real(r_kind),    parameter :: t_kelvin          = 273.15
real(r_kind),    parameter :: missing_r         = -999.0
integer(i_kind), parameter :: missing_i         = -999
integer(i_kind), parameter :: itrue             = 1
integer(i_kind), parameter :: ifalse            = 0
integer(i_kind), parameter :: nstring           = 50
integer(i_kind), parameter :: ndatetime         = 20
integer(i_kind), parameter :: nobtype           = 6  ! number of ob types
integer(i_kind), parameter :: n_ncdim           = 5  ! total numner of nc dimensions
integer(i_kind), parameter :: nvar_met          = 7
integer(i_kind), parameter :: nvar_info         = 8  ! number of metadata
integer(i_kind), parameter :: nsen_info         = 7  ! number of sensor metadata
integer(i_kind), parameter :: ninst             = 6
integer(i_kind), parameter :: write_nc_conv     = 1
integer(i_kind), parameter :: write_nc_radiance = 2

! variables for defining observation types and met variables each type has
character(len=nstring), dimension(nobtype) :: obtype_list = &
   (/                 &
      'sondes      ', &
      'aircraft    ', &
      'sfc         ', &
      'satwind     ', &
      'profiler    ', &
      'ascat       '  &
   /)

character(len=nstring), dimension(nvar_met) :: name_var_met = &
   (/           &
      var_prs,  &
      var_u,    &
      var_v,    &
      var_ts,   &
      var_tv,   &
      var_q,    &
      var_ps    &
   /)

! variable flags for var_prs, var_u, var_v, var_ts, var_tv, var_q, var_ps
integer(i_kind), dimension(nvar_met,nobtype) :: vflag = reshape ( &
   (/                                              &
      itrue, itrue, itrue, itrue,  itrue,  itrue,  ifalse, & ! sonde
      itrue, itrue, itrue, itrue,  ifalse, itrue,  ifalse, & ! aircraft
      itrue, itrue, itrue, itrue,  ifalse, itrue,  itrue,  & ! sfc
      itrue, itrue, itrue, ifalse, ifalse, ifalse, ifalse, & ! satwnd
      itrue, itrue, itrue, ifalse, ifalse, ifalse, ifalse, & ! profiler
      itrue, itrue, itrue, ifalse, ifalse, ifalse, ifalse  & ! ascat
   /), (/nvar_met,nobtype/) )

! variables for defining radiance instrument types
character(len=nstring), dimension(ninst) :: inst_list = &
   (/                     &
      'amsua_n15       ', &
      'amsua_n18       ', &
      'amsua_n19       ', &
      'amsua_metop-a   ', &
      'amsua_metop-b   ', &
      'amsua_aqua      ' /)  ! to do

! variables for outputing netcdf files
character(len=nstring), dimension(n_ncdim) :: name_ncdim = &
   (/               &
      'nvars     ', &
      'nlocs     ', &
      'nrec      ', &
      'nstring   ', &
      'ndatetime '  &
   /)
character(len=nstring), dimension(nvar_info) :: name_var_info = &
   (/ 'time             ', &
      'station_elevation', &
      'latitude         ', &
      'longitude        ', &
      'record_number    ', &
      'datetime         ', &
      'station_id       ', &
      'variable_names   '  &
   /)
integer(i_kind), dimension(nvar_info) :: type_var_info = &
   (/             &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_float, &
      nf90_int,   &
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
      'sensor_view_angle   ', &  ! to be derived from fov
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
type field_type
   real(r_kind)       :: val          ! observation value
   integer(i_kind)    :: qc           ! observation QC
   real(r_kind)       :: err          ! observational error
   integer(i_kind)    :: rptype       ! report type
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

type xdata_type
   integer(i_kind)                                     :: nvars
   integer(i_kind)                                     :: nrecs
   integer(i_kind)                                     :: nlocs
   integer(i_kind),        allocatable, dimension(:)   :: var_idx
   type (field_type),      allocatable, dimension(:,:) :: xfield
   real(r_kind),           allocatable, dimension(:,:) :: xinfo_float
   integer(i_kind),        allocatable, dimension(:,:) :: xinfo_int
   character(len=nstring), allocatable, dimension(:,:) :: xinfo_char
   real(r_kind),           allocatable, dimension(:,:) :: xseninfo_float
   integer(i_kind),        allocatable, dimension(:,:) :: xseninfo_int
   character(len=nstring), allocatable, dimension(:,:) :: xseninfo_char
end type xdata_type

type(xdata_type), allocatable, dimension(:) :: xdata  ! dim: number of ob types

type datalink_radiance

   character(len=nstring)    :: msg_type   ! BUFR message type name
   integer(i_kind)           :: satid      ! satellite identifier
   integer(i_kind)           :: instid     ! instrument identifier
   character(len=nstring)    :: inst       ! instrument name eg. amsua-n15
   character(len=ndatetime)  :: datetime   ! ccyy-mm-ddThh:mm:ssZ
   integer(i_kind)           :: nchan      ! number of channels
   real(r_kind)              :: lat        ! latitude in degree
   real(r_kind)              :: lon        ! longitude in degree
   real(r_kind)              :: elv        ! elevation in m
   real(r_kind)              :: dhr        ! obs time minus analysis time in hour
   integer(i_kind)           :: inst_idx   ! index of inst in inst_list
   integer(i_kind)           :: landsea
   integer(i_kind)           :: scanpos
   real(r_kind)              :: satzen
   real(r_kind)              :: satazi
   real(r_kind)              :: solzen
   real(r_kind)              :: solazi
   real(r_kind), allocatable :: tb(:)
   real(r_kind), allocatable :: ch(:)
   type (datalink_radiance), pointer :: next ! pointer to next data
end type datalink_radiance

type(datalink_radiance), pointer :: rhead=>null(), rlink=>null()

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
      case (531, 532, 561, 562)
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
      case (71, 73, 75, 76, 77)
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
      case ( 204 ); sensor = 'mhs'
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

end module define_types_mod
