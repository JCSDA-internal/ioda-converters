module define_types_mod

use kinds, only: r_kind, i_kind
use ufo_vars_mod, only: var_ps, var_prs, var_u, var_v, var_ts, var_q

implicit none

real(r_kind),    parameter :: t_kelvin         = 273.15
real(r_kind),    parameter :: missing_r        = -999.0
integer(i_kind), parameter :: missing_i        = -999
integer(i_kind), parameter :: itrue            = 1
integer(i_kind), parameter :: ifalse           = 0
integer(i_kind), parameter :: nstring          = 50
integer(i_kind), parameter :: ndatetime        = 20
integer(i_kind), parameter :: nobtype          = 6  ! number of ob types
integer(i_kind), parameter :: n_ncdim          = 5  ! total numner of nc dimensions
integer(i_kind), parameter :: nvar_met         = 6
integer(i_kind), parameter :: nvar_info_float  = 5
integer(i_kind), parameter :: nvar_info_int    = 1
integer(i_kind), parameter :: nvar_info_char20 = 1
integer(i_kind), parameter :: nvar_info_char50 = 1

! variables for defining observation types and met variables each type has
character (len=12) :: obtype_list(nobtype) = (/ &
                         'sondes      ',        &
                         'aircraft    ',        &
                         'sfc         ',        &
                         'satwind     ',        &
                         'profiler    ',        &
                         'ascat       ' /)

character(len=100), dimension(nvar_met) :: name_var_met = &
                                           (/ var_prs, var_u, var_v, var_ts, var_q, var_ps /)

! variable flags for var_prs, var_u, var_v, var_ts, var_q, var_ps
integer(i_kind) :: vflag(nvar_met,nobtype) = reshape (              &
                     (/                                             &
                       itrue, itrue, itrue, itrue,  itrue,  ifalse, & ! sonde
                       itrue, itrue, itrue, itrue,  itrue,  ifalse, & ! aircraft
                       itrue, itrue, itrue, itrue,  itrue,  itrue,  & ! sfc
                       itrue, itrue, itrue, ifalse, ifalse, ifalse, & ! satwnd
                       itrue, itrue, itrue, ifalse, ifalse, ifalse, & ! profiler
                       itrue, itrue, itrue, ifalse, ifalse, ifalse  & ! ascat
                     /), (/nvar_met,nobtype/) )

! variables for outputing netcdf files
character(len=10), dimension(n_ncdim) :: name_ncdim =     &
                                         (/ 'nvars     ', &
                                            'nlocs     ', &
                                            'nrec      ', &
                                            'nstring   ', &
                                            'ndatetime ' /)
character(len=100), dimension(nvar_info_float) :: name_var_info_float =     &
                                                  (/ 'time              ', &
                                                     'station_elevation ', &
                                                     'latitude          ', &
                                                     'longitude         ', &
                                                     'height            '  &
                                                   /)
character(len=100), dimension(nvar_info_int) :: name_var_info_int =         &
                                                   (/ 'record_number     ' /)
character(len=100), dimension(nvar_info_char20) :: name_var_info_char20 =   &
                                                   (/ 'datetime          ' /)
character(len=100), dimension(nvar_info_char50) :: name_var_info_char50 =   &
                                                   (/ 'station_id        ' /)

! variables for storing data
type field_type
   real(r_kind)       :: val          ! observation value
   integer(i_kind)    :: qc           ! observation QC
   real(r_kind)       :: err          ! observational error
   integer(i_kind)    :: rptype       ! report type
end type field_type

type info_type
   character (len=40) :: name         ! station name
   character (len=40) :: id           ! 5 digit station identifer
   character (len=20) :: datetime     ! ccyy-mm-ddThh:mm:ssZ
   integer(i_kind)    :: nlevels      ! number of levels
   real(r_kind)       :: lat          ! latitude in degree
   real(r_kind)       :: lon          ! longitude in degree
   real(r_kind)       :: elv          ! elevation in m
   real(r_kind)       :: dhr          ! obs time minus analysis time in hour
end type info_type

type each_level_type
   type (field_type)  :: h            ! height in m
   type (field_type)  :: u            ! Wind x-component in m/s
   type (field_type)  :: v            ! Wind y-component in m/s
   type (field_type)  :: p            ! Pressure in Pa
   type (field_type)  :: t            ! Temperature in K
   type (field_type)  :: q            ! (kg/kg)
   real(r_kind)       :: lat          ! Latitude in degree
   real(r_kind)       :: lon          ! Longitude in degree
end type each_level_type

type multi_level_type
   type (info_type)                    :: info
   !type (field_type)                   :: ps         ! surface pressure
   type (field_type)                   :: slp        ! sea level pressure
   type (field_type)                   :: pw         ! precipitable water
   type (each_level_type), allocatable :: each(:)
end type multi_level_type

type datalink_BUFR
    type (multi_level_type) :: platform
    integer(i_kind)         :: nlevels
    integer(i_kind)         :: t29        ! data dump report type
    integer(i_kind)         :: rptype     ! prepbufr report type
    character(len=12)       :: obtype     ! ob type, eg sonde, satwnd
    integer(i_kind)         :: obtype_idx ! index of obtype in obtype_list
    type(datalink_BUFR), pointer :: next
end type datalink_BUFR

type(datalink_BUFR), pointer :: head=>null(), plink=>null()

type xdata_type
   integer(i_kind)                                :: nrecs
   integer(i_kind)                                :: nlocs
   integer(i_kind),   allocatable, dimension(:)   :: var_idx
   type (field_type), allocatable, dimension(:,:) :: xfield
   real(r_kind),      allocatable, dimension(:,:) :: xinfo_float
   integer(i_kind),   allocatable, dimension(:,:) :: xinfo_int
   character(len=20), allocatable, dimension(:,:) :: xinfo_char20
   character(len=50), allocatable, dimension(:,:) :: xinfo_char50
end type xdata_type

type(xdata_type), allocatable, dimension(:) :: xdata  ! dim nobtype

contains

subroutine set_obtype(t29, obtype)

! assign obtype name based on data dump report type (t29)
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

end subroutine set_obtype

end module define_types_mod
