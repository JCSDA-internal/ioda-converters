module ahi_HSD_mod

!Himawari Standard Data
!HS_H08_20210119_1600_B09_FLDK_R20_S0510.DAT

!MSC Website
!Sample data
!https://www.data.jma.go.jp/mscweb/en/himawari89/space_segment/spsg_sample.html
!Sample source code in C Programming Language
!https://www.data.jma.go.jp/mscweb/en/himawari89/space_segment/hsd_sample/sample_code_netcdf121.zip

!Himawari Standard Data User's Guide
!https://www.data.jma.go.jp/mscweb/en/himawari89/space_segment/hsd_sample/HS_D_users_guide_en_v13.pdf

!https://www.jstage.jst.go.jp/article/jmsj/94/2/94_2016-009/_pdf/-char/en

use kinds, only: i_byte, i_short, i_long, i_llong, i_kind, r_single, r_double, r_kind
use define_mod, only: missing_r, missing_i, nstring, ndatetime, &
   ninst, inst_list, set_name_satellite, set_name_sensor, xdata, name_sen_info, &
   nvar_info, name_var_info, type_var_info, nsen_info, type_sen_info, set_brit_obserr, strlen
use ufo_vars_mod, only: ufo_vars_getindex
use netcdf, only: nf90_float, nf90_int, nf90_char, nf90_int64
use utils_mod, only: get_julian_time

implicit none

integer(i_kind) :: mmday(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
character(len=ndatetime)  :: datetime   ! ccyy-mm-ddThh:mm:ssZ
real(r_double)   :: gstime
integer(i_llong) :: epochtime
integer(i_kind)  :: iyear, imonth, iday, ihour, imin, isec

integer(i_kind) :: subsample

integer(i_kind), parameter :: npixel = 5500
integer(i_kind), parameter :: nline  = 5500
integer(i_kind), parameter :: nband = 10  ! number of infrared bands
integer(i_kind), parameter :: nsegm = 10  ! number of segment

real(r_single)  :: longitude(npixel, nline)
real(r_single)  :: latitude(npixel, nline)
real(r_single)  :: brit(npixel, nline, nband)
real(r_single)  :: solzen(npixel, nline)
real(r_single)  :: satzen(npixel, nline)
logical         :: valid(npixel, nline)
real(r_double)  :: rlat, rlon, lon_diff, tmp1, theta1, theta2
integer(i_kind) :: ntotal, npix, nlin

integer(i_kind) :: nfile
integer(i_kind) :: ifile
character(len=StrLen), allocatable :: fnames(:)
logical, allocatable :: fexist(:)

type basic_info
  integer(i_byte)    :: headerNum      ! header block number = 1
  integer(i_short)   :: blockLen       ! block length = 282 bytes
  integer(i_short)   :: numHeader      ! total number of header blocks = 11
  integer(i_byte)    :: byteOrder      ! 0: little_endian, 1: big endian
  character(len=16)  :: satName        ! 'Himawari-8'
  character(len=16)  :: procCenter     ! 'MSC', 'OSK'
  character(len=4)   :: obsArea        ! 'FLDK'
  character(len=2)   :: dummy2
  integer(i_short)   :: hhnn           ! observation timeline
  real(r_double)     :: obsStartTime   ! Modified Julian Date
  real(r_double)     :: obsEndTime     ! Modified Julian Date
  real(r_double)     :: fileCreateTime ! Modified Julian Date
  integer(i_long)    :: totalHeaderLen
  integer(i_long)    :: dataLen
  integer(i_byte)    :: qcflag1
  integer(i_byte)    :: qcflag2
  integer(i_byte)    :: qcflag3
  integer(i_byte)    :: qcflag4
  character(len=32)  :: version
  character(len=128) :: fileName
  character(len=40)  :: dummy40
end type basic_info

type data_info
  integer(i_byte)    :: headerNum      ! header block number = 2
  integer(i_short)   :: blockLen       ! block length = 50 bytes
  integer(i_short)   :: bitPix         ! number of bits per pixel = 16
  integer(i_short)   :: nPix           ! number of columns (pixels east-west)
  integer(i_short)   :: nLin           ! number of lines (pixels north-south)
  integer(i_byte)    :: compression    ! 0: no compression (default), 1: gzip, 2: bzip2
  character(len=40)  :: dummy40
end type data_info

type proj_info
  integer(i_byte)    :: headerNum      ! header block number = 3
  integer(i_short)   :: blockLen       ! block length = 127 bytes
  real(r_double)     :: subLon         ! 140.7 degree
  integer(i_long)    :: cfac           ! column scaling factor
  integer(i_long)    :: lfac           ! line scaling factor
  real(r_single)     :: coff           ! column offset
  real(r_single)     :: loff           ! line offset
  real(r_double)     :: satDis         ! distance from earth's center to virtual satellite = 42164 km
  real(r_double)     :: eqtrRadius     ! earth's equatorial radius = 6378.1370 km
  real(r_double)     :: polrRadius     ! earth's polar radius = 6356.7523 km
  real(r_double)     :: projParam1     ! 0.00669438444
  real(r_double)     :: projParam2     ! 0.993305616
  real(r_double)     :: projParam3     ! 1.006739501
  real(r_double)     :: projParamSd    ! 1737122264
  integer(i_short)   :: resampleKind
  integer(i_short)   :: resampleSize
  character(len=40)  :: dummy40
end type proj_info

type navi_info
  integer(i_byte)    :: headerNum      ! header block number = 4
  integer(i_short)   :: blockLen       ! block length = 139 bytes
  real(r_double)     :: navTime        ! navigation information time in MJD
  real(r_double)     :: sspLon
  real(r_double)     :: sspLat
  real(r_double)     :: satDis
  real(r_double)     :: nadirLon
  real(r_double)     :: nadirLat
  real(r_double)     :: sunPos_x
  real(r_double)     :: sunPos_y
  real(r_double)     :: sunPos_z
  real(r_double)     :: moonPos_x
  real(r_double)     :: moonPos_y
  real(r_double)     :: moonPos_z
  character(len=40)  :: dummy40
end type navi_info

type calib_info
  integer(i_byte)    :: headerNum      ! header block number = 5
  integer(i_short)   :: blockLen       ! block length = 147 bytes
  integer(i_short)   :: bandNo
  real(r_double)     :: waveLen
  integer(i_short)   :: bitPix
  integer(i_short)   :: errorCount
  integer(i_short)   :: outCount
! count-radiance conversion equation
  real(r_double)     :: gain_cnt2rad
  real(r_double)     :: cnst_cnt2rad
! correction coefficient of sensor Planck function for converting radiance to brightness temperature
  real(r_double)     :: rad2btp_c0
  real(r_double)     :: rad2btp_c1
  real(r_double)     :: rad2btp_c2
! for converting brightness temperature to radiance
  real(r_double)     :: btp2rad_c0
  real(r_double)     :: btp2rad_c1
  real(r_double)     :: btp2rad_c2
  real(r_double)     :: lightSpeed
  real(r_double)     :: planckConst
  real(r_double)     :: bolzConst
  character(len=40)  :: dummy40
end type calib_info

type interCalib_info
  integer(i_byte)    :: headerNum      ! header block number = 6
  integer(i_short)   :: blockLen       ! block length = 259 bytes
  character(len=256) :: dummy256
end type interCalib_info

type segm_info
  integer(i_byte)    :: headerNum      ! header block number = 7
  integer(i_short)   :: blockLen       ! block length = 47 bytes
  integer(i_byte)    :: totalSegNum    ! total number of segments. 1: no divisions
  integer(i_byte)    :: segSeqNo       ! segment sequence number
  integer(i_short)   :: startLineNo    ! first line number of image segment
  character(len=40)  :: dummy40
end type segm_info

type naviCorr_info
  integer(i_byte)    :: headerNum      ! header block number = 8
  integer(i_short)   :: blockLen       ! block length
  real(r_single)     :: RoCenterColumn
  real(r_single)     :: RoCenterLine
  real(r_double)     :: RoCorrection
  integer(i_short)   :: correctNum
  integer(i_short), allocatable :: lineNo(:)      !(correctNum)
  real(r_single),   allocatable :: columnShift(:) !(correctNum)
  real(r_single),   allocatable :: lineShift(:)   !(correctNum)
  character(len=40)  :: dummy40
end type naviCorr_info

type obsTime_info
  integer(i_byte)    :: headerNum      ! header block number = 9
  integer(i_short)   :: blockLen       ! block length
  integer(i_short)   :: obsNum         ! number of observation times
  integer(i_short), allocatable :: lineNo(:) !(obsNum)
  real(r_double),   allocatable :: obsMJD(:) !(obsNum) observation time in MJD
  character(len=40)  :: dummy40
end type obsTime_info

type error_info
  integer(i_byte)    :: headerNum      ! header block number = 10
  integer(i_long)    :: blockLen       ! block length = 47
  integer(i_short)   :: errorNum       ! number of error information data = 0
!  integer(i_short), allocatable :: lineNo(:)    !(errorNum)
!  integer(i_short), allocatable :: errPixNum(:) !(errorNum)
  character(len=40)  :: dummy40
end type error_info

type dummy_info
  integer(i_byte)    :: headerNum      ! header block number = 11
  integer(i_short)   :: blockLen       ! block length = 259 bytes
  character(len=256) :: dummy256
end type dummy_info

type HSD_header
  type(basic_info)      :: basic
  type(data_info)       :: data
  type(proj_info)       :: proj
  type(navi_info)       :: navi
  type(calib_info)      :: calib
  type(interCalib_info) :: interCalib
  type(segm_info)       :: segm
  type(naviCorr_info)   :: navicorr
  type(obsTime_info)    :: obstime
  type(error_info)      :: error
  type(dummy_info)      :: dummy
end type HSD_header

type(HSD_header) :: header

real(r_double), parameter :: pi = acos(-1.0)
real(r_double), parameter :: deg2rad = pi/180.0
real(r_double), parameter :: rad2deg = 180.0/pi

contains

subroutine read_HSD(ccyymmddhhnn, inpdir)

implicit none

character(len=12), intent(in) :: ccyymmddhhnn
character(len=*),  intent(in) :: inpdir

character(len=1051) :: dummy1051
character(len=1)    :: dummy1
character(len=16)   :: dummy16
character(len=8)    :: ccyymmdd
character(len=4)    :: ccyy, hhnn
character(len=2)    :: mm, dd, hh, nn
character(len=3)    :: satellite = 'H08'
character(len=4)    :: region = 'FLDK'
character(len=3)    :: resolution = 'R20'
character(len=5)    :: segment ! S0110, S0210, etc
character(len=3)    :: band    ! B07 - B16
integer(i_short), allocatable :: idata(:)
integer(i_kind) :: blocklen_8
integer(i_kind) :: numCorrect
integer(i_kind) :: numObs
integer(i_kind) :: ipixel, iline
integer(i_kind) :: startLine, endLine
integer(i_kind) :: count, i, ii, jj, ij, iv
integer(i_kind) :: iband, isegm
integer(i_kind) :: ierr
integer(i_kind) :: nlocs, nvars, iloc
integer(i_kind) :: ihh, imm, idd, jday
integer(i_kind) :: iunit = 21
real(r_double)  :: lon, lat
real(r_double)  :: radiance, tbb
real(r_double)  :: lon_sat, h_sat, r_eq
! end of declaration
continue

longitude(:,:) = missing_r
latitude(:,:)  = missing_r
brit(:,:,:) = missing_r
solzen(:,:) = missing_r
satzen(:,:) = missing_r
valid(:,:)  = .false.

! construct file names
nfile = nband * nsegm
allocate (fnames(nfile))
allocate (fexist(nfile))

ccyymmdd = ccyymmddhhnn(1:8)
hhnn     = ccyymmddhhnn(9:12)
ccyy     = ccyymmddhhnn(1:4)
mm       = ccyymmddhhnn(5:6)
dd       = ccyymmddhhnn(7:8)
hh       = ccyymmddhhnn(9:10)
nn       = ccyymmddhhnn(11:12)
read(mm, '(i2)') imm  !month
read(dd, '(i2)') idd  !day
jday = 0
do i = 1, imm - 1
   jday = jday + mmday(i)
end do
jday = jday + idd

do iband = 1, nband
   do isegm = 1, nsegm
      write(band, '(a,i2.2)') 'B', iband+6
      write(segment,'(a,i2.2,i2.2)') 'S', isegm, nsegm
      ij = isegm + (iband-1) * nsegm
      fnames(ij) = trim(inpdir)//'HS_'//satellite//'_'//ccyymmdd//'_'//hhnn//'_'//band//'_'//region//'_'//resolution//'_'//segment//'.DAT'
!write(33,*) 'wget -np -nd -nc http://noaa-himawari8.s3.amazonaws.com/AHI-L1b-FLDK/' &
!& //ccyy//'/'//mm//'/'//dd//'/'//hhnn//'/'//trim(fnames(ij))//'.bz2'
      inquire(file=trim(fnames(ij)), exist=fexist(ij))
!print*,iband, isegm, trim(fnames(ij)), fexist(ij)
   end do
end do

do iband = 1, nband
   do isegm = 1, nsegm
      ifile = isegm + (iband-1) * nsegm
      if ( .not. fexist(ifile) ) cycle
      open(iunit, file=trim(fnames(ifile)), form='unformatted', access='stream', status='old')

print*,'Reading from ', trim(fnames(ifile))
      read(iunit) header%basic, &
                  header%data,  &
                  header%proj,  &
                  header%navi,  &
                  header%calib, &
                  header%interCalib, &
                  header%segm,  &
                  header%navicorr%headerNum, &
             header%navicorr%blockLen, &
             header%navicorr%RoCenterColumn, &
             header%navicorr%RoCenterLine, &
             header%navicorr%RoCorrection, &
             header%navicorr%correctNum
      if ( header%navicorr%correctNum > 0 ) then
         numCorrect = header%navicorr%correctNum
         allocate(header%navicorr%lineNo(numCorrect))
         allocate(header%navicorr%columnShift(numCorrect))
         allocate(header%navicorr%lineShift(numCorrect))
      end if
      rewind(iunit)
      read(iunit) header%basic, &
                  header%data,  &
                  header%proj,  &
                  header%navi,  &
                  header%calib, &
                  header%interCalib, &
                  header%segm,  &
                  header%navicorr%headerNum, &
                  header%navicorr%blockLen, &
                  header%navicorr%RoCenterColumn, &
                  header%navicorr%RoCenterLine, &
                  header%navicorr%RoCorrection, &
                  header%navicorr%correctNum, &
                  header%navicorr%lineNo, &
                  header%navicorr%columnShift, &
                  header%navicorr%lineShift, &
                  header%navicorr%dummy40, &
                  header%obsTime%headerNum, &
                  header%obsTime%blockLen, &
                  header%obsTime%obsNum
      if ( header%obsTime%obsNum > 0 ) then
         numObs = header%obsTime%obsNum
         allocate(header%obsTime%lineNo(numObs))
         allocate(header%obsTime%obsMJD(numObs))
      end if
      npix = header%data%nPix
      nlin = header%data%nLin
      ntotal = npix * nlin
      allocate(idata(ntotal))
      rewind(iunit)
      read(iunit) header%basic, &
                  header%data,  &
                  header%proj,  &
                  header%navi,  &
                  header%calib, &
                  header%interCalib, &
                  header%segm,  &
                  header%navicorr%headerNum, &
                  header%navicorr%blockLen, &
                  header%navicorr%RoCenterColumn, &
                  header%navicorr%RoCenterLine, &
                  header%navicorr%RoCorrection, &
                  header%navicorr%correctNum, &
                  header%navicorr%lineNo, &
                  header%navicorr%columnShift, &
                  header%navicorr%lineShift, &
                  header%navicorr%dummy40, &
                  header%obsTime%headerNum, &
                  header%obsTime%blockLen, &
                  header%obsTime%obsNum, &
                  header%obsTime%lineNo, &
                  header%obsTime%obsMJD, &
                  header%obsTime%dummy40, &
                  header%error, &
                  header%dummy, &
                  idata

      startLine = header%segm%startLineNo
      endLine = startLine + header%data%nLin - 1
!print*,ifile, trim(fnames(ifile)), header%data%nPix, header%data%nLin, startLine, endLine
      lon_sat = header%proj%subLon * deg2rad
      h_sat =  header%proj%satDis * 1000.0
      r_eq = header%proj%eqtrRadius * 1000.0
      do jj = 1, header%data%nLin
         do ii = 1, header%data%nPix

            tbb = missing_r
            lon = missing_r
            lat = missing_r

            ij = ii + (jj-1) * header%data%nPix
            iline = header%segm%startLineNo + jj - 1
            ipixel = ii

            if ( abs(latitude(ipixel, iline)) > 90.0 .or. &
                 abs(longitude(ipixel, iline)) > 360.0 ) then
               call pixlin_to_lonlat(ipixel, iline, lon, lat, ierr)
               if ( ierr == 0 ) then
                  valid(ipixel, iline) = .true.
                  latitude(ipixel, iline) = lat
                  longitude(ipixel, iline) = lon
                  call calc_solar_zenith_angle( &
                     latitude(ipixel, iline), &
                     longitude(ipixel, iline), &
                     ihh, imm, jday, &
                     solzen(ipixel, iline))
                  ! calculate geostationary satellite zenith angle
                  rlat = lat * deg2rad ! in radian
                  rlon = lon * deg2rad ! in radian
                  lon_diff = abs(rlon-lon_sat)
                  tmp1 = sqrt((2.0*r_eq*sin(lon_diff/2.)-r_eq*(1.0-cos(rlat))*sin(lon_diff/2.))**2 &
                         +(2.0*r_eq*sin(rlat/2.))**2-(r_eq*(1.0-cos(rlat))*sin(lon_diff/2.))**2)
                  theta1 = 2.0*asin(tmp1/r_eq/2.)
                  theta2 = atan(r_eq*sin(theta1)/((h_sat-r_eq)+r_eq*(1.0-sin(theta1))))
                  satzen(ipixel, iline) = (theta1+theta2) * rad2deg
                  if ( satzen(ipixel, iline) > 65.0 ) then
                     valid(ipixel, iline) = .false.
                  end if
               end if
            end if

            count = idata(ij)
            if ( count /= header%calib%outCount .and. &
                 count /= header%calib%errorCount .and. &
                 count > 0 ) then
               ! convert count value to radiance
               radiance = count * header%calib%gain_cnt2rad + &
                          header%calib%cnst_cnt2rad
               radiance = radiance * 1000000.0  ! [ W/(m^2 sr micro m)] => [ W/(m^2 sr m)]
               ! convert radiance to physical value
               ! infrared band
               ! check header[n]->calib->bandNo
               call hisd_radiance_to_tbb(radiance, tbb)
               ! visible or near infrared band
               !  data->phys[kk] = header[n]->calib->rad2albedo * radiance
!print*,iband+6, ij, ii, jj, count, tbb, lon, lat, solzen(ipixel, iline), satzen(ipixel, iline)
               brit(ipixel, iline, iband) = tbb
            end if

         end do ! pixel
      end do ! line

      deallocate(idata)
      if ( header%obsTime%obsNum > 0 ) then
         deallocate(header%obsTime%lineNo)
         deallocate(header%obsTime%obsMJD)
      end if
      if ( header%navicorr%correctNum > 0 ) then
         deallocate(header%navicorr%lineNo)
         deallocate(header%navicorr%columnShift)
         deallocate(header%navicorr%lineShift)
      end if

      close(iunit)

   end do
end do

deallocate (fexist)
deallocate (fnames)

!print*,'transfering to xdata'

nlocs = 0
do jj = 1, nline , subsample
   do ii = 1, npixel , subsample
      if ( valid(ii,jj) ) then
         nlocs = nlocs + 1
      end if
   end do
end do

allocate(xdata(1,1))
nvars = nband
xdata(1,1) % nlocs = nlocs
xdata(1,1) % nrecs = nlocs
xdata(1,1) % nvars = nvars
! allocate and initialize
allocate (xdata(1,1)%xinfo_float(nlocs, nvar_info))
allocate (xdata(1,1)%xinfo_int  (nlocs, nvar_info))
allocate (xdata(1,1)%xinfo_char (nlocs, nvar_info))
allocate (xdata(1,1)%xseninfo_float(nlocs, nsen_info))
allocate (xdata(1,1)%xseninfo_int  (nvars, nsen_info))
xdata(1,1)%xinfo_float   (:,:) = missing_r
xdata(1,1)%xinfo_int     (:,:) = missing_i
xdata(1,1)%xinfo_char    (:,:) = ''
xdata(1,1)%xseninfo_float(:,:) = missing_r
xdata(1,1)%xseninfo_int  (:,:) = missing_i
if ( nvars > 0 ) then
   allocate (xdata(1,1)%xfield(nlocs, nvars))
   xdata(1,1)%xfield(:,:)%val = missing_r
   xdata(1,1)%xfield(:,:)%qm  = missing_i
   xdata(1,1)%xfield(:,:)%err = missing_r
   allocate (xdata(1,1)%var_idx(nvars))
   do iv = 1, nvars
      xdata(1,1)%var_idx(iv) = iv
   end do
end if

datetime = ccyy//'-'//mm//'-'//dd//'T'//hh//':'//nn//':00Z'
read (ccyymmddhhnn,'(i4,4i2)') iyear, imonth, iday, ihour, imin
isec = 0
call get_julian_time(iyear, imonth, iday, ihour, imin, isec, gstime, epochtime)

iloc = 0
do jj = 1, nline, subsample
   do ii = 1, npixel, subsample
      if ( .not. valid(ii,jj) ) cycle
      iloc = iloc + 1

!print*,ii,jj, latitude(ii,jj), longitude(ii,jj), satzen(ii,jj), solzen(ii,jj), brit(ii,jj,1)

      do i = 1, nvar_info
         if ( type_var_info(i) == nf90_int ) then
         else if ( type_var_info(i) == nf90_float ) then
            if ( trim(name_var_info(i)) == 'station_elevation' ) then
               xdata(1,1)%xinfo_float(iloc,i) = missing_r
            else if ( trim(name_var_info(i)) == 'latitude' ) then
               xdata(1,1)%xinfo_float(iloc,i) = latitude(ii,jj)
            else if ( trim(name_var_info(i)) == 'longitude' ) then
               xdata(1,1)%xinfo_float(iloc,i) = longitude(ii,jj)
            end if
         else if ( type_var_info(i) == nf90_char ) then
            if ( trim(name_var_info(i)) == 'datetime' ) then
               xdata(1,1)%xinfo_char(iloc,i) = datetime
            else if ( trim(name_var_info(i)) == 'station_id' ) then
               xdata(1,1)%xinfo_char(iloc,i) = 'ahi_himawari8'
            end if
         else if ( type_var_info(i) == nf90_int64 ) then
            if ( trim(name_var_info(i)) == 'dateTime' ) then
               xdata(1,1)%xinfo_int64(iloc,i) = epochtime
            end if
         end if
      end do

      do i = 1, nsen_info
         if ( type_sen_info(i) == nf90_float ) then
            if ( trim(name_sen_info(i)) == 'scan_position' ) then
               xdata(1,1)%xseninfo_float(iloc,i) = ii
            else if ( trim(name_sen_info(i)) == 'sensor_zenith_angle' ) then
               xdata(1,1)%xseninfo_float(iloc,i) = satzen(ii,jj)
            else if ( trim(name_sen_info(i)) == 'solar_zenith_angle' ) then
               xdata(1,1)%xseninfo_float(iloc,i) = solzen(ii,jj)
            else if ( trim(name_sen_info(i)) == 'sensor_azimuth_angle' ) then
               xdata(1,1)%xseninfo_float(iloc,i) = missing_r
            else if ( trim(name_sen_info(i)) == 'solar_azimuth_angle' ) then
               xdata(1,1)%xseninfo_float(iloc,i) = missing_r
            else if ( trim(name_sen_info(i)) == 'sensor_view_angle' ) then
               !call calc_sensor_view_angle(trim(rlink%inst), rlink%scanpos, xdata(1,1)%xseninfo_float(iloc,i))
               xdata(1,1)%xseninfo_float(iloc,i) = satzen(ii,jj)
            end if
!         else if ( type_sen_info(i) == nf90_int ) then
!         else if ( type_sen_info(i) == nf90_char ) then
         end if
      end do

      iv = ufo_vars_getindex(name_sen_info, 'sensor_channel')
      xdata(1,1)%xseninfo_int(:,iv) = (/ 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 /)

      do i = 1, nvars
         xdata(1,1)%xfield(iloc,i)%val = brit(ii,jj,i)
         xdata(1,1)%xfield(iloc,i)%err = 1.0
         !call set_brit_obserr(trim(rlink%inst), i, xdata(1,1)%xfield(iloc,i)%err)
         xdata(1,1)%xfield(iloc,i)%qm  = 0
      end do
   end do
end do

end subroutine read_HSD

subroutine pixlin_to_lonlat(pix, lin, lon, lat, ierr)

 implicit none

 integer(i_kind), intent(in)  :: pix, lin
 real(r_double),  intent(out) :: lon, lat
 integer(i_kind), intent(out) :: ierr

 real(r_double) :: SCLUNIT = 2.0**(-16)
 real(r_double) :: x, y
 real(r_double) :: Sd, Sn, S1, S2, S3, Sxy
 real(r_double) :: c, l

 ! initialize
 lon = missing_r
 lat = missing_r
 ierr = 0

 ! pix, lin -> c, l
 c = float(pix)
 l = float(lin)

 ! intermediate coordinates (x,y)
 ! Global Specification 4.4.4 Scaling Function
 !    c = COFF + nint(x * 2^-16 * CFAC)
 !    l = LOFF + nint(y * 2^-16 * LFAC)
 ! The intermediate coordinates (x,y) are as follows :
 !    x = (c -COFF) / (2^-16 * CFAC)
 !    y = (l -LOFF) / (2^-16 * LFAC)
 !    SCLUNIT = 2^-16
 x = deg2rad * ( c - header%proj%coff) / ( SCLUNIT * header%proj%cfac)
 y = deg2rad * ( l - header%proj%loff) / ( SCLUNIT * header%proj%lfac)

 ! longtitude,latitude
 ! Global Specification 4.4.3.2
 ! The invers projection function is as follows :
 !   lon = arctan(S2/S1) + sub_lon
 !   lat = arctan( (Req^2/Rpol^2) * S3 / Sxy )
 !
 ! Thererin the variables S1,S2,S3,Sxy are as follows :
 !    S1  = Rs - Sn * cos(x) * cos(y)
 !    S2  = Sn * sin(x) * cos(y)
 !    S3  =-Sn * sin(y)
 !    Sxy = sqrt(S1^2 + S2^2)
 !    Sn  =(Rs * cos(x) * cos(y) - Sd ) /
 !         (cos(y) * cos(y) + (Req^2/Rpol^2) * sin(y) * sin(y))
 !    Sd  =sqrt( (Rs * cos(x) * cos(y))^2
 !               - ( cos(y) * cos(y) + (Req^2/Rpol^2) * sin(y) * sin(y) )
 !               * (Rs^2 - Req^2)
 ! The variables Rs,Rpol,Req,(Req^2/Rpol^2),(Rs^2 - Req^2) are as follows :
 !    Rs  : distance from Earth center to satellite= head->proj->satDis
 !    Rpol: polar radius of the Earth              = head->proj->polrRadius
 !    Req : equator raidus of the Earth            = head->proj->eqtrRadius
 !    (Req^2/Rpol^2)                               = head->proj->projParam3
 !    (Rs^2 - Req^2)                               = head->proj->projParamSd
 Sd = (header%proj%satDis * cos(x) * cos(y)) * &
      (header%proj%satDis * cos(x) * cos(y)) - &
      (cos(y) * cos(y) + header%proj%projParam3 * sin(y) * sin(y)) * &
       header%proj%projParamSd
 if ( Sd < 0 ) then
    !write(*,*) 'Error in Sd'
    ierr = -1
    return
 else
    Sd = sqrt(Sd)
 end if
 Sn = (header%proj%satDis * cos(x) * cos(y) -Sd) / &
      (cos(y) * cos(y) + header%proj%projParam3 * sin(y) * sin(y))
 S1 = header%proj%satDis - (Sn * cos(x) * cos(y))
 S2 = Sn * sin(x) * cos(y)
 S3 =-Sn * sin(y)
 Sxy=sqrt( S1 * S1 + S2 * S2)

 lon = rad2deg * atan2(S2,S1) + header%proj%subLon
 lat = rad2deg * atan(header%proj%projParam3 * S3 / Sxy)

 ! check longtitude
 if ( lon >  180.0 ) lon = lon - 360.0
 if ( lon < -180.0 ) lon = lon + 360.0

 return
end subroutine pixlin_to_lonlat

subroutine calc_solar_zenith_angle(xlat, xlon, gmt, minute, julian, solzen)

! the calulcation is adapted from subroutines radconst and calc_coszen in
! WRF phys/module_radiation_driver.F

 implicit none

 real(r_single),  intent(in)    :: xlat, xlon
 integer(i_kind), intent(in)    :: gmt, minute, julian
 real(r_single),  intent(inout) :: solzen

 real(r_single) :: obliq = 23.5
 real(r_single) :: deg_per_day = 360.0/365.0
 real(r_single) :: slon   ! longitude of the sun
 real(r_single) :: declin ! declination of the sun
 real(r_single) :: hrang, da, eot, xt, tloctm, rlat

 ! initialize to missing values
 solzen = missing_r

 ! calculate longitude of the sun from vernal equinox
 if ( julian >= 80 ) slon = (julian - 80 ) * deg_per_day
 if ( julian <  80 ) slon = (julian + 285) * deg_per_day

 declin = asin(sin(obliq*deg2rad)*sin(slon*deg2rad)) ! in radian

 da = 6.2831853071795862*(julian-1)/365.
 eot = (0.000075+0.001868*cos(da)-0.032077*sin(da) &
        -0.014615*cos(2.0*da)-0.04089*sin(2.0*da))*(229.18)
 xt = gmt + (minute + eot)/60.0

 if ( abs(xlon) > 360.0 .or. abs(xlat) > 90.0 ) return
 tloctm = xt + xlon/15.0
 hrang = 15.0*(tloctm-12.0) * deg2rad
 rlat = xlat * deg2rad
 solzen = acos( sin(rlat)*sin(declin) + &
                cos(rlat)*cos(declin)*cos(hrang) )
 solzen = solzen * rad2deg

 return
end subroutine calc_solar_zenith_angle

subroutine hisd_radiance_to_tbb (radiance, tbb)

 implicit none

 real(r_double), intent(in)  :: radiance
 real(r_double), intent(out) :: tbb

 real(r_double) :: lambda
 real(r_double) :: planck_c1
 real(r_double) :: planck_c2

 real(r_double) :: effective_temperature

 ! central wave length
 lambda = header%calib%waveLen / 1000000.0 ! [micro m] => [m]

 ! radiance = radiance * 1000000.0  ! [ W/(m^2 sr micro m)] => [ W/(m^2 sr m)]

 ! planck_c1 = (2 * h * c^2 / lambda^5)
 planck_c1 = 2.0 * header%calib%planckConst *  &
             header%calib%lightSpeed ** 2 / &
             lambda ** 5

 ! planck_c2 = (h * c / k / lambda )
 planck_c2 = header%calib%planckConst * header%calib%lightSpeed / &
             header%calib%bolzConst / lambda

 if ( radiance > 0 ) then
    effective_temperature = planck_c2 / &
                            log( (planck_c1 / radiance ) + 1.0 )
    tbb = header%calib%rad2btp_c0 + &
          header%calib%rad2btp_c1 * effective_temperature + &
          header%calib%rad2btp_c2 * effective_temperature ** 2
 else
    tbb = missing_r
 end if
 return
end subroutine hisd_radiance_to_tbb

end module ahi_HSD_mod
