module radiance_mod

use kinds, only: r_kind,i_kind,r_double
use define_mod, only: missing_r, missing_i, nstring, ndatetime, &
   ninst, inst_list, set_name_satellite, set_name_sensor, xdata, name_sen_info, &
   nvar_info, name_var_info, type_var_info, nsen_info, type_sen_info, set_brit_obserr, &
   dtime_min, dtime_max, strlen
use ufo_vars_mod, only: ufo_vars_getindex
use netcdf, only: nf90_float, nf90_int, nf90_char
use utils_mod, only: get_julian_time, da_advance_time, da_get_time_slots

implicit none
private
public  :: read_amsua_amsub_mhs
public  :: read_airs_colocate_amsua
public  :: read_iasi
public  :: read_cris
public  :: sort_obs_radiance
public  :: radiance_to_temperature

real(r_kind), parameter  :: r8bfms = 9.0E08  ! threshold to check for BUFR missing value

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
   real(r_double)            :: gstime
   integer(i_kind)           :: ifgat
   integer(i_kind)           :: inst_idx   ! index of inst in inst_list
   integer(i_kind)           :: landsea
   integer(i_kind)           :: scanpos
   integer(i_kind)           :: scanline
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

!--------------------------------------------------------------

subroutine read_amsua_amsub_mhs (filename, filedate)

!| NC021023 | A61223 | MTYP 021-023 PROC AMSU-A 1B Tb (NOAA-15-19, METOP-1,2)   |
!| NC021024 | A61224 | MTYP 021-024 PROCESSED AMSU-B 1B Tb (NOAA-15-17)         |
!| NC021025 | A61225 | MTYP 021-025 PROCESSED HIRS-3 1B Tb (NOAA-15-17)         |
!| NC021027 | A61234 | MTYP 021-027 PROCESSED MHS Tb (NOAA-18-19, METOP-1,2)    |
!| NC021028 | A61245 | MTYP 021-028 PROC HIRS-4 1B Tb (NOAA-18-19, METOP-1,2)   |
!|          |                                                                   |
!| NC021023 | YEAR  MNTH  DAYS  HOUR  MINU  SECO  207002  CLAT  CLON  207000    |
!| NC021023 | SAID  SIID  FOVN  LSQL  SAZA  SOZA  HOLS  202127  HMSL  202000    |
!| NC021023 | SOLAZI  BEARAZ  "BRITCSTC"15                                      |
!|          |                                                                   |
!| NC021024 | YEAR  MNTH  DAYS  HOUR  MINU  SECO  207002  CLAT  CLON  207000    |
!| NC021024 | SAID  SIID  FOVN  LSQL  SAZA  SOZA  HOLS  202127  HMSL  202000    |
!| NC021024 | SOLAZI  BEARAZ  "BRITCSTC"5                                       |
!|          |                                                                   |
!| NC021025 | YEAR  MNTH  DAYS  HOUR  MINU  SECO  207002  CLAT  CLON  207000    |
!| NC021025 | SAID  SIID  FOVN  LSQL  SAZA  SOZA  HOLS  202127  HMSL  202000    |
!| NC021025 | SOLAZI  BEARAZ  "BRIT"20                                          |
!|          |                                                                   |
!| NC021027 | YEAR  MNTH  DAYS  HOUR  MINU  SECO  207002  CLAT  CLON  207000    |
!| NC021027 | SAID  SIID  FOVN  LSQL  SAZA  SOZA  HOLS  202127  HMSL  202000    |
!| NC021027 | SOLAZI  BEARAZ  "BRITCSTC"5                                       |
!|          |                                                                   |
!| NC021028 | YEAR  MNTH  DAYS  HOUR  MINU  SECO  207002  CLAT  CLON  207000    |
!| NC021028 | SAID  SIID  FOVN  LSQL  SAZA  SOZA  HOLS  202127  HMSL  202000    |
!| NC021028 | SOLAZI  BEARAZ  "BRIT"20                                          |
!|          |                                                                   |
!| BRITCSTC | CHNM  TMBR  CSTC                                                  |
!| BRIT     | CHNM  TMBR                                                        |

   implicit none

   character (len=*),  intent(in)  :: filename
   character (len=10), intent(out) :: filedate  ! ccyymmddhh


   integer(i_kind), parameter :: ntime = 6      ! number of data to read in timestr
   integer(i_kind), parameter :: ninfo = 10     ! number of data to read in infostr
   integer(i_kind), parameter :: nlalo = 2      ! number of data to read in lalostr
   integer(i_kind), parameter :: nbrit = 2      ! number of data to read in britstr
   integer(i_kind), parameter :: maxchan = 15   ! max nchan among amsua, amsub, mhs

   character(len=80) :: timestr, infostr, lalostr, britstr

   real(r_double), dimension(ntime)     :: timedat
   real(r_double), dimension(ninfo)     :: infodat
   real(r_double), dimension(nlalo)     :: lalodat
   real(r_double), dimension(2,maxchan) :: data1b8

   character(len=8)  :: subset
   character(len=10) :: cdate

   integer(i_kind) :: iunit, iost, iret, i
   integer(i_kind) :: nchan
   integer(i_kind) :: idate
   integer(i_kind) :: num_report_infile
   integer(i_kind) :: ireadmg, ireadsb

   integer(i_kind) :: iyear, imonth, iday, ihour, imin, isec
   real(r_double)  :: ref_time, obs_time

   write(*,*) '--- reading '//trim(filename)//' ---'

   timestr = 'YEAR MNTH DAYS HOUR MINU SECO'
   infostr = 'SAID SIID FOVN LSQL SAZA SOZA HOLS HMSL SOLAZI BEARAZ'
   lalostr = 'CLAT CLON'
   britstr = 'CHNM TMBR'

   num_report_infile  = 0

   iunit = 96

   ! open bufr file
   open (unit=iunit, file=trim(filename), &
         iostat=iost, form='unformatted', status='old')
   if (iost /= 0) then
      write(unit=*,fmt='(a,i5,a)') &
         "Error",iost," opening BUFR obs file "//trim(filename)
         return
   end if

   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readmg(iunit,subset,idate,iret)

   if ( iret /= 0 ) then
      write(unit=*,fmt='(A,I5,A)') &
         "Error",iret," reading BUFR obs file "//trim(filename)
      call closbf(iunit)
      return
   end if
   rewind(iunit)

   write(unit=*,fmt='(1x,a,i10)') trim(filename)//' file date is: ', idate
   write(unit=filedate, fmt='(i10)') idate
   read (filedate(1:10),'(i4,3i2)') iyear, imonth, iday, ihour
   call get_julian_time (iyear,imonth,iday,ihour,0,ref_time)

   if ( .not. associated(rhead) ) then
      nullify ( rhead )
      allocate ( rhead )
      nullify ( rhead%next )
   end if

   if ( .not. associated(rlink) ) then
      rlink => rhead
   else
      allocate ( rlink%next )
      rlink => rlink%next
      nullify ( rlink%next )
   end if

   msg_loop: do while (ireadmg(iunit,subset,idate)==0)
!print*,subset
      subset_loop: do while (ireadsb(iunit)==0)

         num_report_infile = num_report_infile + 1

         call ufbint(iunit,timedat,ntime,1,iret,timestr)

         iyear  = nint(timedat(1))
         imonth = nint(timedat(2))
         iday   = nint(timedat(3))
         ihour  = nint(timedat(4))
         imin   = nint(timedat(5))
         isec   = min(59, nint(timedat(6))) ! raw BUFR data that has SECO = 60.0 SECOND
                                            ! that was probably rounded from 59.x seconds
                                            ! reset isec to 59 rather than advancing one minute
         if ( iyear  > 1900 .and. iyear  < 3000 .and. &
              imonth >=   1 .and. imonth <=  12 .and. &
              iday   >=   1 .and. iday   <=  31 .and. &
              ihour  >=   0 .and. ihour  <   24 .and. &
              imin   >=   0 .and. imin   <   60 .and. &
              isec   >=   0 .and. isec   <   60 ) then
            write(unit=rlink%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
               iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'
            call get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
            rlink%dhr = (obs_time + (isec/60.0) - ref_time)/60.0
            rlink%gstime = obs_time
         else
            cycle subset_loop
         end if

         call ufbint(iunit,lalodat,nlalo,1,iret,lalostr)
         if ( abs(lalodat(1)) > 90.0 .or. abs(lalodat(1)) > 360.0 ) cycle subset_loop

         call ufbint(iunit,infodat,ninfo,1,iret,infostr)
         call ufbrep(iunit,data1b8,2,maxchan,nchan,britstr)

         rlink % nchan = nchan
         if ( nchan > 0 ) then
            allocate ( rlink % tb(nchan) )   ! brightness temperature
            allocate ( rlink % ch(nchan) )   ! channel number
         end if

         call fill_datalink(rlink, missing_r, missing_i)

         if ( lalodat(1) < r8bfms ) rlink % lat = lalodat(1)
         if ( lalodat(2) < r8bfms ) rlink % lon = lalodat(2)

         rlink % satid  = nint(infodat(1))  ! SAID satellite identifier
         rlink % instid = nint(infodat(2))  ! SIID instrument identifier

         if ( infodat(3)  < r8bfms ) rlink % scanpos = nint(infodat(3)) ! FOVN field of view number
         if ( infodat(4)  < r8bfms ) rlink % landsea = infodat(4)       ! LSQL land sea qualifier 0:land, 1:sea, 2:coast
         if ( infodat(5)  < r8bfms ) rlink % satzen  = infodat(5)       ! SAZA satellite zenith angle (degree)
         if ( infodat(10) < r8bfms ) rlink % satazi  = infodat(10)      ! BEARAZ satellite azimuth (degree true)
         if ( infodat(6)  < r8bfms ) rlink % solzen  = infodat(6)       ! SOZA solar zenith angle (degree)
         if ( infodat(9)  < r8bfms ) rlink % solazi  = infodat(9)       ! SOLAZI solar azimuth (degree true)
         if ( infodat(7)  < r8bfms ) rlink % elv     = infodat(7)       ! HOLS height of land surface (m)

         if ( nchan > 0 ) then
            do i = 1, nchan
               if ( data1b8(1,i) < r8bfms ) rlink % ch(i) = nint(data1b8(1,i))
               if ( data1b8(2,i) < r8bfms ) rlink % tb(i) = data1b8(2,i)
            end do
         end if

         allocate ( rlink%next )
         rlink => rlink%next
         nullify ( rlink%next )

      end do subset_loop ! ireadsb
   end do msg_loop ! ireadmg

   call closbf(iunit)
   close(iunit)

   write(*,'(1x,a,a,a,i10)') 'num_report_infile ', trim(filename), ' : ', num_report_infile

end subroutine read_amsua_amsub_mhs

!--------------------------------------------------------------

subroutine read_airs_colocate_amsua (filename, filedate)

! adapted from WRFDA/var/da/da_radiance/da_read_obs_bufrairs.inc

!| NC021249 | A50243 | MTYP 021-249 EVERY  FOV AIRS/AMSU-A/HSB 1B BTEMPS(AQUA)  |
!|          |                                                                   |
!| NC021249 | SCO1C3IN                                                          |
!|          |                                                                   |
!| SCO1C3IN | SPITSEQN  SITPSEQN  (SCBTSEQN)  "SVCASEQN"4  TOCC  AMSUSPOT       |
!| SCO1C3IN | "AMSUCHAN"15  HSBSPOT  "HSBCHAN"5                                 |
!|          |                                                                   |
!| SPITSEQN | SAID  ORBN  201133  SLNM  201000  201132  MJFC  201000  202126    |
!| SPITSEQN | SELV  202000  SOZA  SOLAZI  "INTMS"9                              |
!|          |                                                                   |
!| SITPSEQN | SIID  YEAR  MNTH  DAYS  HOUR  MINU  202131  201138  SECO  201000  |
!| SITPSEQN | 202000  CLATH  CLONH  SAZA  BEARAZ  FOVN                          |
!|          |                                                                   |
!| SCBTSEQN | 201134  CHNM  201000  LOGRCW  ACQF  TMBR                          |
!|          |                                                                   |
!| AMSUSPOT | SIID  YEAR  MNTH  DAYS  HOUR  MINU  202131  201138  SECO  201000  |
!| AMSUSPOT | 202000  CLATH  CLONH  SAZA  BEARAZ  FOVN                          |
!|          |                                                                   |
!| AMSUCHAN | 201134  CHNM  201000  LOGRCW  ACQF  TMBR                          |
!|          |                                                                   |

  implicit none

  character (len=*),  intent(in)  :: filename
  character (len=10), intent(out) :: filedate   ! ccyymmddhh

  integer(i_kind),parameter :: N_MAXCHAN  = 281 ! max nchan of airs-281-subset and amsua-a

  ! variables for BUFR SPITSEQN
  integer(i_kind),parameter :: N_satellitespot_LIST = 25
  type satellitespot_list
     sequence
     real(r_double) :: said       ! Satellite identifier
     real(r_double) :: orbn       ! Orbit number
     real(r_double) :: slnm       ! Scan line number
     real(r_double) :: mjfc       ! Major frame count
     real(r_double) :: selv       ! Height of station
     real(r_double) :: soza       ! Solar zenith angle
     real(r_double) :: solazi     ! Solar azimuth angle
     real(r_double) :: intms(2,9) ! SATELLITE inSTRUMENT TEMPERATURES
  end type satellitespot_list
  real(r_double), dimension(1:N_satellitespot_LIST) :: satellitespot_list_array

  ! variables for BUFR SITPSEQN/AMSUSPOT
  integer(i_kind),parameter :: N_sensorspot_LIST = 12
  type sensorspot_list
     sequence
     real(r_double) :: siid   ! Satellite instruments
     real(r_double) :: year
     real(r_double) :: mnth
     real(r_double) :: days
     real(r_double) :: hour
     real(r_double) :: minu
     real(r_double) :: seco
     real(r_double) :: clath  ! Latitude (high accuracy)
     real(r_double) :: clonh  ! Longitude (high accuracy)
     real(r_double) :: saza   ! Satellite zenith angle
     real(r_double) :: bearaz ! Bearing or azimuth
     real(r_double) :: fovn   ! Field of view number
  end type sensorspot_list
  real(r_double), dimension(1:N_sensorspot_LIST) :: sensorspot_list_array

  ! variables for BUFR SCBTSEQN/AMSUCHAN
  integer(i_kind),parameter :: N_sensorchan_LIST = 4
  type sensorchan_list
     sequence
     real(r_double) :: chnm    ! Channel number
     real(r_double) :: logrcw  ! Log-10 of temperature-radiance central wavenumber
     real(r_double) :: acqf    ! Channel quality flags for ATOVS
     real(r_double) :: tmbr    ! Brightness temperature
  end type sensorchan_list
  real(r_double), dimension(1:N_sensorchan_LIST,1:N_MAXCHAN) :: sensorchan_list_array

  ! Variables for BUFR data
  type(satellitespot_list) :: satellitespot
  type(sensorspot_list)    :: sensorspot
  type(sensorchan_list)    :: sensorchan(N_MAXCHAN)

  character(len=8)  :: subset
  character(len=8)  :: spotname
  character(len=8)  :: channame
  integer(i_kind)   :: ireadmg, ireadsb
  integer(i_kind)   :: nchan
  integer(i_kind)   :: iret

  ! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: iyear, imonth, iday, ihour, imin, isec
  real(r_double)    :: ref_time, obs_time

  ! Other work variables
  integer(i_kind)  :: i, ich
  integer(i_kind)  :: iost, iunit
  integer(i_kind)  :: num_report_infile
  logical          :: decode_airs, decode_amsua


  write(*,*) '--- reading '//trim(filename)//' ---'

  decode_amsua = .false.
  decode_airs  = .false.
  if ( ufo_vars_getindex(inst_list, 'amsua_aqua') > 0 ) decode_amsua = .true.
  if ( ufo_vars_getindex(inst_list, 'airs_aqua')  > 0 ) decode_airs  = .true.

  num_report_infile  = 0

  iunit = 97

  ! open bufr file
  open (unit=iunit, file=trim(filename), &
        iostat=iost, form='unformatted', status='old')
  if (iost /= 0) then
     write(unit=*,fmt='(a,i5,a)') &
        "Error",iost," opening BUFR obs file "//trim(filename)
        return
  end if

  call openbf(iunit,'IN',iunit)
  call datelen(10)
  call readmg(iunit,subset,idate,iret)

  if ( iret /= 0 ) then
     write(unit=*,fmt='(A,I5,A)') &
        "Error",iret," reading BUFR obs file "//trim(filename)
     call closbf(iunit)
     return
  end if
  rewind(iunit)

  write(unit=*,fmt='(1x,a,i10)') trim(filename)//' file date is: ', idate
  write(unit=filedate, fmt='(i10)') idate
  read (filedate(1:10),'(i4,3i2)') iyear, imonth, iday, ihour
  call get_julian_time (iyear,imonth,iday,ihour,0,ref_time)

  if ( .not. associated(rhead) ) then
     nullify ( rhead )
     allocate ( rhead )
     nullify ( rhead%next )
  end if

  if (.not. associated(rlink)) then
     rlink => rhead
  else
     allocate ( rlink%next )
     rlink => rlink%next
     nullify ( rlink%next )
  end if

  msg_loop: do while ( ireadmg(iunit,subset,idate)==0 )

     subset_loop: do while ( ireadsb(iunit)==0 )

        num_report_infile = num_report_infile + 1

        ! Read SPITSEQN
        call ufbseq(iunit,satellitespot_list_array,N_satellitespot_LIST,1,iret,'SPITSEQN')
        satellitespot = satellitespot_list(             &
                        satellitespot_list_array(1), &
                        satellitespot_list_array(2), &
                        satellitespot_list_array(3), &
                        satellitespot_list_array(4), &
                        satellitespot_list_array(5), &
                        satellitespot_list_array(6), &
                        satellitespot_list_array(7), &
                        RESHAPE(satellitespot_list_array(8:25), (/2,9/)) )

        loop_sensor: do i = 1, 2
           if ( i == 1  ) then
              if ( decode_airs ) then
                 spotname = 'SITPSEQN'
                 channame = 'SCBTSEQN'
              else
                 cycle loop_sensor
              end if
           else if ( i == 2 ) then
              if ( decode_amsua ) then
                 spotname = 'AMSUSPOT'
                 channame = 'AMSUCHAN'
              else
                 exit loop_sensor
              end if
           end if

           ! Read SITPSEQN / AMSUSPOT
           call ufbseq(iunit,sensorspot_list_array,N_sensorspot_LIST,1,iret,spotname)

           sensorspot = sensorspot_list( sensorspot_list_array(1),  &
                                         sensorspot_list_array(2),  &
                                         sensorspot_list_array(3),  &
                                         sensorspot_list_array(4),  &
                                         sensorspot_list_array(5),  &
                                         sensorspot_list_array(6),  &
                                         sensorspot_list_array(7),  &
                                         sensorspot_list_array(8),  &
                                         sensorspot_list_array(9),  &
                                         sensorspot_list_array(10), &
                                         sensorspot_list_array(11), &
                                         sensorspot_list_array(12) )

           iyear  = nint(sensorspot%year)
           imonth = nint(sensorspot%mnth)
           iday   = nint(sensorspot%days)
           ihour  = nint(sensorspot%hour)
           imin   = nint(sensorspot%minu)
           isec   = nint(sensorspot%seco)
           if ( iyear  > 1900 .and. iyear  < 3000 .and. &
                imonth >=   1 .and. imonth <=  12 .and. &
                iday   >=   1 .and. iday   <=  31 .and. &
                ihour  >=   0 .and. ihour  <   24 .and. &
                imin   >=   0 .and. imin   <   60 .and. &
                isec   >=   0 .and. isec   <   60 ) then
              write(unit=rlink%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
               iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'
              call get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
              rlink%dhr = (obs_time + (isec/60.0) - ref_time)/60.0
              rlink%gstime = obs_time
           else
              cycle subset_loop
           end if

           if ( abs(sensorspot%clath) > 90.0 .or. abs(sensorspot%clonh) > 360.0 ) cycle subset_loop

           ! Read SCBTSEQN or AMSUCHAN
           call ufbseq(iunit,sensorchan_list_array,N_sensorchan_LIST,N_MAXCHAN,nchan,channame)

           rlink % nchan = nchan
           if ( nchan > 0 ) then
              allocate ( rlink % tb(1:nchan) )
              allocate ( rlink % ch(1:nchan) )
           end if

           call fill_datalink(rlink, missing_r, missing_i)

           do ich = 1 , nchan
              sensorchan(ich) = sensorchan_list( sensorchan_list_array(1,ich), &
                                                 sensorchan_list_array(2,ich), &
                                                 sensorchan_list_array(3,ich), &
                                                 sensorchan_list_array(4,ich) )
              if ( sensorchan(ich) % tmbr < r8bfms ) rlink % tb(ich) = sensorchan(ich) % tmbr
              if ( sensorchan(ich) % chnm < r8bfms ) rlink % ch(ich) = sensorchan(ich) % chnm
           end do

           if ( sensorspot%clath < r8bfms ) rlink % lat  = sensorspot%clath
           if ( sensorspot%clonh < r8bfms ) rlink % lon  = sensorspot%clonh

           if ( sensorspot%fovn      < r8bfms ) rlink % scanpos  = nint( sensorspot%fovn )
           if ( sensorspot%saza      < r8bfms ) rlink % satzen   = sensorspot%saza
           if ( satellitespot%slnm   < r8bfms ) rlink % scanline = nint(satellitespot%slnm)
           if ( sensorspot%bearaz    < r8bfms ) rlink % satazi   = sensorspot%bearaz
           if ( satellitespot%soza   < r8bfms ) rlink % solzen   = satellitespot%soza
           if ( satellitespot%solazi < r8bfms ) rlink % solazi   = satellitespot%solazi

           rlink % satid = nint(satellitespot % said)
           rlink % instid = nint(sensorspot % siid)

           allocate ( rlink%next )
           rlink => rlink%next
           nullify ( rlink%next )

        end do loop_sensor
     end do subset_loop ! ireadsb
  end do msg_loop ! ireadmg

  call closbf(iunit)
  close(iunit)

  write(*,'(1x,a,a,a,i10)') 'num_report_infile ', trim(filename), ' : ', num_report_infile

end subroutine read_airs_colocate_amsua

!--------------------------------------------------------------

subroutine read_iasi (filename, filedate)

!| NC021241 | A61207 | MTYP 021-241 IASI 1C RADIANCES (VARIABLE CHNS) (METOP)   |
!|------------------------------------------------------------------------------|
!| MNEMONIC | SEQUENCE                                                          |
!|----------|-------------------------------------------------------------------|
!| NC021241 | SAID  GCLONG  SIID  SCLF  YEAR  MNTH  DAYS  HOUR  MINU  202131    |
!| NC021241 | 201138  SECO  201000  202000  CLATH  CLONH  SAZA  BEARAZ  SOZA    |
!| NC021241 | SOLAZI  FOVN  ORBN  201133  SLNM  201000  201132  MJFC  201000    |
!| NC021241 | 202126  SELV  202000  QGFQ  QGQI  QGQIL  QGQIR  QGQIS  QGSSQ      |
!| NC021241 | "IASIL1CB"10  (IASICHN)  SIID  AVHCST  "IASIL1CS"7                |
!|          |                                                                   |
!| IASIL1CB | STCH  ENCH  CHSF                                                  |
!| IASICHN  | 201136  CHNM  201000  SCRA                                        |
!| IASIL1CS | YAPCG  ZAPCG  FCPH  "AVHRCHN"6                                    |
!| QGFQ     | 033060 | INDIVIDUAL IASI-SYSTEM QUALITY FLAG                      |
!| STCH     | 025140 | START CHANNEL                                            |
!| ENCH     | 025141 | END CHANNEL                                              |
!| CHSF     | 025142 | CHANNEL SCALE FACTOR                                     |
!| CHNM     | 005042 | CHANNEL NUMBER                                           |
!| SCRA     | 014046 | SCALED IASI RADIANCE                                     |
!|          |                                                                   |
!SCRA | W M**-2 SR**-1 M

   implicit none

   character (len=*),  intent(in)  :: filename
   character (len=10), intent(out) :: filedate  ! ccyymmddhh

   integer(i_kind), parameter :: ntime = 6  ! number of data to read in timestr
   integer(i_kind), parameter :: ninfo = 9  ! number of data to read in infostr
   integer(i_kind), parameter :: nqual = 1  ! number of data to read in qualstr
   integer(i_kind), parameter :: nlalo = 2  ! number of data to read in lalostr
   integer(i_kind), parameter :: nchan_bufr = 616  ! nchan in iasi bufr

   character(len=80) :: timestr, infostr, lalostr, qualstr

   real(r_double), dimension(ntime) :: timedat
   real(r_double), dimension(ninfo) :: infodat
   real(r_double), dimension(nqual) :: qualdat
   real(r_double), dimension(nlalo) :: lalodat
   real(r_double), dimension(2,nchan_bufr) :: data1b8
   real(r_double), dimension(3,10) :: cscale

   character(len=8)  :: subset
   character(len=10) :: cdate

   integer(i_kind) :: iunit, iost, iret, i, j, jstart
   integer(i_kind) :: nchan
   integer(i_kind) :: idate
   integer(i_kind) :: num_report_infile
   integer(i_kind) :: ireadmg, ireadsb

   integer(i_kind) :: iyear, imonth, iday, ihour, imin, isec
   real(r_double)  :: ref_time, obs_time

   ! for scale factors for radiance
   integer(i_kind) :: chan_range1, chan_range2, ichan
   integer(i_kind) :: iscale
   !real(r_double), dimension(nchan_bufr) :: radiance
   real(r_double) :: radiance

   write(*,*) '--- reading '//trim(filename)//' ---'

   timestr = 'YEAR MNTH DAYS HOUR MINU SECO'
   infostr = 'SAID SIID SAZA BEARAZ SOZA SOLAZI FOVN SLNM SELV'
   qualstr = 'QGFQ'
   lalostr = 'CLATH CLONH'

   num_report_infile  = 0

   iunit = 96

   ! open bufr file
   open (unit=iunit, file=trim(filename), &
         iostat=iost, form='unformatted', status='old')
   if (iost /= 0) then
      write(unit=*,fmt='(a,i5,a)') &
         "Error",iost," opening BUFR obs file "//trim(filename)
         return
   end if

   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readmg(iunit,subset,idate,iret)

   if ( iret /= 0 ) then
      write(unit=*,fmt='(A,I5,A)') &
         "Error",iret," reading BUFR obs file "//trim(filename)
      call closbf(iunit)
      return
   end if
   rewind(iunit)

   write(unit=*,fmt='(1x,a,i10)') trim(filename)//' file date is: ', idate
   write(unit=filedate, fmt='(i10)') idate
   read (filedate(1:10),'(i4,3i2)') iyear, imonth, iday, ihour
   call get_julian_time (iyear,imonth,iday,ihour,0,ref_time)

   if ( .not. associated(rhead) ) then
      nullify ( rhead )
      allocate ( rhead )
      nullify ( rhead%next )
   end if

   if ( .not. associated(rlink) ) then
      rlink => rhead
   else
      allocate ( rlink%next )
      rlink => rlink%next
      nullify ( rlink%next )
   end if

   msg_loop: do while (ireadmg(iunit,subset,idate)==0)
!print*,subset
      subset_loop: do while (ireadsb(iunit)==0)

         num_report_infile = num_report_infile + 1

         call ufbint(iunit,timedat,ntime,1,iret,timestr)

         iyear  = nint(timedat(1))
         imonth = nint(timedat(2))
         iday   = nint(timedat(3))
         ihour  = nint(timedat(4))
         imin   = nint(timedat(5))
         isec   = min(59, nint(timedat(6))) ! raw BUFR data that has SECO = 60.0 SECOND
                                            ! that was probably rounded from 59.x seconds
                                            ! reset isec to 59 rather than advancing one minute
         if ( iyear  > 1900 .and. iyear  < 3000 .and. &
              imonth >=   1 .and. imonth <=  12 .and. &
              iday   >=   1 .and. iday   <=  31 .and. &
              ihour  >=   0 .and. ihour  <   24 .and. &
              imin   >=   0 .and. imin   <   60 .and. &
              isec   >=   0 .and. isec   <   60 ) then
            write(unit=rlink%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
               iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'
            call get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
            rlink%dhr = (obs_time + (isec/60.0) - ref_time)/60.0
            rlink%gstime = obs_time
         else
            cycle subset_loop
         end if

         call ufbint(iunit,lalodat,nlalo,1,iret,lalostr)
         if ( abs(lalodat(1)) > 90.0 .or. abs(lalodat(2)) > 360.0 ) cycle subset_loop

         call ufbint(iunit,qualdat,nqual,1,iret,qualstr)  ! QGFQ: GqisFlagQual 0: good, 1: bad
         if ( nint(qualdat(1)) /= 0 ) cycle subset_loop

         call ufbint(iunit,infodat,ninfo,1,iret,infostr)  ! SAID SIID SAZA BEARAZ SOZA SOLAZI FOVN SLNM SELV
         if ( iret /= 1 ) cycle subset_loop

         call ufbseq(iunit,cscale,3,10,iret,'IASIL1CB')  ! STCH ENCH CHSF
         if ( iret /= 10 ) cycle subset_loop

         call ufbseq(iunit,data1b8,2,nchan_bufr,iret,'IASICHN')  ! CHNM SCRA
         if ( iret /= nchan_bufr ) cycle subset_loop

         nchan = nchan_bufr
         rlink % nchan = nchan_bufr
         allocate ( rlink % tb(nchan) )   ! radiance for now
         allocate ( rlink % ch(nchan) )   ! channel number

         call fill_datalink(rlink, missing_r, missing_i)

         if ( lalodat(1) < r8bfms ) rlink % lat = lalodat(1)
         if ( lalodat(2) < r8bfms ) rlink % lon = lalodat(2)

         rlink % satid  = nint(infodat(1))  ! SAID satellite identifier
         rlink % instid = nint(infodat(2))  ! SIID instrument identifier

         if ( infodat(3)  < r8bfms ) rlink % satzen  = infodat(3)        ! SAZA satellite zenith angle (degree)
         if ( infodat(4)  < r8bfms ) rlink % satazi  = infodat(4)        ! BEARAZ satellite azimuth (degree true)
         if ( infodat(5)  < r8bfms ) rlink % solzen  = infodat(5)        ! SOZA solar zenith angle (degree)
         if ( infodat(6)  < r8bfms ) rlink % solazi  = infodat(6)        ! SOLAZI solar azimuth (degree true)
         if ( infodat(7)  < r8bfms ) rlink % scanpos = nint(infodat(7))  ! FOVN field of view number
         if ( infodat(8)  < r8bfms ) rlink % scanline = nint(infodat(8)) ! SLNM scan line number
         if ( infodat(9)  < r8bfms ) rlink % elv = infodat(9)            ! SELV height of station (eg. 828400.0 m)

         jstart = 1
         chan_loop: do i = 1, nchan
            if ( data1b8(1,i) > r8bfms .or. data1b8(2,i) > r8bfms ) cycle chan_loop
            ichan = nint(data1b8(1,i))
            radiance = data1b8(2,i)
            ! scale factors are stored in 10 channel groups
            iscale = 0  ! initialize
            range_loop: do j = jstart, 10
               chan_range1 = cscale(1,j)
               chan_range2 = cscale(2,j)
               if ( ichan >= chan_range1 .and. ichan <= chan_range2 ) then
                  if ( cscale(3,j) < r8bfms ) iscale = nint(cscale(3,j))
                  jstart = j
                  exit range_loop
               end if
            end do range_loop
            if ( iscale /= 0 ) iscale = -1*(iscale-5)
            radiance = radiance * 10.0**iscale
            rlink % tb(i) = radiance
            rlink % ch(i) = ichan
         end do chan_loop

         allocate ( rlink%next )
         rlink => rlink%next
         nullify ( rlink%next )

      end do subset_loop ! ireadsb
   end do msg_loop ! ireadmg

   call closbf(iunit)
   close(iunit)

   write(*,'(1x,a,a,a,i10)') 'num_report_infile ', trim(filename), ' : ', num_report_infile

end subroutine read_iasi

!--------------------------------------------------------------

subroutine read_cris (filename, filedate)

!| NC021202 | A10060 | MTYP 021-202 CrIS APODIZED RADIANCE DATA (399 CHANNEL)   |
!|------------------------------------------------------------------------------|
!| MNEMONIC | SEQUENCE                                                          |
!|----------|-------------------------------------------------------------------|
!| NC021202 | SAID  OGCE  SIID  SCLF  YYMMDD  HHMM  207003  SECO  207000        |
!| NC021202 | LOCPLAT  LTLONH  SAZA  BEARAZ  SOZA  SOLAZI  STKO  201133  SLNM   |
!| NC021202 | 201000  FORN  FOVN  ORBN  HOLS  201129  HMSL  201000  202127      |
!| NC021202 | 201125  ALFR  201000  202000  LSQL  TOCC  HOCT  RDTF  NSQF        |
!| NC021202 | "BCFQFSQ"3  TOBD  NGQI  QMRKH  (CRCHN)                            |

!| NC021206 | A10198 | MTYP 021-206 CrIS FULL SPCTRL RADIANCE  (431 CHN SUBSET  |
!|------------------------------------------------------------------------------|
!| MNEMONIC | SEQUENCE                                                          |
!|----------|-------------------------------------------------------------------|
!| NC021206 | SAID  OGCE  SIID  SCLF  YYMMDD  HHMM  207003  SECO  207000        |
!| NC021206 | LOCPLAT  LTLONH  SAZA  BEARAZ  SOZA  SOLAZI  STKO  201133  SLNM   |
!| NC021206 | 201000  FORN  FOVN  ORBN  HOLS  201129  HMSL  201000  202127      |
!| NC021206 | 201125  ALFR  201000  202000  LSQL  TOCC  HOCT  RDTF  NSQF        |
!| NC021206 | "BCFQFSQ"3  TOBD  NGQI  QMRKH  (CRCHNM)  MTYP  TOBD  {GCRCHN}     |
!| NC021206 | TOBD  SIID  "CRISCS"7                                             |
!|          |                                                                   |
!| MTYP     | 002141 | MEASUREMENT TYPE                                         |
!| CRCHN    | 350201 | NPP CrIS CHANNEL DATA                                    |
!| CRCHNM   | 350216 | NPP CrIS CHANNEL DATA EXTENDED                           |
!|          |                                                                   |
!| YYMMDD   | YEAR  MNTH  DAYS                                                  |
!| HHMM     | HOUR  MINU                                                        |
!| LTLONH   | CLATH  CLONH                                                      |
!| CRCHN    | CHNM  SRAD                                                        |
!| CRCHNM   | CHNM  SRAD                                                        |
! SRAD | W M**-2 SR**-1 CM


   implicit none

   character (len=*),  intent(in)  :: filename
   character (len=10), intent(out) :: filedate  ! ccyymmddhh

   integer(i_kind), parameter :: ntime = 6  ! number of data to read in timestr
   integer(i_kind), parameter :: ninfo = 10 ! number of data to read in infostr
   integer(i_kind), parameter :: nlalo = 2  ! number of data to read in lalostr

   integer(i_kind) :: nchan_bufr  ! nchan in cris bufr, either 399 or 431

   character(len=80) :: timestr, infostr, lalostr

   real(r_double), dimension(ntime) :: timedat
   real(r_double), dimension(ninfo) :: infodat
   real(r_double), dimension(nlalo) :: lalodat
   real(r_double), allocatable :: data1b8(:,:)

   character(len=8)  :: subset
   character(len=10) :: cdate

   integer(i_kind) :: iunit, iost, iret, i
   integer(i_kind) :: nchan
   integer(i_kind) :: idate
   integer(i_kind) :: num_report_infile
   integer(i_kind) :: ireadmg, ireadsb

   integer(i_kind) :: iyear, imonth, iday, ihour, imin, isec
   real(r_double)  :: ref_time, obs_time

   character(len=3) :: cmtyp
   real(r_double) :: r8mtyp(1)
   equivalence (r8mtyp, cmtyp)

   write(*,*) '--- reading '//trim(filename)//' ---'

   timestr = 'YEAR MNTH DAYS HOUR MINU SECO'
   infostr = 'SAID SIID SAZA BEARAZ SOZA SOLAZI SLNM FORN FOVN HMSL'
   lalostr = 'CLATH CLONH'

   num_report_infile  = 0

   iunit = 96

   ! open bufr file
   open (unit=iunit, file=trim(filename), &
         iostat=iost, form='unformatted', status='old')
   if (iost /= 0) then
      write(unit=*,fmt='(a,i5,a)') &
         "Error",iost," opening BUFR obs file "//trim(filename)
         return
   end if

   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readmg(iunit,subset,idate,iret)

   if ( iret /= 0 ) then
      write(unit=*,fmt='(A,I5,A)') &
         "Error",iret," reading BUFR obs file "//trim(filename)
      call closbf(iunit)
      return
   end if
   rewind(iunit)

   if ( subset == 'NC021202' ) then
      nchan_bufr = 399
   else if ( subset == 'NC021206' ) then
      nchan_bufr = 431
   else
      write(unit=*,fmt='(A,I5,A)') &
         "Error: "//subset//" from "//trim(filename)//" not implemented"
      call closbf(iunit)
      return
   end if
   allocate (data1b8(2,nchan_bufr))

   write(unit=*,fmt='(1x,a,i10)') trim(filename)//' file date is: ', idate
   write(unit=filedate, fmt='(i10)') idate
   read (filedate(1:10),'(i4,3i2)') iyear, imonth, iday, ihour
   call get_julian_time (iyear,imonth,iday,ihour,0,ref_time)

   if ( .not. associated(rhead) ) then
      nullify ( rhead )
      allocate ( rhead )
      nullify ( rhead%next )
   end if

   if ( .not. associated(rlink) ) then
      rlink => rhead
   else
      allocate ( rlink%next )
      rlink => rlink%next
      nullify ( rlink%next )
   end if

   msg_loop: do while (ireadmg(iunit,subset,idate)==0)
!print*,subset
      subset_loop: do while (ireadsb(iunit)==0)

         num_report_infile = num_report_infile + 1

         call ufbint(iunit,timedat,ntime,1,iret,timestr)

         iyear  = nint(timedat(1))
         imonth = nint(timedat(2))
         iday   = nint(timedat(3))
         ihour  = nint(timedat(4))
         imin   = nint(timedat(5))
         isec   = min(59, nint(timedat(6))) ! raw BUFR data that has SECO = 60.0 SECOND
                                            ! that was probably rounded from 59.x seconds
                                            ! reset isec to 59 rather than advancing one minute
         if ( iyear  > 1900 .and. iyear  < 3000 .and. &
              imonth >=   1 .and. imonth <=  12 .and. &
              iday   >=   1 .and. iday   <=  31 .and. &
              ihour  >=   0 .and. ihour  <   24 .and. &
              imin   >=   0 .and. imin   <   60 .and. &
              isec   >=   0 .and. isec   <   60 ) then
            write(unit=rlink%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
               iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'
            call get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
            rlink%dhr = (obs_time + (isec/60.0) - ref_time)/60.0
            rlink%gstime = obs_time
         else
            cycle subset_loop
         end if

         call ufbint(iunit,lalodat,nlalo,1,iret,lalostr)
         if ( abs(lalodat(1)) > 90.0 .or. abs(lalodat(2)) > 360.0 ) cycle subset_loop

         call ufbint(iunit,infodat,ninfo,1,iret,infostr)  ! SAID SIID SAZA BEARAZ SOZA SOLAZI SLNM FORN FOVN HMSL
         if ( iret /= 1 ) cycle subset_loop

         if ( subset == 'NC021202' ) then
            call ufbseq(iunit,data1b8,2,nchan_bufr,iret,'CRCHN')
         else if ( subset == 'NC021206' ) then
            call ufbint(iunit,r8mtyp,1,1,iret,'MTYP')
            if ( iret /= 1 ) cycle subset_loop
            if ( cmtyp == 'FSR' ) then
               call ufbseq(iunit,data1b8,2,nchan_bufr,iret,'CRCHNM')
            end if
         end if
         if ( iret /= nchan_bufr ) cycle subset_loop

         nchan = nchan_bufr
         rlink % nchan = nchan_bufr
         allocate ( rlink % tb(nchan) )   ! radiance for now
         allocate ( rlink % ch(nchan) )   ! channel number

         call fill_datalink(rlink, missing_r, missing_i)

         if ( lalodat(1) < r8bfms ) rlink % lat = lalodat(1)
         if ( lalodat(2) < r8bfms ) rlink % lon = lalodat(2)

         rlink % satid  = nint(infodat(1))  ! SAID satellite identifier
         rlink % instid = nint(infodat(2))  ! SIID instrument identifier

         if ( infodat(3)  < r8bfms ) rlink % satzen  = infodat(3)        ! SAZA satellite zenith angle (degree)
         if ( infodat(4)  < r8bfms ) rlink % satazi  = infodat(4)        ! BEARAZ satellite azimuth (degree true)
         if ( infodat(5)  < r8bfms ) rlink % solzen  = infodat(5)        ! SOZA solar zenith angle (degree)
         if ( infodat(6)  < r8bfms ) rlink % solazi  = infodat(6)        ! SOLAZI solar azimuth (degree true)
         if ( infodat(7)  < r8bfms ) rlink % scanline = nint(infodat(7)) ! SLNM scan line number
         if ( infodat(8)  < r8bfms ) rlink % scanpos = nint(infodat(8))  ! FORN field of regard number 1-30
         !if ( infodat(9)  < r8bfms ) rlink % scanpos = nint(infodat(9))  ! FOVN field of view number 1-9
         if ( infodat(10) < r8bfms ) rlink % elv = infodat(10)           ! HMSL height or altitude (eg. 836410.0 m)

         chan_loop: do i = 1, nchan
            if ( data1b8(1,i) > r8bfms .or. data1b8(2,i) > r8bfms ) cycle chan_loop
            rlink % ch(i) = nint(data1b8(1,i))
            rlink % tb(i) = data1b8(2,i) * 1000.0  ! radiance for now
         end do chan_loop

         allocate ( rlink%next )
         rlink => rlink%next
         nullify ( rlink%next )

      end do subset_loop ! ireadsb
   end do msg_loop ! ireadmg

   deallocate (data1b8)
   call closbf(iunit)
   close(iunit)

   write(*,'(1x,a,a,a,i10)') 'num_report_infile ', trim(filename), ' : ', num_report_infile

end subroutine read_cris

!--------------------------------------------------------------

subroutine sort_obs_radiance(filedate, nfgat)

   implicit none

   character(len=*), intent(in) :: filedate
   integer(i_kind),  intent(in) :: nfgat

   integer(i_kind)                   :: i, iv, k, ii
   integer(i_kind)                   :: ityp, irec, itim
   integer(i_kind), dimension(ninst,nfgat) :: nrecs
   integer(i_kind), dimension(ninst,nfgat) :: nlocs
   integer(i_kind), dimension(ninst,nfgat) :: iloc
   integer(i_kind), dimension(ninst) :: nvars
   character(len=nstring)            :: satellite
   character(len=nstring)            :: sensor
   character(len=14) :: cdate_min, cdate_max
   real(r_double) :: time_slots(0:nfgat)

   call da_advance_time(filedate, dtime_min, cdate_min)
   call da_advance_time(filedate, dtime_max, cdate_max)
   call da_get_time_slots(nfgat, cdate_min, cdate_max, time_slots)

   nrecs(:,:) = 0
   nlocs(:,:) = 0
   nvars(:) = 0

   write(*,*) '--- sorting radiance obs...'

   ! set inst type from satellite id and sensor id
   ! and count the numbers
   rlink => rhead
   set_inst_loop: do while ( associated(rlink) )

      if (  rlink%nchan < 1 ) then
         rlink => rlink%next
         cycle set_inst_loop
      end if

      ! determine time_slot index
      do i = 1, nfgat
         if ( rlink%gstime >= time_slots(i-1) .and.  &
              rlink%gstime <= time_slots(i) ) then
             exit
         end if
      end do
      rlink%ifgat = i

      call set_name_satellite(rlink%satid,  satellite)
      call set_name_sensor   (rlink%instid, sensor)
      rlink % inst = trim(sensor)//'_'//trim(satellite)
!print*, rlink%inst, rlink % nchan

      ! find index of inst in inst_list
      rlink%inst_idx = ufo_vars_getindex(inst_list, rlink%inst)
      if ( rlink % inst_idx > 0 ) then
         ! obtype assigned, advance ob counts
         nrecs(rlink%inst_idx,rlink%ifgat) = nrecs(rlink%inst_idx,rlink%ifgat) + 1
         nlocs(rlink%inst_idx,rlink%ifgat) = nlocs(rlink%inst_idx,rlink%ifgat) + 1
         nvars(rlink%inst_idx) = rlink % nchan
      end if

      rlink => rlink%next
   end do set_inst_loop

   do ii = 1, nfgat
      if ( nfgat > 1 ) then
         write(*,'(a)') '-----------'
         write(*,'(1x,a,i3)') 'time', ii
         write(*,'(a)') '-----------'
      end if
      !write(*,*) 'num_report_decoded = ', sum(nrecs(:,ii))
      write(*,'(1x,20x,a10)') 'nlocs'
      do i = 1, ninst
         !write(*,'(1x,a20,2i10)') inst_list(i), nrecs(i,ii), nlocs(i,ii)
         write(*,'(1x,a20,i10)') inst_list(i), nlocs(i,ii)
      end do
   end do

   ! allocate data arrays with the just counted numbers
   allocate (xdata(ninst,nfgat))
   do ii = 1, nfgat
   do i = 1, ninst
      xdata(i,ii) % nrecs = nrecs(i,ii)
      xdata(i,ii) % nlocs = nlocs(i,ii)
      xdata(i,ii) % nvars = nvars(i)

      if ( nlocs(i,ii) > 0 ) then
         allocate (xdata(i,ii)%xinfo_float(nlocs(i,ii), nvar_info))
         allocate (xdata(i,ii)%xinfo_int  (nlocs(i,ii), nvar_info))
         allocate (xdata(i,ii)%xinfo_char (nlocs(i,ii), nvar_info))
         allocate (xdata(i,ii)%xseninfo_float(nlocs(i,ii), nsen_info))
         allocate (xdata(i,ii)%xseninfo_int  (nvars(i), nsen_info))
         xdata(i,ii)%xinfo_float   (:,:) = missing_r
         xdata(i,ii)%xinfo_int     (:,:) = missing_i
         xdata(i,ii)%xinfo_char    (:,:) = ''
         xdata(i,ii)%xseninfo_float(:,:) = missing_r
         xdata(i,ii)%xseninfo_int  (:,:) = missing_i
         if ( nvars(i) > 0 ) then
            allocate (xdata(i,ii)%xfield(nlocs(i,ii), nvars(i)))
            xdata(i,ii)%xfield(:,:)%val = missing_r
            xdata(i,ii)%xfield(:,:)%qm  = missing_i
            xdata(i,ii)%xfield(:,:)%err = missing_r
            allocate (xdata(i,ii)%var_idx(nvars(i)))
            do iv = 1, nvars(i)
               xdata(i,ii)%var_idx(iv) = iv
            end do
         end if
      end if
   end do ! ninst
   end do ! nfgat

   ! transfer data from rlink to xdata

   iloc(:,:) = 0
   irec = 0

   rlink => rhead
   reports: do while ( associated(rlink) )
      irec = irec + 1
      ityp = rlink%inst_idx
      itim = rlink%ifgat
      if ( ityp < 0 ) then
         rlink => rlink%next
         cycle reports
      end if
      if (  rlink%nchan < 1 ) then
         rlink => rlink%next
         cycle reports
      end if

      iloc(ityp,itim) = iloc(ityp,itim) + 1

      do i = 1, nvar_info
         if ( type_var_info(i) == nf90_int ) then
            if ( trim(name_var_info(i)) == 'record_number' ) then
               xdata(ityp,itim)%xinfo_int(iloc(ityp,itim),i) = irec
            end if
         else if ( type_var_info(i) == nf90_float ) then
            if ( name_var_info(i) == 'time' ) then
               xdata(ityp,itim)%xinfo_float(iloc(ityp,itim),i) = rlink%dhr
            else if ( trim(name_var_info(i)) == 'station_elevation' ) then
               xdata(ityp,itim)%xinfo_float(iloc(ityp,itim),i) = rlink%elv
            else if ( trim(name_var_info(i)) == 'latitude' ) then
               xdata(ityp,itim)%xinfo_float(iloc(ityp,itim),i) = rlink%lat
            else if ( trim(name_var_info(i)) == 'longitude' ) then
               xdata(ityp,itim)%xinfo_float(iloc(ityp,itim),i) = rlink%lon
            end if
         else if ( type_var_info(i) == nf90_char ) then
            if ( trim(name_var_info(i)) == 'datetime' ) then
               xdata(ityp,itim)%xinfo_char(iloc(ityp,itim),i) = rlink%datetime
            else if ( trim(name_var_info(i)) == 'station_id' ) then
               xdata(ityp,itim)%xinfo_char(iloc(ityp,itim),i) = rlink%inst
            end if
         end if
      end do

      do i = 1, nsen_info
         if ( type_sen_info(i) == nf90_float ) then
            if ( trim(name_sen_info(i)) == 'scan_position' ) then
               xdata(ityp,itim)%xseninfo_float(iloc(ityp,itim),i) = rlink%scanpos
            else if ( trim(name_sen_info(i)) == 'sensor_zenith_angle' ) then
               xdata(ityp,itim)%xseninfo_float(iloc(ityp,itim),i) = rlink%satzen
            else if ( trim(name_sen_info(i)) == 'sensor_azimuth_angle' ) then
               xdata(ityp,itim)%xseninfo_float(iloc(ityp,itim),i) = rlink%satazi
            else if ( trim(name_sen_info(i)) == 'solar_zenith_angle' ) then
               xdata(ityp,itim)%xseninfo_float(iloc(ityp,itim),i) = rlink%solzen
            else if ( trim(name_sen_info(i)) == 'solar_azimuth_angle' ) then
               xdata(ityp,itim)%xseninfo_float(iloc(ityp,itim),i) = rlink%solazi
            else if ( trim(name_sen_info(i)) == 'sensor_view_angle' ) then
               call calc_sensor_view_angle(trim(rlink%inst), rlink%scanpos, xdata(ityp,itim)%xseninfo_float(iloc(ityp,itim),i))
            end if
!         else if ( type_sen_info(i) == nf90_int ) then
!         else if ( type_sen_info(i) == nf90_char ) then
         end if
      end do

      iv = ufo_vars_getindex(name_sen_info, 'sensor_channel')
      xdata(ityp,itim)%xseninfo_int(:,iv) = rlink%ch(:)

      do i = 1, nvars(ityp)
         xdata(ityp,itim)%xfield(iloc(ityp,itim),i)%val = rlink%tb(i)
         !xdata(ityp,itim)%xfield(iloc(ityp,itim),i)%err = 1.0
         call set_brit_obserr(trim(rlink%inst), i, xdata(ityp,itim)%xfield(iloc(ityp,itim),i)%err)
         xdata(ityp,itim)%xfield(iloc(ityp,itim),i)%qm  = 0
      end do
      rlink => rlink%next
   end do reports

   ! done with rlink
   ! release the linked list
   rlink => rhead
   do while ( associated(rlink) )
      rhead => rlink%next
      if ( allocated (rlink%tb) ) deallocate (rlink%tb)
      if ( allocated (rlink%ch) ) deallocate (rlink%ch)
      if ( associated (rlink) ) deallocate (rlink)
      rlink => rhead
   end do
   nullify (rhead)

end subroutine sort_obs_radiance

!--------------------------------------------------------------

subroutine fill_datalink (datalink, rfill, ifill)

   implicit none

   type (datalink_radiance), intent(inout) :: datalink
   real(r_kind),             intent(in)    :: rfill    ! fill value in real
   integer(i_kind),          intent(in)    :: ifill    ! fill value in integer

   integer(i_kind) :: i

   !datalink % datetime = ''
   datalink % lat      = rfill
   datalink % lon      = rfill
   datalink % satid    = ifill
   datalink % instid   = ifill
   datalink % scanpos  = rfill
   datalink % landsea  = rfill
   datalink % satzen   = rfill
   datalink % satazi   = rfill
   datalink % solzen   = rfill
   datalink % solazi   = rfill
   datalink % elv      = rfill
   if ( allocated (datalink % tb) ) then
      datalink % tb(:) = rfill
   end if
   if ( allocated (datalink % ch) ) then
      datalink % ch(:) = ifill
   end if

end subroutine fill_datalink

subroutine calc_sensor_view_angle(name_inst, ifov, view_angle)

! calculate sensor view angle from given scan position (field of view number)

   implicit none

   character(len=*), intent(in)  :: name_inst  ! instrument name eg. amsua_n15
   integer(i_kind),  intent(in)  :: ifov       ! field of view number
   real(r_kind),     intent(out) :: view_angle ! sensor view angle

   integer(i_kind) :: idx
   real(r_kind)    :: start, step

   view_angle = missing_r

   idx = index(name_inst, '_')
   select case ( name_inst(1:idx-1) )
      case ( 'amsua' )
         start  = -48.0_r_kind - 1.0_r_kind/3.0_r_kind
         step = 3.0_r_kind + 1.0_r_kind/3.0_r_kind
      case ( 'amsub' )
         start  = -48.95_r_kind
         step   = 1.1_r_kind
      case ( 'mhs' )
         start  = -445.0_r_kind/9.0_r_kind
         step   = 10.0_r_kind/9.0_r_kind
      case ( 'atms' )
         start  = -52.725_r_kind
         step   = 1.11_r_kind
      case ( 'iasi' )
         start  = -50.0_r_kind
         step   = 5.0_r_kind/6.0_r_kind
      case ( 'cris' )
         start  = -51.615_r_kind
         step   = 3.33_r_kind
      case default
         return
   end select

   view_angle = start + float(ifov-1) * step

end subroutine calc_sensor_view_angle

subroutine radiance_to_temperature(ninst, nfgat)

implicit none
integer(i_kind),  intent(in) :: ninst ! first dim of xdata
integer(i_kind),  intent(in) :: nfgat ! second dim of xdata
real(r_double), allocatable :: planck_c1(:)
real(r_double), allocatable :: planck_c2(:)
real(r_double), allocatable :: band_c1(:)
real(r_double), allocatable :: band_c2(:)
integer(i_kind) :: ierr
integer(i_kind) :: i, ii, iloc, ichan, nchan, nlocs
real(r_double) :: radiance
real(r_double) :: effective_temperature
real(r_double) :: temperature

fgat_loop: do ii = 1, nfgat
  inst_loop: do i = 1, ninst

    if ( trim(inst_list(i)) /= 'cris_npp' .and. &
         trim(inst_list(i)) /= 'cris_n20' .and. &
         trim(inst_list(i)) /= 'iasi_metop-a' .and. &
         trim(inst_list(i)) /= 'iasi_metop-b' .and. &
         trim(inst_list(i)) /= 'iasi_metop-c' ) then
       cycle inst_loop
    end if

    nlocs = xdata(i,ii) % nlocs
    if ( nlocs <= 0 ) cycle inst_loop

    nchan = xdata(i,ii) % nvars
    if ( nchan <= 0 ) cycle inst_loop

    allocate(planck_c1(nchan))
    allocate(planck_c2(nchan))
    allocate(band_c1(nchan))
    allocate(band_c2(nchan))

    call read_spc(trim(inst_list(i)), nchan, planck_c1, planck_c2, band_c1, band_c2, ierr)
    if ( ierr /= 0 ) then
      deallocate(planck_c1)
      deallocate(planck_c2)
      deallocate(band_c1)
      deallocate(band_c2)
      cycle inst_loop
    end if

    write(*,*) '--- converting radiance to brightness temperature... '
    do ichan = 1, nchan
      do iloc = 1, nlocs
        radiance = xdata(i,ii)%xfield(iloc,ichan)%val
        if ( radiance <= 0.0 ) cycle
        effective_temperature = planck_c2(ichan)  / &
                                LOG( ( planck_c1(ichan) / radiance ) + 1.0_r_double )
        temperature = ( effective_temperature - band_c1(ichan) ) / &
                      band_c2(ichan)
        xdata(i,ii)%xfield(iloc,ichan)%val = temperature
      end do
    end do

    deallocate(planck_c1)
    deallocate(planck_c2)
    deallocate(band_c1)
    deallocate(band_c2)

  end do inst_loop
end do fgat_loop

end subroutine radiance_to_temperature

subroutine read_spc(inst_id, nchan, planck_c1, planck_c2, band_c1, band_c2, iret)

implicit none

character(len=*), intent(in) :: inst_id
integer(i_kind), intent(in) :: nchan
real(r_double), intent(out) :: planck_c1(nchan)
real(r_double), intent(out) :: planck_c2(nchan)
real(r_double), intent(out) :: band_c1(nchan)
real(r_double), intent(out) :: band_c2(nchan)
integer(i_kind), intent(out) :: iret

integer(i_kind) :: sensor_channel(nchan)
integer(i_kind) :: polarization(nchan)
integer(i_kind) :: channel_flag(nchan)
real(r_double) :: frequency(nchan)
real(r_double) :: wavenumber(nchan)

character(len=StrLen) :: coefdir, coefname
logical :: fexist
integer(i_kind) :: fid, status, magic_number, i
integer(i_kind) :: sensor_type
integer(i_kind) :: release, version, nchannel, nfov, wmo_satellite_id, wmo_sensor_id
character(len=20) :: sensor_id

iret = 0
fid = 11
coefdir = '.'
coefname = trim(inst_id)//'.SpcCoeff.bin'
inquire(file=trim(coefdir)//'/'//trim(coefname), exist=fexist)
if ( .not. fexist ) then
   write(*,*) 'Warning: SpcCoeff ', trim(coefdir)//'/'//trim(coefname), &
      ' not found for converting radiance to brightness temperature...'
   iret = -1
   return
end if

write(*,*) '--- reading ', trim(coefdir)//'/'//trim(coefname)

open(fid, file=trim(coefname), access='sequential', form='unformatted', action='read', status='old')
read(fid,iostat=status) magic_number
!write(0,*) magic_number
read(fid,iostat=status) release, version
!write(0,*) release, version
read(fid,iostat=status) nchannel, nfov
!write(0,*) nchannel, nfov
if ( nchannel /= nchan ) then
   iret = -2
   write(*,*) 'mismatch nchannel ', nchannel, nchan
   close(fid)
   return
end if
read(fid,iostat=status) sensor_id, sensor_type, wmo_satellite_id, wmo_sensor_id
!write(0,*) sensor_id, sensor_type, wmo_satellite_id, wmo_sensor_id
read(fid,iostat=status) sensor_channel, &
                        polarization, &
                        channel_flag, &
                        frequency, &
                        wavenumber, &
                        planck_c1, &
                        planck_c2, &
                        band_c1, &
                        band_c2
iret = status
close(fid)

end subroutine read_spc

end module radiance_mod
