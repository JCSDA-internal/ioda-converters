module radiance_mod

use kinds, only: r_kind,i_kind,r_double
use define_mod, only: missing_r, missing_i, nstring, ndatetime, &
   ninst, inst_list, set_name_satellite, set_name_sensor, xdata, name_sen_info, &
   nvar_info, name_var_info, type_var_info, nsen_info, type_sen_info, set_brit_obserr
use ufo_vars_mod, only: ufo_vars_getindex
use netcdf, only: nf90_float, nf90_int, nf90_char

implicit none
private
public  :: read_amsua_amsub_mhs
public  :: read_airs_colocate_amsua
public  :: sort_obs_radiance

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

subroutine sort_obs_radiance

   implicit none

   integer(i_kind)                   :: i, iv, k
   integer(i_kind)                   :: ityp, irec
   integer(i_kind), dimension(ninst) :: nrecs
   integer(i_kind), dimension(ninst) :: nlocs
   integer(i_kind), dimension(ninst) :: nvars
   integer(i_kind), dimension(ninst) :: iloc
   character(len=nstring)            :: satellite
   character(len=nstring)            :: sensor

   nrecs(:) = 0
   nlocs(:) = 0
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

      call set_name_satellite(rlink%satid,  satellite)
      call set_name_sensor   (rlink%instid, sensor)
      rlink % inst = trim(sensor)//'_'//trim(satellite)
!print*, rlink%inst, rlink % nchan

      ! find index of inst in inst_list
      rlink%inst_idx = ufo_vars_getindex(inst_list, rlink%inst)
      if ( rlink % inst_idx > 0 ) then
         ! obtype assigned, advance ob counts
         nrecs(rlink%inst_idx) = nrecs(rlink%inst_idx) + 1
         nlocs(rlink%inst_idx) = nlocs(rlink%inst_idx) + 1
         nvars(rlink%inst_idx) = rlink % nchan
      end if

      rlink => rlink%next
   end do set_inst_loop

   !write(*,*) 'num_report_decoded = ', sum(nrecs(:))
   write(*,'(1x,20x,a10)') 'nlocs'
   do i = 1, ninst
      !write(*,'(1x,a20,2i10)') inst_list(i), nrecs(i), nlocs(i)
      write(*,'(1x,a20,i10)') inst_list(i), nlocs(i)
   end do

   ! allocate data arrays with the just counted numbers
   allocate (xdata(ninst))
   do i = 1, ninst
      xdata(i) % nrecs = nrecs(i)
      xdata(i) % nlocs = nlocs(i)
      xdata(i) % nvars = nvars(i)

      if ( nlocs(i) > 0 ) then
         allocate (xdata(i)%xinfo_float(nlocs(i), nvar_info))
         allocate (xdata(i)%xinfo_int  (nlocs(i), nvar_info))
         allocate (xdata(i)%xinfo_char (nlocs(i), nvar_info))
         allocate (xdata(i)%xseninfo_float(nlocs(i), nsen_info))
         allocate (xdata(i)%xseninfo_int  (nvars(i), nsen_info))
         xdata(i)%xinfo_float   (:,:) = missing_r
         xdata(i)%xinfo_int     (:,:) = missing_i
         xdata(i)%xinfo_char    (:,:) = ''
         xdata(i)%xseninfo_float(:,:) = missing_r
         xdata(i)%xseninfo_int  (:,:) = missing_i
         if ( nvars(i) > 0 ) then
            allocate (xdata(i)%xfield(nlocs(i), nvars(i)))
            xdata(i)%xfield(:,:)%val = missing_r
            xdata(i)%xfield(:,:)%qm  = missing_i
            xdata(i)%xfield(:,:)%err = missing_r
            allocate (xdata(i)%var_idx(nvars(i)))
            do iv = 1, nvars(i)
               xdata(i)%var_idx(iv) = iv
            end do
         end if
      end if
   end do

   ! transfer data from rlink to xdata

   iloc(:) = 0
   irec    = 0

   rlink => rhead
   reports: do while ( associated(rlink) )
      irec = irec + 1
      ityp = rlink%inst_idx
      if ( ityp < 0 ) then
         rlink => rlink%next
         cycle reports
      end if
      if (  rlink%nchan < 1 ) then
         rlink => rlink%next
         cycle reports
      end if

      iloc(ityp) = iloc(ityp) + 1

      do i = 1, nvar_info
         if ( type_var_info(i) == nf90_int ) then
            if ( trim(name_var_info(i)) == 'record_number' ) then
               xdata(ityp)%xinfo_int(iloc(ityp),i) = irec
            end if
         else if ( type_var_info(i) == nf90_float ) then
            if ( name_var_info(i) == 'time' ) then
               xdata(ityp)%xinfo_float(iloc(ityp),i) = rlink%dhr
            else if ( trim(name_var_info(i)) == 'station_elevation' ) then
               xdata(ityp)%xinfo_float(iloc(ityp),i) = rlink%elv
            else if ( trim(name_var_info(i)) == 'latitude' ) then
               xdata(ityp)%xinfo_float(iloc(ityp),i) = rlink%lat
            else if ( trim(name_var_info(i)) == 'longitude' ) then
               xdata(ityp)%xinfo_float(iloc(ityp),i) = rlink%lon
            end if
         else if ( type_var_info(i) == nf90_char ) then
            if ( trim(name_var_info(i)) == 'datetime' ) then
               xdata(ityp)%xinfo_char(iloc(ityp),i) = rlink%datetime
            else if ( trim(name_var_info(i)) == 'station_id' ) then
               xdata(ityp)%xinfo_char(iloc(ityp),i) = rlink%inst
            end if
         end if
      end do

      do i = 1, nsen_info
         if ( type_sen_info(i) == nf90_float ) then
            if ( trim(name_sen_info(i)) == 'scan_position' ) then
               xdata(ityp)%xseninfo_float(iloc(ityp),i) = rlink%scanpos
            else if ( trim(name_sen_info(i)) == 'sensor_zenith_angle' ) then
               xdata(ityp)%xseninfo_float(iloc(ityp),i) = rlink%satzen
            else if ( trim(name_sen_info(i)) == 'sensor_azimuth_angle' ) then
               xdata(ityp)%xseninfo_float(iloc(ityp),i) = rlink%satazi
            else if ( trim(name_sen_info(i)) == 'solar_azimuth_angle' ) then
               xdata(ityp)%xseninfo_float(iloc(ityp),i) = rlink%solzen
            else if ( trim(name_sen_info(i)) == 'sensor_azimuth_angle' ) then
               xdata(ityp)%xseninfo_float(iloc(ityp),i) = rlink%solazi
            else if ( trim(name_sen_info(i)) == 'sensor_view_angle' ) then
               call calc_sensor_view_angle(trim(rlink%inst), rlink%scanpos, xdata(ityp)%xseninfo_float(iloc(ityp),i))
            end if
!         else if ( type_sen_info(i) == nf90_int ) then
!         else if ( type_sen_info(i) == nf90_char ) then
         end if
      end do

      iv = ufo_vars_getindex(name_sen_info, 'sensor_channel')
      xdata(ityp)%xseninfo_int(:,iv) = rlink%ch(:)

      do i = 1, nvars(ityp)
         xdata(ityp)%xfield(iloc(ityp),i)%val = rlink%tb(i)
         !xdata(ityp)%xfield(iloc(ityp),i)%err = 1.0
         call set_brit_obserr(trim(rlink%inst), i, xdata(ityp)%xfield(iloc(ityp),i)%err)
         xdata(ityp)%xfield(iloc(ityp),i)%qm  = 0
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
      case default
         return
   end select

   view_angle = start + float(ifov-1) * step

end subroutine calc_sensor_view_angle

subroutine get_julian_time(year,month,day,hour,minute,gstime)

! taken from WRFDA/var/da/da_tools/da_get_julian_time.inc

   implicit none

   integer(i_kind), intent(in)  :: year
   integer(i_kind), intent(in)  :: month
   integer(i_kind), intent(in)  :: day
   integer(i_kind), intent(in)  :: hour
   integer(i_kind), intent(in)  :: minute
   real(r_double),  intent(out) :: gstime

   integer(i_kind) :: iw3jdn, ndays, nmind

   iw3jdn  =    day - 32075 &
              + 1461 * (year + 4800 + (month - 14) / 12) / 4 &
              + 367 * (month - 2 - (month - 14) / 12 * 12) / 12 &
              - 3 * ((year + 4900 + (month - 14) / 12) / 100) / 4
   ndays = iw3jdn - 2443510

   nmind = ndays*1440 + hour * 60 + minute
   gstime = float(nmind)

end subroutine get_julian_time

end module radiance_mod
