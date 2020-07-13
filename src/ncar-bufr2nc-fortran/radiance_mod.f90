module radiance_mod

use kinds, only: r_kind,i_kind,r_double
use define_types_mod, only: missing_r, missing_i, rhead, rlink, nstring, &
   ninst, inst_list, set_name_satellite, set_name_sensor, xdata, name_sen_info, &
   nvar_info, name_var_info, type_var_info, nsen_info, type_sen_info
use ufo_vars_mod, only: ufo_vars_getindex
use netcdf, only: nf90_float, nf90_int, nf90_char

implicit none
private
public  :: read_amsua_amsub_mhs
public  :: sort_obs_radiance

contains

!--------------------------------------------------------------

subroutine read_amsua_amsub_mhs (filename, filedate)

   implicit none

   character (len=*),  intent(in)  :: filename
   character (len=10), intent(out) :: filedate  ! ccyymmddhh

   real(r_kind), parameter  :: r8bfms = 9.0E08  ! threshold to check for BUFR missing value

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

   write(*,*) '--- reading '//trim(filename)//' ---'

   timestr = 'YEAR MNTH DAYS HOUR MINU SECO'
   infostr = 'SAID SIID FOVN LSQL SAZA SOZA HOLS HMSL SOLAZI BEARAZ'
   lalostr = 'CLAT CLON'
   britstr = 'CHNM TMBR'

   if ( .not. associated(rhead) ) then
      nullify ( rhead )
      allocate ( rhead )
      nullify ( rhead%next )
   end if

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

   write(unit=*,fmt='(1x,a,i10)') trim(filename)//' file date is: ', idate
   write(unit=filedate, fmt='(i10)') idate

   reports: do while (ireadmg(iunit,subset,idate)==0)
!print*,subset
      do while (ireadsb(iunit)==0)

         num_report_infile = num_report_infile + 1

         if (.not. associated(rlink)) then
            rlink => rhead
         else
            allocate ( rlink%next )
            rlink => rlink%next
            nullify ( rlink%next )
         end if

         call ufbint(iunit,timedat,ntime,1,iret,timestr)
         call ufbint(iunit,infodat,ninfo,1,iret,infostr)
         call ufbint(iunit,lalodat,nlalo,1,iret,lalostr)
         call ufbrep(iunit,data1b8,2,maxchan,nchan,britstr)

         rlink % lat = lalodat(1)
         rlink % lon = lalodat(2)

         iyear  = nint(timedat(1))
         imonth = nint(timedat(2))
         iday   = nint(timedat(3))
         ihour  = nint(timedat(4))
         imin   = nint(timedat(5))
         isec   = nint(timedat(6))


         write(unit=rlink%datetime, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
            iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'

         rlink % satid  = nint(infodat(1))  ! SAID satellite identifier
         rlink % instid = nint(infodat(2))  ! SIID instrument identifier

         rlink % scanpos = missing_i
         rlink % landsea = missing_i
         rlink % satzen  = missing_r
         rlink % satazi  = missing_r
         rlink % solzen  = missing_r
         rlink % solazi  = missing_r
         rlink % elv     = missing_r

         if ( infodat(3)  < r8bfms ) rlink % scanpos = nint(infodat(3)) ! FOVN field of view number
         if ( infodat(4)  < r8bfms ) rlink % landsea = infodat(4)       ! LSQL land sea qualifier 0:land, 1:sea, 2:coast
         if ( infodat(5)  < r8bfms ) rlink % satzen  = infodat(5)       ! SAZA satellite zenith angle (degree)
         if ( infodat(10) < r8bfms ) rlink % satazi  = infodat(10)      ! BEARAZ satellite azimuth (degree true)
         if ( infodat(6)  < r8bfms ) rlink % solzen  = infodat(6)       ! SOZA solar zenith angle (degree)
         if ( infodat(9)  < r8bfms ) rlink % solazi  = infodat(9)       ! SOLAZI solar azimuth (degree true)
         if ( infodat(7)  < r8bfms ) rlink % elv     = infodat(7)       ! HOLS height of land surface (m)

         rlink % nchan = nchan
         if ( nchan > 0 ) then
            allocate ( rlink % tb(nchan) )   ! brightness temperature
            allocate ( rlink % ch(nchan) )   ! channel number
            rlink % tb(:) = missing_r
            rlink % ch(:) = missing_i
            do i = 1, nchan
               if ( data1b8(1,i) < r8bfms ) rlink % ch(i) = nint(data1b8(1,i))
               if ( data1b8(2,i) < r8bfms ) rlink % tb(i) = data1b8(2,i)
            end do
         end if

      end do ! ireadsb
   end do reports

   call closbf(iunit)
   close(iunit)

   write(*,*) 'num_report_infile = ', num_report_infile

end subroutine read_amsua_amsub_mhs

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

   write(*,*) 'num_report_decoded = ', sum(nrecs(:))
   do i = 1, ninst
      write(*,'(a20,2i10)') inst_list(i), nrecs(i), nlocs(i)
   end do

   ! allocate data arrays with the just counted numbers
   allocate (xdata(ninst))
   do i = 1, ninst
      xdata(i) % nrecs = nrecs(i)
      xdata(i) % nlocs = nlocs(i)
      xdata(i) % nvars = nvars(i)

      if ( nlocs(i) > 0 ) then
         allocate (xdata(i)%xinfo_float(nvar_info, nlocs(i)))
         allocate (xdata(i)%xinfo_int  (nvar_info, nlocs(i)))
         allocate (xdata(i)%xinfo_char (nvar_info, nlocs(i)))
         allocate (xdata(i)%xseninfo_float(nsen_info, nlocs(i)))
         allocate (xdata(i)%xseninfo_int  (nsen_info, nvars(i)))
         if ( nvars(i) > 0 ) then
            allocate (xdata(i)%xfield(nvars(i), nlocs(i)))
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
               xdata(ityp)%xinfo_int(i,iloc(ityp)) = irec
            end if
         else if ( type_var_info(i) == nf90_float ) then
            if ( name_var_info(i) == 'time' ) then
               xdata(ityp)%xinfo_float(i,iloc(ityp)) = missing_r !rlink%dhr
            else if ( trim(name_var_info(i)) == 'station_elevation' ) then
               xdata(ityp)%xinfo_float(i,iloc(ityp)) = rlink%elv
            else if ( trim(name_var_info(i)) == 'latitude' ) then
               xdata(ityp)%xinfo_float(i,iloc(ityp)) = rlink%lat
            else if ( trim(name_var_info(i)) == 'longitude' ) then
               xdata(ityp)%xinfo_float(i,iloc(ityp)) = rlink%lon
            end if
         else if ( type_var_info(i) == nf90_char ) then
            if ( trim(name_var_info(i)) == 'datetime' ) then
               xdata(ityp)%xinfo_char(i,iloc(ityp)) = rlink%datetime
            else if ( trim(name_var_info(i)) == 'station_id' ) then
               xdata(ityp)%xinfo_char(i,iloc(ityp)) = rlink%inst
            end if
         end if
      end do

      do i = 1, nsen_info
         if ( type_sen_info(i) == nf90_float ) then
            if ( trim(name_sen_info(i)) == 'scan_position' ) then
               xdata(ityp)%xseninfo_float(i,iloc(ityp)) = rlink%scanpos
            else if ( trim(name_sen_info(i)) == 'sensor_zenith_angle' ) then
               xdata(ityp)%xseninfo_float(i,iloc(ityp)) = rlink%satzen
            else if ( trim(name_sen_info(i)) == 'sensor_azimuth_angle' ) then
               xdata(ityp)%xseninfo_float(i,iloc(ityp)) = rlink%satazi
            else if ( trim(name_sen_info(i)) == 'solar_azimuth_angle' ) then
               xdata(ityp)%xseninfo_float(i,iloc(ityp)) = rlink%solzen
            else if ( trim(name_sen_info(i)) == 'sensor_azimuth_angle' ) then
               xdata(ityp)%xseninfo_float(i,iloc(ityp)) = rlink%solazi
            else if ( trim(name_sen_info(i)) == 'sensor_view_angle' ) then
               xdata(ityp)%xseninfo_float(i,iloc(ityp)) = missing_r
            end if
!         else if ( type_sen_info(i) == nf90_int ) then
!         else if ( type_sen_info(i) == nf90_char ) then
         end if
      end do

      iv = ufo_vars_getindex(name_sen_info, 'sensor_channel')
      xdata(ityp)%xseninfo_int(iv,:) = rlink%ch(:)

      do i = 1, nvars(ityp)
         do iv = 1, nvars(ityp)
            xdata(ityp)%xfield(iv,iloc(ityp))%val = rlink%tb(iv)
         end do
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

end module radiance_mod
