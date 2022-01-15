program obs2ioda

use define_mod, only: write_nc_conv, write_nc_radiance, write_nc_radiance_geo, StrLen, xdata, &
   ninst
use kinds, only: i_kind
use prepbufr_mod, only: read_prepbufr, sort_obs_conv, filter_obs_conv, do_tv_to_ts
use radiance_mod, only: read_amsua_amsub_mhs, read_airs_colocate_amsua, sort_obs_radiance, &
   read_iasi, read_cris, radiance_to_temperature
use ncio_mod, only: write_obs
use gnssro_bufr2ioda, only: read_write_gnssro
use ahi_hsd_mod, only: read_hsd, subsample
use satwnd_mod, only: read_satwnd, filter_obs_satwnd, sort_obs_satwnd
use utils_mod, only: da_advance_time

implicit none

integer(i_kind), parameter :: NameLen   = 64
integer(i_kind), parameter :: DateLen   = 10
integer(i_kind), parameter :: DateLen14 = 14
integer(i_kind), parameter :: nfile_all = 8
integer(i_kind), parameter :: ftype_unknown  = -1
integer(i_kind), parameter :: ftype_prepbufr =  1
integer(i_kind), parameter :: ftype_gnssro   =  2
integer(i_kind), parameter :: ftype_amsua    =  3
integer(i_kind), parameter :: ftype_mhs      =  4
integer(i_kind), parameter :: ftype_airs     =  5
integer(i_kind), parameter :: ftype_satwnd   =  6
integer(i_kind), parameter :: ftype_iasi     =  7
integer(i_kind), parameter :: ftype_cris     =  8

integer(i_kind)            :: ftype(nfile_all)
character(len=NameLen)     :: flist_all(nfile_all) = &
   (/                    &
      "gnssro.bufr    ", &
      "prepbufr.bufr  ", &
      "satwnd.bufr    ", &
      "amsua.bufr     ", &
      "airs.bufr      ", &
      "mhs.bufr       ", &
      "iasi.bufr      ", &
      "cris.bufr      "  &
   /)
character (len=NameLen) :: flist(nfile_all)  ! file names to be read in from command line arguments
character (len=NameLen) :: filename
character (len=DateLen) :: filedate, filedate_out
character (len=StrLen)  :: inpdir, outdir, cdatetime
logical                 :: fexist
logical                 :: do_radiance
logical                 :: do_radiance_hyperIR
logical                 :: do_ahi
logical                 :: apply_gsi_qc
logical                 :: time_split
integer(i_kind)         :: nfgat, hour_fgat
integer(i_kind)         :: nfile, ifile
integer(i_kind)         :: itmp
integer(i_kind)         :: itime
character (len=DateLen14) :: dtime, datetmp


do_tv_to_ts = .true.
do_radiance = .false. ! initialize
do_radiance_hyperIR = .false. ! initialize
do_ahi = .false.
apply_gsi_qc = .true. !.false.
time_split = .false.

call parse_files_to_convert

if ( time_split ) then
   hour_fgat = 1  ! can also be 3 or 2
   ! corresponding to dtime_min='-3h' and dtime_max='+3h'
   nfgat = (6/hour_fgat) + 1
else
   nfgat = 1
end if

do ifile = 1, nfile

   filename = flist(ifile)

   if ( ftype(ifile) == ftype_gnssro ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         write(*,*) '--- processing gnssro.bufr ---'
         call read_write_gnssro(trim(inpdir)//trim(filename), trim(outdir))
      end if
   end if

   if ( ftype(ifile) == ftype_satwnd ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         ! read satwnd file and store data in sequential linked list for conv obs
         call read_satwnd(trim(inpdir)//trim(filename), filedate)

         if ( apply_gsi_qc ) then
            write(*,*) '--- applying some additional QC as in GSI read_satwnd.f90 for the global model ---'
            call filter_obs_satwnd
         end if

         ! transfer info from limked list to arrays grouped by obs/variable types
         call sort_obs_satwnd(filedate, nfgat)

         ! write out netcdf files
         if ( nfgat > 1 ) then
            do itime = 1, nfgat
               ! corresponding to dtime_min='-3h' and dtime_max='+3h'
               write(dtime,'(i2,a)')  hour_fgat*(itime-1)-3, 'h'
               call da_advance_time(filedate, trim(dtime), datetmp)
               filedate_out = datetmp(1:10)
               call write_obs(filedate_out, write_nc_conv, outdir, itime)
            end do
         else
            call write_obs(filedate, write_nc_conv, outdir, 1)
         end if
         if ( allocated(xdata) ) deallocate(xdata)
      end if
   end if

   if ( ftype(ifile) == ftype_prepbufr ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         ! read prepbufr file and store data in sequential linked list for conv obs
         call read_prepbufr(trim(inpdir)//trim(filename), filedate)

         if ( apply_gsi_qc ) then
            write(*,*) '--- applying some additional QC as in GSI read_prepbufr.f90 for the global model ---'
            call filter_obs_conv
         end if

         ! transfer info from limked list to arrays grouped by obs/variable types
         call sort_obs_conv(filedate, nfgat)

         ! write out netcdf files
         if ( nfgat > 1 ) then
            do itime = 1, nfgat
               ! corresponding to dtime_min='-3h' and dtime_max='+3h'
               write(dtime,'(i2,a)')  hour_fgat*(itime-1)-3, 'h'
               call da_advance_time(filedate, trim(dtime), datetmp)
               filedate_out = datetmp(1:10)
               call write_obs(filedate_out, write_nc_conv, outdir, itime)
            end do
         else
            call write_obs(filedate, write_nc_conv, outdir, 1)
         end if
         if ( allocated(xdata) ) deallocate(xdata)
      end if
   end if

   if ( ftype(ifile) == ftype_amsua ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         do_radiance = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_amsua_amsub_mhs(trim(inpdir)//trim(filename), filedate)
      end if
   end if

   if ( ftype(ifile) == ftype_airs ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         do_radiance = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_airs_colocate_amsua(trim(inpdir)//trim(filename), filedate)
      end if
   end if

   if ( ftype(ifile) == ftype_mhs ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         do_radiance = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_amsua_amsub_mhs(trim(inpdir)//trim(filename), filedate)
      end if
   end if

end do ! nfile list

if ( do_radiance ) then
   ! transfer info linked list to arrays grouped by satellite instrument types
   call sort_obs_radiance(filedate, nfgat)

   ! write out netcdf files
   if ( nfgat > 1 ) then
      do itime = 1, nfgat
         ! corresponding to dtime_min='-3h' and dtime_max='+3h'
         write(dtime,'(i2,a)')  hour_fgat*(itime-1)-3, 'h'
         call da_advance_time(filedate, trim(dtime), datetmp)
         filedate_out = datetmp(1:10)
         call write_obs(filedate_out, write_nc_radiance, outdir, itime)
      end do
   else
      call write_obs(filedate, write_nc_radiance, outdir, 1)
   end if
   if ( allocated(xdata) ) deallocate(xdata)
end if

do ifile = 1, nfile

   filename = flist(ifile)

   if ( ftype(ifile) == ftype_iasi ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         do_radiance_hyperIR = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_iasi(trim(inpdir)//trim(filename), filedate)
      end if
   end if

   if ( ftype(ifile) == ftype_cris ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         do_radiance_hyperIR = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_cris(trim(inpdir)//trim(filename), filedate)
      end if
   end if

end do

if ( do_radiance_hyperIR ) then
   ! transfer info from linked list to arrays grouped by satellite instrument types
   call sort_obs_radiance(filedate, nfgat)

   call radiance_to_temperature(ninst, nfgat)

   ! write out netcdf files
   if ( nfgat > 1 ) then
      do itime = 1, nfgat
         ! corresponding to dtime_min='-3h' and dtime_max='+3h'
         write(dtime,'(i2,a)')  hour_fgat*(itime-1)-3, 'h'
         call da_advance_time(filedate, trim(dtime), datetmp)
         filedate_out = datetmp(1:10)
         call write_obs(filedate_out, write_nc_radiance, outdir, itime)
      end do
   else
      call write_obs(filedate, write_nc_radiance, outdir, 1)
   end if
   if ( allocated(xdata) ) deallocate(xdata)
end if

if ( do_ahi ) then
   if ( len_trim(cdatetime) /= 12 ) then
      write(*,*) 'Error: -t ccyymmddhhnn not specified for -ahi'
      stop
   end if
   call read_HSD(cdatetime, inpdir)
   filedate = cdatetime(1:10)
   call write_obs(filedate, write_nc_radiance_geo, outdir, 1)
   if ( allocated(xdata) ) deallocate(xdata)
end if

write(6,*) 'all done!'

contains

subroutine parse_files_to_convert

implicit none

integer(i_kind)       :: iunit = 21
integer(i_kind)       :: narg, iarg, iarg_inpdir, iarg_outdir, iarg_datetime, iarg_subsample
integer(i_kind)       :: itmp
integer(i_kind)       :: iost, iret, idate
character(len=StrLen) :: strtmp
character(len=8)      :: subset

narg = command_argument_count()
ifile = 0
inpdir = '.'
outdir = '.'
cdatetime = ''
flist(:) = 'null'
if ( narg > 0 ) then
   do iarg = 1, narg
      call get_command_argument(number=iarg, value=strtmp)
      if ( trim(strtmp) == '-qc' ) then
         apply_gsi_qc = .true.
      else if ( trim(strtmp) == '-noqc' ) then
         apply_gsi_qc = .false.
      else if ( trim(strtmp) == '-tv' ) then
         do_tv_to_ts = .false.
      else if ( trim(strtmp) == '-ahi' ) then
         do_ahi = .true.
      else if ( trim(strtmp) == '-split' ) then
         time_split = .true.
      else if ( trim(strtmp) == '-i' ) then
         iarg_inpdir = iarg + 1
      else if ( trim(strtmp) == '-o' ) then
         iarg_outdir = iarg + 1
      else if ( trim(strtmp) == '-t' ) then
         iarg_datetime = iarg + 1
      else if ( trim(strtmp) == '-s' ) then
         iarg_subsample = iarg + 1
      else
         if ( iarg == iarg_inpdir ) then
            call get_command_argument(number=iarg, value=inpdir)
         else if ( iarg == iarg_outdir ) then
            call get_command_argument(number=iarg, value=outdir)
         else if ( iarg == iarg_datetime ) then
            call get_command_argument(number=iarg, value=cdatetime)
         else if ( iarg == iarg_subsample ) then
            call get_command_argument(number=iarg, value=strtmp)
            if ( len_trim(strtmp) > 0 ) then
              read(strtmp,'(i2)') subsample
            else
              subsample = 1
            end if
         else
            ifile = ifile + 1
            call get_command_argument(number=iarg, value=flist(ifile))
         end if
      end if
   end do
   if ( ifile == 0 ) then
      nfile = nfile_all
      flist(:) = flist_all(:)
      ftype(:) = (/ ftype_gnssro, ftype_prepbufr, ftype_satwnd,  &
                    ftype_amsua, ftype_airs, ftype_mhs,  &
                    ftype_iasi, ftype_cris /)
   else
      nfile = ifile
   end if
else
   inpdir = '.'
   outdir = '.'
   nfile = nfile_all
   flist(:) = flist_all(:)
   ftype(:) = (/ ftype_gnssro, ftype_prepbufr, ftype_satwnd,  &
                 ftype_amsua, ftype_airs, ftype_mhs,  &
                 ftype_iasi, ftype_cris /)
end if

itmp = len_trim(inpdir)
if ( inpdir(itmp:itmp) /= '/' ) inpdir = trim(inpdir)//'/'
itmp = len_trim(outdir)
if ( outdir(itmp:itmp) /= '/' ) outdir = trim(outdir)//'/'

! use default file lists if not set in command-line arguemnt
if ( narg == 0 .or. ifile == 0 ) return

! determine the input file type
fileloop: do ifile = 1, nfile
   if ( trim(flist(ifile)) == 'null' ) then
      ftype(ifile) = ftype_unknown
      cycle fileloop
   end if
   open(unit=iunit, file=trim(inpdir)//trim(flist(ifile)), form='unformatted', iostat=iost, status='old')
   call openbf(iunit, 'IN', iunit)
   call readmg(iunit,subset,idate,iret)
!print*,subset
   if ( subset(1:5) == 'NC005' ) then
      ftype(ifile) = ftype_satwnd
   else
   select case ( trim(subset) )
   case (  'ADPUPA', 'ADPSFC' )
      ftype(ifile) = ftype_prepbufr
   case ( 'NC003010' )
      ftype(ifile) = ftype_gnssro
   case ( 'NC021023' )
      ftype(ifile) = ftype_amsua
   case ( 'NC021027' )
      ftype(ifile) = ftype_mhs
   case ( 'NC021249' )
      ftype(ifile) = ftype_airs
   case ( 'NC021241' )
      ftype(ifile) = ftype_iasi
   case ( 'NC021202', 'NC021206' )
      ftype(ifile) = ftype_cris
   case default
      ftype(ifile) = ftype_unknown
   end select
   end if
   call closbf(iunit)
   close(iunit)
end do fileloop

end subroutine parse_files_to_convert

end program obs2ioda
