program bufr2nc

use define_mod, only: write_nc_conv, write_nc_radiance
use kinds, only: i_kind
use prepbufr_mod, only: read_prepbufr, sort_obs_conv, filter_obs_conv
use radiance_mod, only: read_amsua_amsub_mhs, read_airs_colocate_amsua, sort_obs_radiance
use ncio_mod, only: write_obs
use gnssro_bufr2ioda, only: read_write_gnssro

implicit none

integer(i_kind), parameter :: StrLen    = 512
integer(i_kind), parameter :: NameLen   = 15
integer(i_kind), parameter :: DateLen   = 10
integer(i_kind), parameter :: nfile_all = 5
character(len=NameLen)     :: flist_all(nfile_all) = &
   (/                    &
      "gnssro.bufr    ", &
      "prepbufr.bufr  ", &
      "amsua.bufr     ", &
      "airs.bufr      ", &
      "mhs.bufr       "  &
   /)
character (len=NameLen) :: flist(nfile_all)  ! file names to be read in from command line arguments
character (len=NameLen) :: filename
character (len=DateLen) :: filedate
character (len=StrLen)  :: inpdir, outdir, strtmp
logical                 :: fexist
logical                 :: do_radiance
integer(i_kind)         :: narg, iarg, iarg_inpdir, iarg_outdir
integer(i_kind)         :: nfile, ifile
integer(i_kind)         :: itmp


do_radiance = .false. ! initialize

narg = command_argument_count()
ifile = 0
inpdir = '.'
outdir = '.'
flist(:) = 'null'
if ( narg > 0 ) then
   do iarg = 1, narg
      call get_command_argument(number=iarg, value=strtmp)
      if ( trim(strtmp) == '-i' ) then
         iarg_inpdir = iarg + 1
      else if ( trim(strtmp) == '-o' ) then
         iarg_outdir = iarg + 1
      else
         if ( iarg == iarg_inpdir ) then
            call get_command_argument(number=iarg, value=inpdir)
         else if ( iarg == iarg_outdir ) then
            call get_command_argument(number=iarg, value=outdir)
         else
            ifile = ifile + 1
            call get_command_argument(number=iarg, value=flist(ifile))
         end if
      end if
   end do
   if ( ifile == 0 ) then
      nfile = nfile_all
      flist(:) = flist_all(:)
   else
      nfile = ifile
   end if
else
   inpdir = '.'
   outdir = '.'
   nfile = nfile_all
   flist(:) = flist_all(:)
end if

itmp = len_trim(inpdir)
if ( inpdir(itmp:itmp) /= '/' ) inpdir = trim(inpdir)//'/'
itmp = len_trim(outdir)
if ( outdir(itmp:itmp) /= '/' ) outdir = trim(outdir)//'/'

do ifile = 1, nfile

   filename = 'gnssro.bufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         write(*,*) '--- processing gnssro.bufr ---'
         call read_write_gnssro(trim(inpdir)//trim(filename), trim(outdir))
      end if
   end if

   filename = 'prepbufr.bufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         ! read prepbufr file and store data in sequential linked list for conv obs
         call read_prepbufr(trim(inpdir)//trim(filename), filedate)

         call filter_obs_conv

         ! transfer info from limked list to arrays grouped by obs/variable types
         call sort_obs_conv

         ! write out netcdf files
         call write_obs(filedate, write_nc_conv, outdir)
      end if
   end if

   filename = 'amsua.bufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         do_radiance = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_amsua_amsub_mhs(trim(inpdir)//trim(filename), filedate)
      end if
   end if

   filename = 'airs.bufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
      inquire(file=trim(inpdir)//trim(filename), exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
      else
         do_radiance = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_airs_colocate_amsua(trim(inpdir)//trim(filename), filedate)
      end if
   end if

   filename = 'mhs.bufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
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
   call sort_obs_radiance

   ! write out netcdf files
   call write_obs(filedate, write_nc_radiance, outdir)
end if

write(6,*) 'all done!'

end program bufr2nc
