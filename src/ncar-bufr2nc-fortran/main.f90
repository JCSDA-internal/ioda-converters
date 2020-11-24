program bufr2nc

use mpisetup
use define_types_mod, only: write_nc_conv, write_nc_radiance
use prepbufr_mod, only: read_prepbufr, sort_obs_conv, filter_obs_conv
use radiance_mod, only: read_amsua_amsub_mhs, read_airs_colocate_amsua, sort_obs_radiance
use ncio_mod, only: write_obs
use gnssro_bufr2ioda, only: read_write_gnssro

implicit none

integer, parameter      :: StrLen    = 10
integer, parameter      :: DateLen   = 10
integer, parameter      :: nfile_all = 4
character(len=StrLen)   :: flist_all(nfile_all) = (/ "gnssrobufr", &
                                                     "prepbufr  ", &
                                                     "amsuabufr ", &
                                                     "airsbufr  " /)
character (len=StrLen), allocatable :: flist(:)
character (len=StrLen)  :: filename
character (len=DateLen) :: filedate
logical                 :: fexist
logical                 :: do_radiance
integer                 :: nfile
integer                 :: ifile

! initialize MPI ( from mpisetup)
! sets nproc, numproc (from mpisetup),
! where nproc is process number, numproc is total number of processes
call mpi_initialize

do_radiance = .false. ! initialize

nfile = command_argument_count()
if ( nfile > 0 ) then
   allocate(flist(nfile))
   do ifile = 1, nfile
      call get_command_argument(number=ifile, value=flist(ifile))
   end do
else
   nfile = nfile_all
   allocate(flist(nfile_all))
   flist(:) = flist_all(:)
end if

do ifile = 1, nfile

   filename = 'gnssrobufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
      inquire(file=filename, exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(filename), ' not found for decoding...'
      else
         write(*,*) '--- processing gnssrobufr ---'
         call read_write_gnssro(filename)
      end if
   end if

   filename = 'prepbufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
      inquire(file=filename, exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(filename), ' not found for decoding...'
      else
         ! read prepbufr file and store data in sequential linked list for conv obs
         call read_prepbufr(filename, filedate)

         call filter_obs_conv

         ! transfer info from limked list to arrays grouped by obs/variable types
         call sort_obs_conv

         ! write out netcdf files
         call write_obs(filedate, write_nc_conv)
      end if
   end if

   filename = 'amsuabufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
      inquire(file=filename, exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(filename), ' not found for decoding...'
      else
         do_radiance = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_amsua_amsub_mhs(filename, filedate)
      end if
   end if

   filename = 'airsbufr'
   if ( trim(flist(ifile)) == trim(filename) ) then
      inquire(file=filename, exist=fexist)
      if ( .not. fexist ) then
         write(*,*) 'Warning: ', trim(filename), ' not found for decoding...'
      else
         do_radiance = .true.
         ! read bufr file and store data in sequential linked list for radiances
         call read_airs_colocate_amsua(filename, filedate)
      end if
   end if

end do ! nfile list

if ( do_radiance ) then
   ! transfer info linked list to arrays grouped by satellite instrument types
   call sort_obs_radiance

   ! write out netcdf files
   call write_obs(filedate, write_nc_radiance)
end if

if ( allocated(flist) ) deallocate(flist)

! finalize MPI
call mpi_cleanup ! from mpisetup

end program bufr2nc
