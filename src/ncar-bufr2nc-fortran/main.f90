program bufr2nc

use mpisetup
use define_types_mod, only: write_nc_conv, write_nc_radiance
use prepbufr_mod, only: read_prepbufr, sort_obs_conv
use radiance_mod, only: read_amsua_amsub_mhs, read_airs_colocate_amsua, sort_obs_radiance
use ncio_mod, only: write_obs
use gnssro_bufr2ioda, only: read_write_gnssro

implicit none

character (len=10) :: filename
character (len=10) :: filedate
logical            :: fexist
logical            :: do_radiance

! initialize MPI ( from mpisetup)
! sets nproc, numproc (from mpisetup),
! where nproc is process number, numproc is total number of processes
call mpi_initialize

do_radiance = .false. ! initialize

filename = 'gnssrobufr'
inquire(file=filename, exist=fexist)
if ( .not. fexist ) then
   write(*,*) 'Warning: ', trim(filename), ' not found for decoding...'
else
   write(*,*) '--- processing gnssrobufr ---'
   call read_write_gnssro(filename)
end if

filename = 'prepbufr'
inquire(file=filename, exist=fexist)
if ( .not. fexist ) then
   write(*,*) 'Warning: ', trim(filename), ' not found for decoding...'
else
   ! read prepbufr file and store data in sequential linked list for conv obs
   call read_prepbufr(filename, filedate)

   ! transfer info from limked list to arrays grouped by obs/variable types
   call sort_obs_conv

   ! write out netcdf files
   call write_obs(filedate, write_nc_conv)
end if

filename = 'amsuabufr'
inquire(file=filename, exist=fexist)
if ( .not. fexist ) then
   write(*,*) 'Warning: ', trim(filename), ' not found for decoding...'
else
   do_radiance = .true.
   ! read bufr file and store data in sequential linked list for radiances
   call read_amsua_amsub_mhs(filename, filedate)
end if

filename = 'airsbufr'
inquire(file=filename, exist=fexist)
if ( .not. fexist ) then
   write(*,*) 'Warning: ', trim(filename), ' not found for decoding...'
else
   do_radiance = .true.
   ! read bufr file and store data in sequential linked list for radiances
   call read_airs_colocate_amsua(filename, filedate)
end if

if ( do_radiance ) then
   ! transfer info linked list to arrays grouped by satellite instrument types
   call sort_obs_radiance

   ! write out netcdf files
   call write_obs(filedate, write_nc_radiance)
end if

! finalize MPI
call mpi_cleanup ! from mpisetup

end program bufr2nc
