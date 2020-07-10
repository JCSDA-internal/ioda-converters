program bufr2nc

use mpisetup
use prepbufr_mod, only: read_prepbufr, sort_convobs
use ncio_mod, only: write_convobs
use gnssro_bufr2ioda, only: read_gnssro

implicit none

character (len=10) :: filename
logical            :: fexist

! initialize MPI ( from mpisetup)
! sets nproc, numproc (from mpisetup),
! where nproc is process number, numproc is total number of processes
call mpi_initialize

filename = 'gnssrobufr'
inquire(file=filename, exist=fexist)
if ( .not. fexist ) then
   write(*,*) 'Error: ', trim(filename), ' not found for decoding...'
else
   write(*,*) '--- processing gnssrobufr'
   call read_gnssro(filename)
end if

filename = 'prepbufr'
inquire(file=filename, exist=fexist)
if ( .not. fexist ) then
   write(*,*) 'Error: ', trim(filename), ' not found for decoding...'
else
   ! read prepbufr file and store data in sequential plink
   call read_prepbufr(filename)

   ! transfer info from plink to arrays grouped by obs/variable types
   call sort_convobs

   ! write out netcdf files
   call write_convobs
end if

! finalize MPI
call mpi_cleanup ! from mpisetup

end program bufr2nc
