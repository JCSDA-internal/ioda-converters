!> Converter from hacked GSI obs error covariance files (have to have channel numbers instead
!> of channel indices stored in indxR) to JEDI files
!> Call:
!> obserror2ioda.x <input GSI obs error correlation file> <output JEDI obs error correlation file>
program obserror2ioda
use netcdf
implicit none

character(len=256) :: filename_in, filename_out
integer :: lu, ioflag
integer :: nch_active, nctot, iprec
integer :: ii
integer :: ncid_out, nch_dimid, varid_nch, varid_corr
integer, allocatable, dimension(:)   :: indxR  !< (nch_active)
real(4), allocatable, dimension(:,:) :: readR4 !< (nch_active, nch_active)

!> get filenames from command line arguments
call getarg(1,filename_in)
call getarg(2,filename_out)

!> read GSI obs error covariances
open(lu,file=trim(filename_in),convert='little_endian',form='unformatted')
read(lu,IOSTAT=ioflag) nch_active, nctot, iprec
print *, "Number of active channels: ", nch_active
allocate(indxR(nch_active))
read(lu,IOSTAT=ioflag) indxR
print *, "Active channels: ", indxR(:)
allocate(readR4(nch_active,nch_active))
read(lu,IOSTAT=ioflag) readR4
close(lu)

!> write out JEDI obs error correlation file
call check( nf90_create(trim(filename_out), NF90_CLOBBER + NF90_NETCDF4,ncid_out))
call check( nf90_def_dim(ncid_out, 'channels', nch_active,  nch_dimid) )
call check( nf90_def_var(ncid_out, 'channels', NF90_INT, nch_dimid, varid_nch) )
call check( nf90_def_var(ncid_out, "obserror_correlations", NF90_FLOAT, (/nch_dimid, nch_dimid/), varid_corr) )
call check( nf90_put_var(ncid_out, varid_nch, indxR) )
call check( nf90_put_var(ncid_out, varid_corr, readR4) )
call check( nf90_close(ncid_out) )

contains
 subroutine check(status)
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then
     print *, trim(nf90_strerror(status))
     stop "Stopped"
    end if
  end subroutine check

end program obserror2ioda
