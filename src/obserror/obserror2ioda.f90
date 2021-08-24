program obserror2ioda
use netcdf
implicit none
integer :: nch_active, lu, ioflag, nctot, iprec, ii, ncid_out, nch_dimid, varid_corr
integer, allocatable, dimension(:) :: indxR
real(4),allocatable, dimension(:,:) :: readR4  ! nch_active x nch_active x ninstruments

open(lu,file=trim("Rcov_iasibsea"),convert='little_endian',form='unformatted')
read(lu,IOSTAT=ioflag) nch_active, nctot, iprec

print *, "nch_active: ", nch_active, ", nctot: ", nctot, ", iprec= ", iprec
allocate(indxR(nch_active))
read(lu,IOSTAT=ioflag) indxR

print *, " active channels: "
do ii = 1, nch_active
  print *, indxR(ii)
enddo

allocate(readR4(nch_active,nch_active))
read(lu,IOSTAT=ioflag) readR4

print *, "R: "
do ii = 1, nch_active
  print *, readR4(ii,:)
enddo
close(lu)

call check( nf90_create(trim("Rcov_iasibsea.nc4"), NF90_CLOBBER + NF90_NETCDF4,ncid_out))
call check( nf90_def_dim(ncid_out, 'nchannels', nch_active,  nch_dimid) )
call check( nf90_def_var(ncid_out, "obserror_correlations", NF90_FLOAT, (/nch_dimid, nch_dimid/), varid_corr) )
call check( nf90_put_var(ncid_out, varid_corr, readR4) )
call check( nf90_close(ncid_out) )

end program obserror2ioda
