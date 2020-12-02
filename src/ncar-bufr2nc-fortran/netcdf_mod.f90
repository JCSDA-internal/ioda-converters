module netcdf_mod

use netcdf
use define_mod, only: missing_r, missing_i, nstring

implicit none

private

! public subroutines
public :: open_netcdf, close_netcdf, get_netcdf_dims, get_netcdf_var_1d
public :: open_netcdf_for_write
public :: def_netcdf_dims, def_netcdf_var, def_netcdf_end
public :: put_netcdf_var

! variables visible to this module only
integer :: ncstatus

interface get_netcdf_var_1d
   module procedure get_netcdf_var_1d_real
   module procedure get_netcdf_var_1d_integer
   module procedure get_netcdf_var_1d_char
end interface

interface put_netcdf_var
   module procedure put_netcdf_var_real
   module procedure put_netcdf_var_integer
   module procedure put_netcdf_var_char
end interface

contains

subroutine open_netcdf(fname,ncfileid)
   character(len=*), intent(in) :: fname
   integer, intent(out) :: ncfileid

   ncstatus = nf90_open(path=trim(adjustl(fname)),mode=nf90_nowrite,ncid=ncfileid)  ! open file
   if ( ncstatus /= 0 ) then
      write(0,fmt='(a)') 'error opening netcdf file '//trim(adjustl(fname))
      write(0,*) 'ncstatus = ', ncstatus
      stop
   endif

   return
end subroutine open_netcdf

subroutine close_netcdf(fname,ncfileid)
   character(len=*), intent(in) :: fname
   integer, intent(in) :: ncfileid
   ncstatus = nf90_close(ncfileid) ! close file
   if ( ncstatus /= 0 ) then
      write(0,fmt='(a)') 'error closing netcdf file '//trim(adjustl(fname))
      write(0,*) 'ncstatus = ', ncstatus
      stop
   endif
end subroutine close_netcdf

subroutine get_netcdf_dims(fileid,variable,output)
   integer, intent(in) :: fileid
   character(len=*), intent(in) :: variable
   integer, intent(out) :: output

   integer :: ncdimid, ierr

   ierr = 0
   ncstatus = nf90_inq_dimid(fileid,trim(adjustl(variable)),ncdimid) ; ierr = ierr + ncstatus
   ncstatus = nf90_inquire_dimension(fileid,ncdimid,len=output)      ; ierr = ierr + ncstatus
   if ( ierr /= 0 ) then
      write(0,*) 'Error reading dimension for '//trim(adjustl(variable))
      write(0,*) 'ierr = ',ierr
      stop
   endif
!  write(*,fmt='(a,i8)')variable//' = ', output ! print out dimensions for the variable

   return

end subroutine get_netcdf_dims

subroutine get_netcdf_var_1d_real(fileid,variable,dim1,output)

   integer, intent(in) :: fileid, dim1
   character(len=*), intent(in) :: variable
   real, intent(inout), dimension(dim1)  :: output

   integer :: ncvarid, ierr

   ierr = 0
   ncstatus = nf90_inq_varid(fileid,trim(adjustl(variable)),ncvarid) ; ierr = ierr + ncstatus
   ncstatus = nf90_get_var(fileid,ncvarid,output)                    ; ierr = ierr + ncstatus

   if ( ierr /= 0 ) then
      write(0,*) 'Error reading data for '//trim(adjustl(variable))
      write(0,*) 'ierr = ',ierr
      stop
   endif

   return

end subroutine get_netcdf_var_1d_real

subroutine get_netcdf_var_1d_integer(fileid,variable,dim1,output)

   integer, intent(in) :: fileid, dim1
   character(len=*), intent(in) :: variable
   integer, intent(inout), dimension(dim1)  :: output

   integer :: ncvarid, ierr

   ierr = 0
   ncstatus = nf90_inq_varid(fileid,trim(adjustl(variable)),ncvarid) ; ierr = ierr + ncstatus
   ncstatus = nf90_get_var(fileid,ncvarid,output)                    ; ierr = ierr + ncstatus

   if ( ierr /= 0 ) then
      write(0,*) 'Error reading data for '//trim(adjustl(variable))
      write(0,*) 'ierr = ',ierr
      stop
   endif

   return

end subroutine get_netcdf_var_1d_integer

subroutine get_netcdf_var_1d_char(fileid,variable,dim1,output)

   integer, intent(in) :: fileid, dim1
   character(len=*), intent(in) :: variable
   character(len=*), intent(inout), dimension(dim1)  :: output

   integer :: ncvarid, ierr

   ierr = 0
   ncstatus = nf90_inq_varid(fileid,trim(adjustl(variable)),ncvarid) ; ierr = ierr + ncstatus
   ncstatus = nf90_get_var(fileid,ncvarid,output)                    ; ierr = ierr + ncstatus

   if ( ierr /= 0 ) then
      write(0,*) 'Error reading data for '//trim(adjustl(variable))
      write(0,*) 'ierr = ',ierr
      stop
   endif

   return

end subroutine get_netcdf_var_1d_char

subroutine open_netcdf_for_write(fname,ncfileid)
   character(len=*), intent(in) :: fname
   integer, intent(out) :: ncfileid

   ! create nc file
   !ncstatus = nf90_create(path=trim(adjustl(fname)),cmode=nf90_clobber,ncid=ncfileid)
   ncstatus = nf90_create(path=trim(adjustl(fname)),cmode=NF90_NETCDF4,ncid=ncfileid)
   if ( ncstatus /= 0 ) then
      write(0,fmt='(a)') 'error creating netcdf file '//trim(adjustl(fname))
      write(0,*) 'ierr = ', ncstatus
      stop
   endif

   return
end subroutine open_netcdf_for_write

subroutine def_netcdf_dims(fileid,variable,input,output)
   integer, intent(in) :: fileid
   character(len=*), intent(in)  :: variable ! name of this dimension
   integer,          intent(in)  :: input    ! size of this dimension
   integer,          intent(out) :: output   ! ncid of this dimension

   integer :: ncdimid, ierr

   ierr = 0
   ncstatus = nf90_def_dim(fileid,trim(adjustl(variable)),input,ncdimid)
   ierr = ierr + ncstatus
   ncstatus = nf90_put_att(fileid, NF90_GLOBAL, variable, input)
   ierr = ierr + ncstatus

   if ( ierr /= 0 ) then
      write(0,*) 'Error defining dimension for '//trim(adjustl(variable))
      write(0,*) 'ncstatus = ', ierr
      stop
   endif

   output = ncdimid

   return
end subroutine def_netcdf_dims

subroutine def_netcdf_var(fileid,variable,dimids,nctype,attrib_name,attrib)

   integer,               intent(in) :: fileid
   character(len=*),      intent(in) :: variable
   integer, dimension(:), intent(in) :: dimids
   integer,               intent(in) :: nctype
   character(len=*),      intent(in), optional :: attrib_name
   character(len=*),      intent(in), optional :: attrib

   integer :: ncvarid, ierr
   character(len=nstring) :: att_name
   character(len=nstring) :: att

   ierr = 0
   ncstatus = nf90_def_var(fileid,trim(adjustl(variable)),nctype,dimids,ncvarid)
   ierr = ierr + ncstatus
   if ( nctype == NF90_FLOAT ) then
      ncstatus = nf90_def_var_fill(fileid, ncvarid, 0, missing_r)
      ierr = ierr + ncstatus
   else if ( nctype == NF90_INT ) then
      ncstatus = nf90_def_var_fill(fileid, ncvarid, 0, missing_i)
      ierr = ierr + ncstatus
   end if

   att_name = ''
   att      = ''
   if ( present(attrib_name) ) att_name = attrib_name
   if ( present(attrib     ) ) att      = attrib
   if ( len_trim(att_name) > 0 .and. len_trim(att) > 0 ) then
      ncstatus = nf90_put_att(fileid, ncvarid, trim(att_name), trim(att))
      ierr = ierr + ncstatus
   end if

   if ( ierr /= 0 ) then
      write(0,*) 'Error defining var for '//trim(adjustl(variable))
      write(0,*) 'ierr = ', ierr
      stop
   endif

   return
end subroutine def_netcdf_var

subroutine def_netcdf_end(fileid)

   integer, intent(in) :: fileid

   ncstatus = nf90_enddef(fileid)
   if ( ncstatus /= 0 ) then
      write(0,*) 'Error end defining for fileid = ', fileid
      write(0,*) 'ncstatus = ', ncstatus
      stop
   endif

   return
end subroutine def_netcdf_end

subroutine put_netcdf_var_real(fileid,variable,input)

   integer,            intent(in) :: fileid
   character(len=*),   intent(in) :: variable
   real, dimension(:), intent(in) :: input

   integer :: ncvarid, ierr

   ierr = 0
   ncstatus = nf90_inq_varid(fileid,trim(adjustl(variable)),ncvarid)
   ierr = ierr + ncstatus
   ncstatus = nf90_put_var(fileid,ncvarid,input)
   ierr = ierr + ncstatus

   if ( ierr /= 0 ) then
      write(0,*) 'Error writing data for '//trim(adjustl(variable))
      write(0,*) 'ierr = ',ierr
      stop
   endif

   return
end subroutine put_netcdf_var_real

subroutine put_netcdf_var_integer(fileid,variable,input)

   integer,               intent(in) :: fileid
   character(len=*),      intent(in) :: variable
   integer, dimension(:), intent(in) :: input

   integer :: ncvarid, ierr

   ierr = 0
   ncstatus = nf90_inq_varid(fileid,trim(adjustl(variable)),ncvarid)
   ierr = ierr + ncstatus
   ncstatus = nf90_put_var(fileid,ncvarid,input)
   ierr = ierr + ncstatus

   if ( ierr /= 0 ) then
      write(0,*) 'Error writing data for '//trim(adjustl(variable))
      write(0,*) 'ierr = ',ierr
      stop
   endif

   return
end subroutine put_netcdf_var_integer

subroutine put_netcdf_var_char(fileid,variable,input)

   integer,                        intent(in) :: fileid
   character(len=*),               intent(in) :: variable
   character(len=*), dimension(:), intent(in) :: input

   integer :: ncvarid, ierr

   ierr = 0
   ncstatus = nf90_inq_varid(fileid,trim(adjustl(variable)),ncvarid)
   ierr = ierr + ncstatus
   ncstatus = nf90_put_var(fileid,ncvarid,input)
   ierr = ierr + ncstatus

   if ( ierr /= 0 ) then
      write(0,*) 'Error writing data for '//trim(adjustl(variable))
      write(0,*) 'ierr = ',ierr
      stop
   endif

   return
end subroutine put_netcdf_var_char

end module netcdf_mod
