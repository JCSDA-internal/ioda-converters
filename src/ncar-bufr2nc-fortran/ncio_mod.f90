module ncio_mod

use netcdf
use kinds, only: i_kind, r_single, r_kind
use define_types_mod, only: nobtype, nvar_info, n_ncdim, nstring, ndatetime, &
   obtype_list, name_ncdim, name_var_met, name_var_info, name_sen_info, &
   xdata, itrue, ifalse, vflag, ninst, inst_list, write_nc_conv, write_nc_radiance, &
   var_tb, nsen_info, type_var_info, type_sen_info, dim_var_info, dim_sen_info
use netcdf_mod, only: open_netcdf_for_write, close_netcdf, &
   def_netcdf_dims, def_netcdf_var, def_netcdf_end, &
   put_netcdf_var, get_netcdf_dims
use ufo_vars_mod, only: ufo_vars_getindex

implicit none

private
public :: write_obs

contains

subroutine write_obs (filedate, write_opt)

   implicit none

   character(len=*), intent(in)          :: filedate
   integer(i_kind),  intent(in)          :: write_opt

   character(len=nstring)                :: ncfname  ! netcdf file name
   integer(i_kind), dimension(n_ncdim)   :: ncid_ncdim
   integer(i_kind), dimension(n_ncdim)   :: val_ncdim
   character(len=nstring)                :: ncname
   integer(i_kind)                       :: ncfileid
   integer(i_kind)                       :: ntype, nvar
   integer(i_kind)                       :: i, ityp, ivar, ii
   integer(i_kind)                       :: idim, dim1, dim2
   character(len=nstring),   allocatable :: str_nstring(:)
   character(len=ndatetime), allocatable :: str_ndatetime(:)
   character(len=nstring),   allocatable :: name_var_tb(:)
   character(len=nstring)                :: str_tmp
   character(len=4)                      :: c4

   if ( write_opt == write_nc_conv ) then
      ntype = nobtype
   else if ( write_opt == write_nc_radiance ) then
      ntype = ninst
   else
      write(*,*) ' Error: unknwon write_opt = ', write_opt
      return
   end if

   val_ncdim(4) = nstring
   val_ncdim(5) = ndatetime

   obtype_loop: do ityp = 1, ntype

      if ( xdata(ityp)%nlocs == 0 ) cycle obtype_loop

      if ( write_opt == write_nc_conv ) then
         ncfname = trim(obtype_list(ityp))//'_obs_'//trim(filedate)//'.nc4'
      else if ( write_opt == write_nc_radiance ) then
         ncfname = trim(inst_list(ityp))//'_obs_'//trim(filedate)//'.nc4'
         allocate (name_var_tb(xdata(ityp)%nvars))
      end if
      write(*,*) '--- writing ', trim(ncfname)
      call open_netcdf_for_write(trim(ncfname),ncfileid)

      val_ncdim(1) = xdata(ityp)%nvars
      val_ncdim(2) = xdata(ityp)%nlocs
      val_ncdim(3) = xdata(ityp)%nrecs

      ! define netcdf dimensions
      do i = 1, n_ncdim
         call def_netcdf_dims(ncfileid,trim(name_ncdim(i)),val_ncdim(i),ncid_ncdim(i))
      end do

      ! define netcdf variables
      do i = 1, xdata(ityp) % nvars
         if ( write_opt == write_nc_conv ) then
            ivar = xdata(ityp) % var_idx(i)
            ncname = trim(name_var_met(ivar))//'@ObsValue'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT)
            ncname = trim(name_var_met(ivar))//'@ObsError'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT)
            ncname = trim(name_var_met(ivar))//'@PreQC'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_INT)
            ncname = trim(name_var_met(ivar))//'@ObsType'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_INT)
         else if ( write_opt == write_nc_radiance ) then
            write(unit=c4, fmt='(i4)') i
            name_var_tb(i) = trim(var_tb)//'_'//trim(adjustl(c4))
            ncname = trim(name_var_tb(i))//'@ObsValue'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT)
         end if
      end do

      do i = 1, nvar_info
         ncname = trim(name_var_info(i))//'@MetaData'
         idim = ufo_vars_getindex(name_ncdim, dim_var_info(1,i))
         dim1 = ncid_ncdim(idim)
         if ( ufo_vars_getindex(name_ncdim, dim_var_info(2,i)) > 0 ) then
            idim = ufo_vars_getindex(name_ncdim, dim_var_info(2,i))
            dim2 = ncid_ncdim(idim)
            call def_netcdf_var(ncfileid,ncname,(/dim1,dim2/),type_var_info(i))
         else
            call def_netcdf_var(ncfileid,ncname,(/dim1/),type_var_info(i))
         end if
      end do ! nvar_info

      if ( write_opt == write_nc_radiance ) then
         do i = 1, nsen_info
            ncname = trim(name_sen_info(i))//'@MetaData'
            if ( trim(name_sen_info(i)) == 'sensor_channel' ) then
               ncname = trim(name_sen_info(i))//'@VarMetaData'
            end if
            idim = ufo_vars_getindex(name_ncdim, dim_sen_info(1,i))
            dim1 = ncid_ncdim(idim)
            if ( ufo_vars_getindex(name_ncdim, dim_sen_info(2,i)) > 0 ) then
               idim = ufo_vars_getindex(name_ncdim, dim_sen_info(2,i))
               dim2 = ncid_ncdim(idim)
               call def_netcdf_var(ncfileid,ncname,(/dim1,dim2/),type_sen_info(i))
            else
               call def_netcdf_var(ncfileid,ncname,(/dim1/),type_sen_info(i))
            end if
         end do ! nsen_info
      end if ! write_nc_radiance

      call def_netcdf_end(ncfileid)

      ! writing netcdf variables
      ivar = 0
      var_loop: do i = 1, xdata(ityp) % nvars
         if ( write_opt == write_nc_conv ) then
            ivar = ivar + 1
            if ( i /= xdata(ityp)%var_idx(ivar) ) cycle
            ncname = trim(name_var_met(i))//'@ObsValue'
            call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xfield(ivar,:)%val)
            ncname = trim(name_var_met(i))//'@ObsError'
            call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xfield(ivar,:)%err)
            ncname = trim(name_var_met(i))//'@PreQC'
            call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xfield(ivar,:)%qc)
            ncname = trim(name_var_met(i))//'@ObsType'
            call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xfield(ivar,:)%rptype)
         else if ( write_opt == write_nc_radiance ) then
            ncname = trim(name_var_tb(i))//'@ObsValue'
            call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xfield(i,:)%val)
         end if
      end do var_loop

      do i = 1, nvar_info
         ncname = trim(name_var_info(i))//'@MetaData'
         if ( type_var_info(i) == nf90_int ) then
            call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xinfo_int(i,:))
         else if ( type_var_info(i) == nf90_float ) then
            call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xinfo_float(i,:))
         else if ( type_var_info(i) == nf90_char ) then
            if ( trim(name_var_info(i)) == 'variable_names' ) then
               if ( write_opt == write_nc_conv ) then
                  call put_netcdf_var(ncfileid,ncname,name_var_met(xdata(ityp)%var_idx(:)))
               else if ( write_opt == write_nc_radiance ) then
                  call put_netcdf_var(ncfileid,ncname,name_var_tb(:))
               end if
            else if ( trim(name_var_info(i)) == 'station_id' ) then
               allocate(str_nstring(val_ncdim(2)))
               str_nstring(:) = xdata(ityp)%xinfo_char(i,:)
               call put_netcdf_var(ncfileid,ncname,str_nstring)
               deallocate(str_nstring)
            else if ( trim(name_var_info(i)) == 'datetime' ) then
               allocate(str_ndatetime(val_ncdim(2)))
               do ii = 1, val_ncdim(2)
                  str_tmp = xdata(ityp)%xinfo_char(i,ii)
                  str_ndatetime(ii) = str_tmp(1:ndatetime)
               end do
               call put_netcdf_var(ncfileid,ncname,str_ndatetime)
               deallocate(str_ndatetime)
            end if
            !call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xinfo_char(i,:))
         end if
      end do

      if ( write_opt == write_nc_radiance ) then
         do i = 1, nsen_info
            ncname = trim(name_sen_info(i))//'@MetaData'
            if ( type_sen_info(i) == nf90_int ) then
               if ( trim(name_sen_info(i)) == 'sensor_channel' ) then
                  ncname = trim(name_sen_info(i))//'@VarMetaData'
                  call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xseninfo_int(i,:))
               end if
               !call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xseninfo_int(i,:))
            else if ( type_sen_info(i) == nf90_float ) then
               call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xseninfo_float(i,:))
            else if ( type_sen_info(i) == nf90_char ) then
               call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xseninfo_char(i,:))
            end if
         end do
         deallocate (name_var_tb)
      end if ! write_nc_radiance

      call close_netcdf(trim(ncfname),ncfileid)

   end do obtype_loop

   ! deallocate xdata
   do i = 1, ntype
      if ( allocated(xdata(i)%var_idx) )        deallocate(xdata(i)%var_idx)
      if ( allocated(xdata(i)%xfield) )         deallocate(xdata(i)%xfield)
      if ( allocated(xdata(i)%xinfo_int) )      deallocate(xdata(i)%xinfo_int)
      if ( allocated(xdata(i)%xinfo_float) )    deallocate(xdata(i)%xinfo_float)
      if ( allocated(xdata(i)%xinfo_char) )     deallocate(xdata(i)%xinfo_char)
      if ( allocated(xdata(i)%xseninfo_int) )   deallocate(xdata(i)%xseninfo_int)
      if ( allocated(xdata(i)%xseninfo_float) ) deallocate(xdata(i)%xseninfo_float)
      if ( allocated(xdata(i)%xseninfo_char) )  deallocate(xdata(i)%xseninfo_char)
   end do
   deallocate(xdata)

end subroutine write_obs

end module ncio_mod
