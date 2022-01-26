module ncio_mod

use netcdf
use kinds, only: i_kind, r_single, r_kind
use define_mod, only: nobtype, nvar_info, n_ncdim, nstring, ndatetime, &
   obtype_list, name_ncdim, name_var_met, name_var_info, name_sen_info, &
   xdata, itrue, ifalse, vflag, ninst, inst_list, write_nc_conv, write_nc_radiance, &
   write_nc_radiance_geo, ninst_geo, geoinst_list, &
   var_tb, nsen_info, type_var_info, type_sen_info, dim_var_info, dim_sen_info, &
   unit_var_met, iflag_conv, iflag_radiance, set_brit_obserr
use netcdf_mod, only: open_netcdf_for_write, close_netcdf, &
   def_netcdf_dims, def_netcdf_var, def_netcdf_end, &
   put_netcdf_var, get_netcdf_dims
use ufo_vars_mod, only: ufo_vars_getindex

implicit none

private
public :: write_obs

contains

subroutine write_obs (filedate, write_opt, outdir, itim)

   implicit none

   character(len=*), intent(in)          :: filedate
   integer(i_kind),  intent(in)          :: write_opt
   character(len=*), intent(in)          :: outdir
   integer(i_kind),  intent(in)          :: itim

   character(len=512)                    :: ncfname  ! netcdf file name
   integer(i_kind), dimension(n_ncdim)   :: ncid_ncdim
   integer(i_kind), dimension(n_ncdim)   :: val_ncdim
   character(len=nstring)                :: ncname
   integer(i_kind)                       :: ncfileid
   integer(i_kind)                       :: ntype, nvar
   integer(i_kind)                       :: i, ityp, ivar, ii, iv
   integer(i_kind)                       :: idim, dim1, dim2
   character(len=nstring),   allocatable :: str_nstring(:)
   character(len=ndatetime), allocatable :: str_ndatetime(:)
   character(len=nstring),   allocatable :: name_var_tb(:)
   character(len=nstring)                :: str_tmp
   character(len=4)                      :: c4
   integer(i_kind)                       :: iflag
   integer(i_kind), allocatable :: ichan(:)
   real(r_kind),    allocatable :: obserr(:)
   integer(i_kind) :: has_wavenumber
   integer(i_kind) :: ncid_ncgrp_wn

   if ( write_opt == write_nc_conv ) then
      ntype = nobtype
   else if ( write_opt == write_nc_radiance ) then
      ntype = ninst
   else if ( write_opt == write_nc_radiance_geo ) then
      ntype = ninst_geo
   else
      write(*,*) ' Error: unknwon write_opt = ', write_opt
      return
   end if

   val_ncdim(3) = nstring
   val_ncdim(4) = ndatetime

   obtype_loop: do ityp = 1, ntype

      if ( xdata(ityp,itim)%nlocs == 0 ) cycle obtype_loop

      if ( write_opt == write_nc_conv ) then
         ncfname = trim(outdir)//trim(obtype_list(ityp))//'_obs_'//trim(filedate)//'.nc4'
      else if ( write_opt == write_nc_radiance ) then
         ncfname = trim(outdir)//trim(inst_list(ityp))//'_obs_'//trim(filedate)//'.nc4'
      else if ( write_opt == write_nc_radiance_geo ) then
         ncfname = trim(outdir)//trim(geoinst_list(ityp))//'_obs_'//trim(filedate)//'.nc4'
      end if
      if ( write_opt == write_nc_radiance .or. write_opt == write_nc_radiance_geo ) then
         allocate (name_var_tb(xdata(ityp,itim)%nvars))
         iv = ufo_vars_getindex(name_sen_info, 'sensor_channel')
         allocate (ichan(xdata(ityp,itim)%nvars))
         ichan(:) = xdata(ityp,itim)%xseninfo_int(:,iv)
         allocate (obserr(xdata(ityp,itim)%nvars))
         call set_brit_obserr(inst_list(ityp), xdata(ityp,itim)%nvars, obserr)
         do i = 1, xdata(ityp,itim) % nlocs
            xdata(ityp,itim)%xfield(i,:)%err = obserr(:)
         end do
      end if
      write(*,*) '--- writing ', trim(ncfname)
      call open_netcdf_for_write(trim(ncfname),ncfileid)

      val_ncdim(1) = xdata(ityp,itim)%nvars
      val_ncdim(2) = xdata(ityp,itim)%nlocs

      ! define netcdf dimensions
      do i = 1, n_ncdim
         call def_netcdf_dims(ncfileid,trim(name_ncdim(i)),val_ncdim(i),ncid_ncdim(i))
      end do

      if ( allocated(xdata(ityp,itim)%wavenumber) ) then
         has_wavenumber = itrue
      else
         has_wavenumber = ifalse
      end if

      ! define netcdf variables
      do i = 1, xdata(ityp,itim) % nvars
         if ( write_opt == write_nc_conv ) then
            ivar = xdata(ityp,itim) % var_idx(i)
            ncname = trim(name_var_met(ivar))//'@ObsValue'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT,'units',unit_var_met(ivar))
            ncname = trim(name_var_met(ivar))//'@ObsError'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT)
            ncname = trim(name_var_met(ivar))//'@PreQC'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_INT)
            ncname = trim(name_var_met(ivar))//'@ObsType'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_INT)
         else if ( write_opt == write_nc_radiance .or. write_opt == write_nc_radiance_geo ) then
            if ( ichan(i) < 0 ) cycle
            write(unit=c4, fmt='(i4)') ichan(i)
            name_var_tb(i) = trim(var_tb)//'_'//trim(adjustl(c4))
            ncname = trim(name_var_tb(i))//'@ObsValue'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT,'units','K')
            ncname = trim(name_var_tb(i))//'@ObsError'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT)
            ncname = trim(name_var_tb(i))//'@PreQC'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_INT)
         end if
      end do

      var_info_def_loop: do i = 1, nvar_info
         if ( write_opt == write_nc_conv ) then
            iflag = iflag_conv(i,ityp)
         else if ( write_opt == write_nc_radiance .or. write_opt == write_nc_radiance_geo ) then
            iflag = iflag_radiance(i)
         end if
         if ( iflag /= itrue ) cycle var_info_def_loop
         ncname = trim(name_var_info(i))//'@MetaData'
         if ( trim(name_var_info(i)) == 'variable_names' ) then
            ncname = trim(name_var_info(i))//'@VarMetaData'
         end if
         idim = ufo_vars_getindex(name_ncdim, dim_var_info(1,i))
         dim1 = ncid_ncdim(idim)
         if ( ufo_vars_getindex(name_ncdim, dim_var_info(2,i)) > 0 ) then
            idim = ufo_vars_getindex(name_ncdim, dim_var_info(2,i))
            dim2 = ncid_ncdim(idim)
            call def_netcdf_var(ncfileid,ncname,(/dim1,dim2/),type_var_info(i))
         else
            call def_netcdf_var(ncfileid,ncname,(/dim1/),type_var_info(i))
         end if
      end do var_info_def_loop ! nvar_info

      if ( write_opt == write_nc_radiance .or. write_opt == write_nc_radiance_geo ) then
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
         if ( has_wavenumber == itrue ) then
            idim = ufo_vars_getindex(name_ncdim, 'nvars')
            dim1 = ncid_ncdim(idim)
            call def_netcdf_var(ncfileid,'sensor_band_central_radiation_wavenumber@VarMetaData', &
                                (/dim1/),NF90_FLOAT)
         end if
      end if ! write_nc_radiance

      call def_netcdf_end(ncfileid)

      ! writing netcdf variables
      var_loop: do i = 1, xdata(ityp,itim) % nvars
         if ( write_opt == write_nc_conv ) then
            ivar = xdata(ityp,itim) % var_idx(i)
            if ( vflag(ivar,ityp) == itrue ) then
               ncname = trim(name_var_met(ivar))//'@ObsValue'
               call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xfield(:,i)%val)
               ncname = trim(name_var_met(ivar))//'@ObsError'
               call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xfield(:,i)%err)
               ncname = trim(name_var_met(ivar))//'@PreQC'
               call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xfield(:,i)%qm)
               ncname = trim(name_var_met(ivar))//'@ObsType'
               call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xfield(:,i)%rptype)
            end if
         else if ( write_opt == write_nc_radiance .or. write_opt == write_nc_radiance_geo ) then
            if ( ichan(i) < 0 ) cycle
            ncname = trim(name_var_tb(i))//'@ObsValue'
            call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xfield(:,i)%val)
            ncname = trim(name_var_tb(i))//'@ObsError'
            call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xfield(:,i)%err)
            ncname = trim(name_var_tb(i))//'@PreQC'
            call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xfield(:,i)%qm)
         end if
      end do var_loop

      var_info_loop: do i = 1, nvar_info
         if ( write_opt == write_nc_conv ) then
            iflag = iflag_conv(i,ityp)
         else if ( write_opt == write_nc_radiance .or. write_opt == write_nc_radiance_geo ) then
            iflag = iflag_radiance(i)
         end if
         if ( iflag /= itrue ) cycle var_info_loop
         ncname = trim(name_var_info(i))//'@MetaData'
         if ( trim(name_var_info(i)) == 'variable_names' ) then
            ncname = trim(name_var_info(i))//'@VarMetaData'
         end if
         if ( type_var_info(i) == nf90_int ) then
            call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xinfo_int(:,i))
         else if ( type_var_info(i) == nf90_float ) then
            call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xinfo_float(:,i))
         else if ( type_var_info(i) == nf90_char ) then
            if ( trim(name_var_info(i)) == 'variable_names' ) then
               if ( write_opt == write_nc_conv ) then
                  call put_netcdf_var(ncfileid,ncname,name_var_met(xdata(ityp,itim)%var_idx(:)))
               else if ( write_opt == write_nc_radiance .or. write_opt == write_nc_radiance_geo ) then
                  call put_netcdf_var(ncfileid,ncname,name_var_tb(:))
               end if
            else if ( trim(name_var_info(i)) == 'station_id' ) then
               allocate(str_nstring(val_ncdim(2))) ! nlocs
               str_nstring(:) = xdata(ityp,itim)%xinfo_char(:,i)
               call put_netcdf_var(ncfileid,ncname,str_nstring)
               deallocate(str_nstring)
            else if ( trim(name_var_info(i)) == 'datetime' ) then
               allocate(str_ndatetime(val_ncdim(2)))
               do ii = 1, val_ncdim(2)
                  str_tmp = xdata(ityp,itim)%xinfo_char(ii,i)
                  str_ndatetime(ii) = str_tmp(1:ndatetime)
               end do
               call put_netcdf_var(ncfileid,ncname,str_ndatetime)
               deallocate(str_ndatetime)
            end if
         end if
      end do var_info_loop

      if ( write_opt == write_nc_radiance .or. write_opt == write_nc_radiance_geo ) then
         do i = 1, nsen_info
            ncname = trim(name_sen_info(i))//'@MetaData'
            if ( type_sen_info(i) == nf90_int ) then
               if ( trim(name_sen_info(i)) == 'sensor_channel' ) then
                  ncname = trim(name_sen_info(i))//'@VarMetaData'
                  call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xseninfo_int(:,i))
               end if
               !call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xseninfo_int(:,i))
            else if ( type_sen_info(i) == nf90_float ) then
               call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xseninfo_float(:,i))
            else if ( type_sen_info(i) == nf90_char ) then
               call put_netcdf_var(ncfileid,ncname,xdata(ityp,itim)%xseninfo_char(:,i))
            end if
         end do
         if ( has_wavenumber == itrue ) then
            call put_netcdf_var(ncfileid, 'sensor_band_central_radiation_wavenumber@VarMetaData', &
                                xdata(ityp,itim)%wavenumber(:))
         end if
         deallocate (ichan)
         deallocate (obserr)
         deallocate (name_var_tb)
      end if ! write_nc_radiance

      call close_netcdf(trim(ncfname),ncfileid)

   end do obtype_loop

   ! deallocate xdata
   do i = 1, ntype
      if ( allocated(xdata(i,itim)%var_idx) )        deallocate(xdata(i,itim)%var_idx)
      if ( allocated(xdata(i,itim)%xfield) )         deallocate(xdata(i,itim)%xfield)
      if ( allocated(xdata(i,itim)%xinfo_int) )      deallocate(xdata(i,itim)%xinfo_int)
      if ( allocated(xdata(i,itim)%xinfo_float) )    deallocate(xdata(i,itim)%xinfo_float)
      if ( allocated(xdata(i,itim)%xinfo_char) )     deallocate(xdata(i,itim)%xinfo_char)
      if ( allocated(xdata(i,itim)%xseninfo_int) )   deallocate(xdata(i,itim)%xseninfo_int)
      if ( allocated(xdata(i,itim)%xseninfo_float) ) deallocate(xdata(i,itim)%xseninfo_float)
      if ( allocated(xdata(i,itim)%xseninfo_char) )  deallocate(xdata(i,itim)%xseninfo_char)
      if ( allocated(xdata(i,itim)%wavenumber) )     deallocate(xdata(i,itim)%wavenumber)
   end do
!   deallocate(xdata) ! moved to main.f90

end subroutine write_obs

end module ncio_mod
