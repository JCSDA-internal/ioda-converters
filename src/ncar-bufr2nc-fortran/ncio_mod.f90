module ncio_mod

use netcdf
use kinds, only: i_kind, r_single, r_kind
use define_types_mod, only: nvar_met, nvar_info_int, nvar_info_float, &
   nvar_info_char20, nvar_info_char50, n_ncdim, nstring, ndatetime, &
   name_var_met, name_var_info_int, name_var_info_float, name_var_info_char20, &
   name_var_info_char50, xdata, itrue, ifalse, nobtype, vflag, name_ncdim, &
   obtype_list
use netcdf_mod, only: open_netcdf_for_write, close_netcdf, &
   def_netcdf_dims, def_netcdf_var, def_netcdf_end, &
   put_netcdf_var, get_netcdf_dims
use prepbufr_mod, only: cdate_bfile

implicit none

private
public :: write_convobs

contains

subroutine write_convobs

   implicit none

   character(len=128)                  :: ncfname  ! netcdf file name
   integer(i_kind), dimension(n_ncdim) :: ncid_ncdim
   integer(i_kind), dimension(n_ncdim) :: val_ncdim
   character(len=100)                  :: ncname
   integer(i_kind)                     :: ncfileid
   integer(i_kind)                     :: i, ityp, ivar
   logical, dimension(nvar_met)        :: mask

   val_ncdim(4) = nstring
   val_ncdim(5) = ndatetime

   obtype_loop: do ityp = 1, nobtype

      if ( xdata(ityp)%nlocs == 0 ) cycle obtype_loop

      ncfname = trim(obtype_list(ityp))//'_obs_'//cdate_bfile//'.nc4'
      write(*,*) '--- writing ', trim(ncfname)
      call open_netcdf_for_write(trim(ncfname),ncfileid)

      mask = vflag(:,ityp)==itrue
      val_ncdim(1) = count(mask)
      val_ncdim(2) = xdata(ityp)%nlocs
      val_ncdim(3) = xdata(ityp)%nrecs

      ! define netcdf dimensions
      do i = 1, n_ncdim
         call def_netcdf_dims(ncfileid,trim(name_ncdim(i)),val_ncdim(i),ncid_ncdim(i))
      end do

      ! define netcdf variables
      do i = 1, nvar_met
         if ( vflag(i,ityp) == itrue ) then
            ncname = trim(name_var_met(i))//'@ObsValue'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT)
            ncname = trim(name_var_met(i))//'@ObsError'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT)
            ncname = trim(name_var_met(i))//'@PreQC'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_INT)
            ncname = trim(name_var_met(i))//'@ObsType'
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_INT)
         end if
      end do
      do i = 1, nvar_info_int
         ncname = trim(name_var_info_int(i))//'@MetaData'
         call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_INT)
      end do
      do i = 1, nvar_info_float
         ncname = trim(name_var_info_float(i))//'@MetaData'
         call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(2)/),NF90_FLOAT)
      end do
      do i = 1, nvar_info_char20
         ncname = trim(name_var_info_char20(i))//'@MetaData'
         if ( trim(name_var_info_char20(i)) == 'datetime' ) then
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(5),ncid_ncdim(2)/),NF90_CHAR)
         end if
      end do
      do i = 1, nvar_info_char50
         ncname = trim(name_var_info_char50(i))//'@MetaData'
         if ( trim(name_var_info_char50(i)) == 'station_id' ) then
            call def_netcdf_var(ncfileid,ncname,(/ncid_ncdim(4),ncid_ncdim(2)/),NF90_CHAR)
         end if
      end do
      call def_netcdf_end(ncfileid)

      ! writing netcdf variables
      ivar = 0
      var_loop: do i = 1, nvar_met
         if ( vflag(i,ityp) == itrue ) then
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
         end if
      end do var_loop

      do i = 1, nvar_info_int
         ncname = trim(name_var_info_int(i))//'@MetaData'
         call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xinfo_int(i,:))
      end do
      do i = 1, nvar_info_float
         ncname = trim(name_var_info_float(i))//'@MetaData'
         call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xinfo_float(i,:))
      end do
      do i = 1, nvar_info_char20
         ncname = trim(name_var_info_char20(i))//'@MetaData'
         call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xinfo_char20(i,:))
      end do
      do i = 1, nvar_info_char50
         ncname = trim(name_var_info_char50(i))//'@MetaData'
         call put_netcdf_var(ncfileid,ncname,xdata(ityp)%xinfo_char50(i,:))
      end do

      call close_netcdf(trim(ncfname),ncfileid)

   end do obtype_loop

   ! deallocate xdata
   do i = 1, nobtype
      if ( allocated(xdata(i)%var_idx) )      deallocate(xdata(i)%var_idx)
      if ( allocated(xdata(i)%xfield) )       deallocate(xdata(i)%xfield)
      if ( allocated(xdata(i)%xinfo_int) )    deallocate(xdata(i)%xinfo_int)
      if ( allocated(xdata(i)%xinfo_float) )  deallocate(xdata(i)%xinfo_float)
      if ( allocated(xdata(i)%xinfo_char20) ) deallocate(xdata(i)%xinfo_char20)
      if ( allocated(xdata(i)%xinfo_char50) ) deallocate(xdata(i)%xinfo_char50)
   end do
   deallocate(xdata)

end subroutine write_convobs

end module ncio_mod
