module atms_spatial_average_c_interface_mod

  use iso_c_binding  

  implicit none

  private
  public:: ATMS_Spatial_Average_c

contains

!  subroutine ATMS_Spatial_Average_c(num_obs, nchanl, fov, time, bt, scanline, error_status) bind(C, name='ATMS_Spatial_Average_f')
   subroutine ATMS_Spatial_Average_c(num_obs, nchanl, fov, bt, scanline, error_status) bind(C, name='ATMS_Spatial_Average_f')

      integer(c_int), value, intent( in) :: num_obs 
      integer(c_int), value, intent( in) :: nchanl
      type(c_ptr),    value, intent( in) :: fov 
!     type(c_ptr),           intent( in) :: time(num_obs) 
      type(c_ptr),           intent(out) :: scanline(num_obs)
!     type(c_ptr),           intent(out) :: bt(nchanl, num_obs) 
      type(c_ptr),           intent(out) :: bt
      integer(c_int),        intent(out) :: error_status 

!>>emily
!      real(c_float), pointer :: fov_f(:) 
!      real(c_float), pointer :: bt_f(:,:) 
!      call c_f_pointer(fov, fov_f, [num_obs])
!      call c_f_pointer(bt, bt_f, [num_obs, nchanl])

      integer, pointer :: fov_f 
      real, pointer :: bt_f 
      call c_f_pointer(fov, fov_f)
      call c_f_pointer(bt, bt_f)
!<<emily

!     call ATMS_Spatial_Average(num_obs, nchanl, fov_f, bt_f,  scanline, error_status)
      call ATMS_Spatial_Average(num_obs, nchanl, scanline, error_status)

   end subroutine ATMS_Spatial_Average_c

end module atms_spatial_average_c_interface_mod


