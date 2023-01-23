module atms_spatial_average_c_interface_mod

  use iso_c_binding  

  implicit none

  private
  public:: ATMS_Spatial_Average_c

contains

  subroutine ATMS_Spatial_Average_c(num_obs, nchanl, fovn, btobs, scanline, error_status) bind(C, name='ATMS_Spatial_Average_f')
    use atms_spatial_average_mod, only: ATMS_Spatial_Average
    use kinds, only: i_kind

    integer(c_int), value, intent(in)    :: num_obs
    integer(c_int), value, intent(in)    :: nchanl
    type(c_ptr),           intent(in)    :: fovn
    type(c_ptr),           intent(inout) :: scanline
    type(c_ptr),           intent(inout) :: btobs 
    integer(c_int),        intent(inout) :: error_status

    integer(c_int), pointer :: scanline_f(:)
    integer(c_int), pointer :: fovn_f(:)
    real(c_float),  pointer :: btobs_f(:,:)

    call c_f_pointer(scanline, scanline_f, [num_obs])
    call c_f_pointer(fovn, fovn_f, [num_obs])
    call c_f_pointer(btobs, btobs_f, [nchanl, num_obs])

    call ATMS_Spatial_Average(num_obs, nchanl, fovn_f, btobs_f, scanline_f, error_status)
  end subroutine ATMS_Spatial_Average_c

end module atms_spatial_average_c_interface_mod
