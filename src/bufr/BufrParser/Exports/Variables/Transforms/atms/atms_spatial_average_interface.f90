module atms_spatial_average_c_interface_mod

  use iso_c_binding  

  implicit none

  private
  public:: ATMS_Spatial_Average_c

contains

  subroutine ATMS_Spatial_Average_c(num_obs, nchanl, scanline, error_status) bind(C, name='ATMS_Spatial_Average_f')
    use atms_spatial_average_mod, only: ATMS_Spatial_Average
    use kinds, only: i_kind

    integer(c_int), value, intent(in)    :: num_obs
    integer(c_int), value, intent(in)    :: nchanl
    type(c_ptr),           intent(inout) :: scanline
    integer(c_int),        intent(inout) :: error_status

    integer(i_kind), pointer :: scanline_f

    call c_f_pointer(scanline, scanline_f)
    call ATMS_Spatial_Average(num_obs, nchanl, scanline_f, error_status)
  end subroutine ATMS_Spatial_Average_c

end module atms_spatial_average_c_interface_mod
