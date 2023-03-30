module atms_spatial_average_c_interface_mod

  use iso_c_binding

  implicit none

  private
  public:: ATMS_Spatial_Average_c

contains

  subroutine ATMS_Spatial_Average_c(num_loc, nchanl, time, fovn, channel, btobs, scanline, error_status) &
                                    bind(C, name='ATMS_Spatial_Average_f')

    use atms_spatial_average_mod, only: ATMS_Spatial_Average

    integer(c_int), value, intent(in)    :: num_loc
    integer(c_int), value, intent(in)    :: nchanl
    type(c_ptr),           intent(in)    :: fovn
    type(c_ptr),           intent(in)    :: time 
    type(c_ptr),           intent(in)    :: channel 
    type(c_ptr),           intent(inout) :: scanline
    type(c_ptr),           intent(inout) :: btobs 
    integer(c_int),        intent(inout) :: error_status

    integer(c_int),     pointer :: scanline_f(:)
    integer(c_int),     pointer :: fovn_f(:)
    integer(c_int64_t), pointer :: time_f(:)
    integer(c_int),     pointer :: channel_f(:,:)
    real(c_float),      pointer :: btobs_f(:,:)

    call c_f_pointer(scanline, scanline_f, [num_loc])
    call c_f_pointer(fovn, fovn_f, [num_loc])
    call c_f_pointer(time, time_f, [num_loc])
    call c_f_pointer(channel, channel_f, [nchanl, num_loc])
    call c_f_pointer(btobs, btobs_f, [nchanl, num_loc])

    call ATMS_Spatial_Average(num_loc, nchanl, time_f, fovn_f, channel_f, btobs_f, scanline_f, error_status)

  end subroutine ATMS_Spatial_Average_c

end module atms_spatial_average_c_interface_mod
