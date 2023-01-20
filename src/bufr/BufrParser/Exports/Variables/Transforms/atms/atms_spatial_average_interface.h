/** @file
    @brief Define signature to enable the ATMS spatial average module
    written in Fortran 90 to be called via wrapper functions from C and C++
    application programs.

 */

#pragma once

#ifdef __cplusplus
extern "C" {
#endif


//  void ATMS_Spatial_Average_f(int num_obs, int nchanl, void** FOV, void** Time, void** BT, 
//                              void** Scanline, int error_status); 

//  void ATMS_Spatial_Average_f(int num_obs, int nchanl, void** FOV, void** BT, 
//                              void* Scanline, int* error_status); 

  void ATMS_Spatial_Average_f(int num_obs, int nchanl, void* Scanline, int* error_status); 

#ifdef __cplusplus
}
#endif


