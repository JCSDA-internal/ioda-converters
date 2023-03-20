/*
 *
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 */

/** @file
    @brief Define signature to enable the ATMS spatial average module
    written in Fortran 90 to be called via wrapper functions from C and C++
    application programs.

 */

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

  void ATMS_Spatial_Average_f(int num_loc, int nchanl, void* time, void* fov, void* channel,
                              void* btobs, void* scanline, int* error_status);

#ifdef __cplusplus
}
#endif


