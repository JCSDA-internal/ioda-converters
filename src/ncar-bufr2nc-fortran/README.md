# bufr2nc_fortran.x

When the executable is run without any arguments, it looks for the following file names in the working directory (if the file exists, do the conversion, otherwise skip it).
  gnssro.bufr
  prepbufr.bufr
  amsua.bufr
  airs.bufr
  mhs.bufr
and writes out converted netcdf files in the same working directory.

The executable can be run with any of the following arguments to optionally set the input directory, output directory and what bufr file(s) to convert.
> bufr2nc_fortran.x -i input_dir -o output_dir gnssro.bufr prepbufr.bufr amsua.bufr mhs.bufr airs.bufr

Example output files (date in the output filename is extracted from the input bufr files):
  aircraft_obs_YYYYMMDDHH.nc4
  amsua_aqua_obs_YYYYMMDDHH.nc4
  amsua_metop-a_obs_YYYYMMDDHH.nc4
  amsua_metop-b_obs_YYYYMMDDHH.nc4
  amsua_n15_obs_YYYYMMDDHH.nc4
  amsua_n18_obs_YYYYMMDDHH.nc4
  amsua_n19_obs_YYYYMMDDHH.nc4
  ascat_obs_YYYYMMDDHH.nc4
  gnssro_obs_YYYYMMDDHH.nc4
  mhs_metop-a_obs_YYYYMMDDHH.nc4
  mhs_metop-b_obs_YYYYMMDDHH.nc4
  mhs_n18_obs_YYYYMMDDHH.nc4
  mhs_n19_obs_YYYYMMDDHH.nc4
  profiler_obs_YYYYMMDDHH.nc4
  satwind_obs_YYYYMMDDHH.nc4
  sfc_obs_YYYYMMDDHH.nc4
  sondes_obs_YYYYMMDDHH.nc4

The output prefix (before _obs) is defined in src/ncar-bufr2nc-fortran/define_mod.f90
The mapping of numeric report types to the named types is coded in define_mod.f90
through subroutines set_obtype_conv, set_name_satellite, set_name_sensor.

The current version is coded to match current GSI-processed diags as close as possible.
- The ob errors of conventional observations are either extracted from the input prepbufr or from an external error table (by changing use_errtable=.false. to use_errtable=.true. in prepbufr_mod.f90).
- The ob errors of AMSU-A/MHS radiances are coded in define_mod.f90. This should changed in the future to read in from an external error table.
- Subroutine fileter_obs_conv (called in main.f90 and can be commented out if it is not desired) applies some additional QC as in GSI's read_prepbufr.f90 for the global model.
100 is added to the @PreQC value when the ob is flagged as not-use by filter_obs_conv.
100 is chosen to make the original prepbufr quality marker easily readable.

