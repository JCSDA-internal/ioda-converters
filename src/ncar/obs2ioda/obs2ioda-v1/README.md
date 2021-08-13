# obs2ioda.x

```
Compile: cd obs2ioda-v1/src; make
```

```
Usage: obs2ioda.x [-i input_dir] [-o output_dir] [bufr_filename(s)_to_convert] [-split]
```
If [-i input_dir] [-o output_dir] are not specified in the command line, the default is the current working directory.  
If [bufr_filename(s)_to_convert] is not specified in the command line, the code looks for file name, **prepbufr.bufr** (also **satwnd.bufr**, **gnssro.bufr**, **amsua.bufr**, **airs.bufr**, **mhs.bufr**), in the input/working directory. If the file exists, do the conversion, otherwise skip it.  
If specify -split, the converted file will contain hourly data.

> obs2ioda.x -i input_dir -o output_dir prepbufr.gdas.YYYYMMDD.tHHz.nr

Example output files (date in the output filename is extracted from the input bufr files):  
  aircraft_obs_YYYYMMDDHH.nc4  
  ascat_obs_YYYYMMDDHH.nc4  
  profiler_obs_YYYYMMDDHH.nc4  
  satwind_obs_YYYYMMDDHH.nc4  
  sfc_obs_YYYYMMDDHH.nc4  
  sondes_obs_YYYYMMDDHH.nc4  

> obs2ioda.x -i input_dir -o output_dir gdas.satwnd.tHHz.YYYYMMDD.bufr

Example output files (date in the output filename is extracted from the input bufr files):  
  satwnd_obs_YYYYMMDDHH.nc4  (GOES-16/GOES-17, AVHRR (METOP/NOAA), VIIRS (NPP/NOAA), LEOGEO AMVs)  
  
> obs2ioda.x -i input_dir -o output_dir gdas.1bamua.tHHz.YYYYMMDD.bufr

Example output files:  
  amsua_metop-a_obs_YYYYMMDDHH.nc4  
  amsua_metop-b_obs_YYYYMMDDHH.nc4  
  amsua_n15_obs_YYYYMMDDHH.nc4  
  amsua_n18_obs_YYYYMMDDHH.nc4  
  amsua_n19_obs_YYYYMMDDHH.nc4  

> obs2ioda.x -i input_dir -o output_dir gdas.airsev.tHHz.YYYYMMDD.bufr

Example output files:  
  amsua_aqua_obs_YYYYMMDDHH.nc4  

> obs2ioda.x -i input_dir -o output_dir gdas.1bmhs.tHHz.YYYYMMDD.bufr

Example output files:  
  mhs_metop-a_obs_YYYYMMDDHH.nc4  
  mhs_metop-b_obs_YYYYMMDDHH.nc4  
  mhs_n18_obs_YYYYMMDDHH.nc4  
  mhs_n19_obs_YYYYMMDDHH.nc4  

> obs2ioda.x -i input_dir -o output_dir gdas.gpsro.tHHz.YYYYMMDD.bufr

Example output files:  
  gnssro_obs_YYYYMMDDHH.nc4  

```
Usage: obs2ioda.x -i input_dir -ahi -t YYYYMMDDHHNN [-s 3]
```

Input files are a list of Himawari Standard Data, e.g. HS_H08_20200815_0000_B14_FLDK_R20_S0210.DAT in the input_dir.  
Minute must be specified in the time (-t) option.  
Number of pixels to skip is optional.

## Notes
* The output prefix (before _obs) is defined in define_mod.f90
* The mapping of numeric report types to the named types is coded in define_mod.f90
through subroutines set_obtype_conv, set_name_satellite, set_name_sensor.
* For gdas.satwnd.tHHz.YYYYMMDD.bufr, only GOES-16/GOES-17, AVHRR (METOP/NOAA), VIIRS (NPP/NOAA), LEOGEO AMVs are converted when available. Other AMVs are available through PREPBUFR files.

## The current version is coded to match current GSI-processed diags as close as possible.
* The ob errors of conventional observations are either extracted from the input prepbufr or from an external error table (if obs_errtable exists in the working directory).
* The ob errors of AMSU-A/MHS radiances are coded in define_mod.f90. This should be changed in the future to read in from an external error table.
* The ob errors of satwnd-decoded AMVs are from an external error table (obs_errtable).
* Subroutine filter_obs_conv applies some additional QC for conventional observations as in GSI's read_prepbufr.f90 for the global model and can be de-activated through ``-noqc`` command-line option.
100 is added to the @PreQC value when the ob is flagged as not-use by filter_obs_conv.  
100 is chosen to make the original prepbufr quality marker easily readable.
* Subroutine filter_obs_satwnd applies QC for GOES-16/GOES-17 AMVs as in GSI's read_satwnd.f90.  
@PreQC value is set to 15 for rejected obs.


