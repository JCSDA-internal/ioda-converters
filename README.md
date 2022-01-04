GNU:[![AWS-gnu](https://codebuild.us-east-1.amazonaws.com/badges?uuid=eyJlbmNyeXB0ZWREYXRhIjoieHZsZGhxZEo1a0diR0hCMVE5SCtpRlovVFJ3N1EyTi8wUkpneklzNjRiMWZyY01qNmxmRkZHMDZlWHAyNm1DSDdXTGJhaXVNM1IwK3c5b1B0ck5ib2VzPSIsIml2UGFyYW1ldGVyU3BlYyI6InBFd0NyeDdJN3Y5WTl0S0wiLCJtYXRlcmlhbFNldFNlcmlhbCI6MX0%3D&branch=develop)](https://us-east-1.console.aws.amazon.com/codesuite/codebuild/projects/automated-testing-ioda-conventers-gnu/history)
INTEL:[![AWS-intel](https://codebuild.us-east-1.amazonaws.com/badges?uuid=eyJlbmNyeXB0ZWREYXRhIjoiNmUzcndiY2VIVGtYcC92S2luakNsVGUrdUV5WjhnSGpYWWp2U3JVVERWM0pjSzNHeUg4c1lUTEV6R2VldDdPcmtyZzZHUHYvaFFHek5WV3hxNlJWQ3A4PSIsIml2UGFyYW1ldGVyU3BlYyI6IjZyU21lWUtRTkVEdG9Ld2ciLCJtYXRlcmlhbFNldFNlcmlhbCI6MX0%3D&branch=develop)](https://us-east-1.console.aws.amazon.com/codesuite/codebuild/projects/automated-testing-ioda-conventers-intel/history)
CLANG:[![AWS-clang](https://codebuild.us-east-1.amazonaws.com/badges?uuid=eyJlbmNyeXB0ZWREYXRhIjoicE8zR0dRZmo1NUV6TVVVSTJsY0RYejA0SlRIR1dGOXZBTDVQNVh5dy9vb0ViNXFEbENHZTFPN20wa3p6aHV2ZWhQOTRHUDNyYlc3TnJKdVloOGtqVTM0PSIsIml2UGFyYW1ldGVyU3BlYyI6IjBENU9vV00xRDI5L3MwRmYiLCJtYXRlcmlhbFNldFNlcmlhbCI6MX0%3D&branch=develop)](https://console.aws.amazon.com/codesuite/codebuild/projects/automated-testing-ioda-conventers-clang/history?region=us-east-1)

# ioda-converters

The converters can be built and tested using ioda-bundle. In ioda-bundle the build of the converters is disabled by default (for now) so you must enable the build using the BUILD_IODA_CONVERTERS directive. Here is an example:

```
git clone https://github.com/jcsda-internal/ioda-bundle
cd ioda-bundle
mkdir build
cd build
ecbuild -DBUILD_IODA_CONVERTERS=ON ..
make -j4
ctest
```

## gsi-ncdiag
These scripts use classes defined in the gsincdiag Python library to convert output from GSI netCDF diag files into
IODA observation files and GeoVaLs for UFO. To run GSI and produce the necessary files, see the feature/files_for_jedi
branch in the ProdGSI repository.

The following executable scripts are to be used by the user:
* `proc_gsi_ncdiag.py`
    * This script uses Python multiprocessing to run multiple instances in separate processes to convert the files in
      parallel.
    * `usage: python proc_gsi_ncdiag.py -n NPROCS -o /path/to/obsout -g /path/to/geovalsout /path/to/diagfiles`
       where NPROCS is the number of parallel processes that will run at one time (should be equal to the number of
       cores on one node.
* `subset_files.py`
    * `usage: subset_files.py -m/-s -n NPROCS /path/to/directory`
    * Subsets all of the files in the input directory to an output of only 100 (m) or 1 (s) location.
    * NPROCS controls how many files can be subsetted at once to speed up the process.
* `combine_conv.py`
    * `usage: combine_conv.py -i /path/to/file1.nc /path/to/filen.nc -o /path/to/outputfile.nc`
    * Finds observations for conventional data at the same locations and combines them from multiple files into one
      output file for additional processing or analysis.
* `test_gsidiag.py`
    * `usage: test_gsidiag.py -i /path/to/inputfile.nc -o /path/to/outdir/ -t conv|rad|aod|oz`
    * A script to convert just a single input GSI diag file into one Obs file and one GeoVaLs file
    * This is called by ctest but can also be used by a user rather than `proc_gsi_ncdiag.py`

For developers, or for those who need to change the names of input/output variables in the scripts, see the README in
src/gsi-ncdiag for details.



## marine
The marine converters all take the following format, with some converters taking additional optional arguments as noted:

```
Usage: <converter.py> -i INPUT_FILE(S) -o OUTPUT_FILE -d YYYYMMDDHH
```

* `emc_ice2ioda.py` - Ice concentration observations from EMC. Optional thinning available with `--thin AMOUNT` argument.
* `gds2_sst2ioda.py` - Generic SST/skin-SST converter for use with any GHRSST GD2.0 L2 or L3 data file. Parallel processing of files available with `--threads THREADS` argument. Thinning of data available with `--thin AMOUNT` argument.
* `gmao_obs2ioda.py` - NASA/GMAO ocean observations
* GODAE insitu temperature and salinity ocean profiles from the Fleet Numerical Meteorology and Oceanography Center(FNMOC). Observations available from [here](https://www.usgodae.org/ftp/outgoing/fnmoc/data/ocn/)
  * `godae_profile2ioda.py`
  * `godae_ship2ioda.py`
  * `godae_trak2ioda.py`
* Hybrid-GODAS - preprocessed, suberobbed, and QCd observations of altimetry, insitu T/S, and SST. Used in the NCEP HGODAS project _(these are likely to be removed at some point)_
  * `hgodas_adt2ioda.py`
  * `hgodas_insitu2ioda.py`
  * `hgodas_sst2ioda.py`  
* `rads_adt2ioda.py` - absolute dynamic topography observations from NOAA/NESDIS. Observations available from `ftp://ftp.star.nesdis.noaa.gov/pub/sod/lsa/rads/adt`
* `smap_sss2ioda.py` - SMAP satellite sea surface salinity observations. Observations available from `ftp://podaac-ftp.jpl.nasa.gov/allData/smap/L2/RSS/V3/SCI`
* `viirs_modis_oc2ioda.py` - L2 satellite ocean color observations from NOAA Coastwatch for VIIRS instruments on board JPSS1/NOAA-20 and SNPP satellites, and from NASA GSFC for MODIS instrument on board Aqua satellite. Observations available from `ftp://ftpcoastwatch.noaa.gov/pub/socd2/mecb/coastwatch/viirs/n20/nrt/L2` (VIIRS-JPSS1/NOAA-20), `ftp://ftpcoastwatch.noaa.gov/pub/socd1/mecb/coastwatch/viirs/nrt/L2` (VIIRS-SNPP), and `https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/L2` (MODIS-Aqua).

* `ncep_classes.py` - Convert (prep-)BUFR with embedded BUFR table to IODA format. See [here](src/ncep/README.md) for usage.


## metar
A converter of Surface observation METAR reports in simple CSV format into IODA-ready netCDF4 file.
Currently, the CSV-formatted file should contain a header line such as the following:
Unix_time,DateString,ICAO,Latitude,Longitude,Elev,Temp,Dewp,Wdir,Wspd,Wgst,Vis,Pcp,Pcp3h,Pcp6h,Pcp24h,QcFlag,WxString,WxCode,Altimeter,Cvg1,Bas1,Cvg2,Bas2,Cvg3,Bas3,Length,Raw

At this time, only the output variables of air_temperature, surface_pressure (computed from altimeter setting), specific_humidity (computed from dewpoint temperature and surface_pressure), and eastward/northward_wind (computed from wind speed and direction) are output into the netCDF4 file.  All initial values of PreQC are set to 2 (un-checked) and obserror=0 since UFO software (under development) will be used to flag/discard/QC/etc. these data.  Other variables are expected to be converted as needed such as horizontal visibility in the near future.

```
Usage: <converter.py> -i INPUT_FILE(S) -o OUTPUT_FILE -d YYYYMMDDHH
```

## chem

The chem converters include all converter scripts for aerosols and related chemistry variables.

For NO2, TROPOMI netCDF files are supported with `tropomi_no2_nc2ioda.py`.
```
Usage: tropomi_no2_nc2ioda.py -i input_tropomi_files.nc -o output_ioda_file.nc
```
For -i you can specify a list of files with a shell wildcard and the converter will write them to one output file.
This converter provides all fields needed for assimilation, including the observation value, error, and averaging kernel information.


For AOD, `viirs_aod2ioda.py`, is used to convert the native netCDF format for observations of optical depth from VIIRS AOD550 to IODA netCDF format. Note that it takes only AOD550 explicitly and does not take the 11 AOD channels from VIIRS. The converter uses the following format to execute:

```
Usage: <converter.py> -i INPUT_FILE(S) -o OUTPUT_FILE -m nesdis -k maskout -t 0.0
```
For method option (-m) of bias and uncertainty calculation (default/nesdis), deafult means to set bias and uncertainty as 0.0 and nesdis means to use NESDIS bias and uncertainty calculation method. For maskout option (-k) default/maskout, default means to keep all missing values and maskout means to not write out missing values. For thinning option, the value should be within 0.0 and 1.0 depending how much data will be thinned, and 0.0 means without any thining.


## land

The land converters include all converter scripts for snowpack, soil, vegeation, and the other surface related land variables.

For snow cover fraction(scf), IMS grib2 files are supported with `ims_scf2ioda.py`.
```
Usage: ims_scf2ioda.py -i input_ims_file.grib2 -o output_ioda_file.nc -m maskout
```
For -i you can specify an input file and the converter will write it to one output file. For maskout option (-m) default/maskout, default means to keep all missing values and maskout means to not write out missing values.


For snow depth (snod), afwa grib1 files are supported with `afwa_snod2ioda.py`.
```
Usage: afwa_snod2ioda.py -i input_afwa_file.grb -o output_ioda_file.nc -m maskout
```
For -i you can specify an input file and the converter will write it to one output file. For maskout option (-m) default/maskout, default means to keep all missing values and maskout means to not write out missing values.

It should be noted that both ims_scf2ioda.py and afwa_snod2ioda.py are depending on the python pygrib module. To enable the testing of these two scripts when the user has a pygrib module available, during the ecbuild process, please add -DUSE_PYGRIB=True to the ecbuild command line.  


For snow depth (snod), GHCN csv files are supported with `ghcn_snod2ioda.py`.
```
Usage: ghcn_snod2ioda.py -i input_ghcn_file.csv -o output_ioda_file.nc -f ghcn_station.txt -d YYYYMMDD -m maskout
````
In the test case, YYYYMMDD is set 20200228. For -i you can specify an input file and the converter will write it to one output file. For fix file option (-f), you can specify fix station list file which includes station ID, latitude, longitude, and elevation. For maskout option (-m) default/maskout, default means to keep all missing values and maskout means to not write out missing values.


For surface volumetric soil moisture (ssm), SMAP NRT h5 files are supported with `smap_ssm2ioda.py`.
```
Usage: smap_ssm2ioda.py -i input_smap_file.h5 -o output_ioda_file.nc -m maskout
```
For -i you can specify an input file and the converter will write it to one output file. For maskout option (-m) default/maskout, default means to keep all missing values and maskout means to not write out missing values. It should be noted that SMAP NRT h5 filename contains date and time which has been transferred to the datetime in smap_ssm2ioda.py because the data in the file does not have date and time variables. The h5 file is read with the netCDF4 module rather than the h5py module generally used.

For surface volumetric soil moisture (ssm), SMOS L2 NRT Netcdf files are supported with `smos_ssm2ioda.py`.
```
Usage: smos_ssm2ioda.py -i input_smos_file.nc -o output_ioda_file.nc -m maskout
```
For -i you can specify an input file and the converter will write it to one output file. For maskout option (-m) default/maskout, default means to keep all missing values and maskout means to not write out missing values. Here soil moisture with negative values is also not written out.

For surface soil moisture normalized (ssm), ASCAT L2 NRT Netcdf files are supported with `ascat_ssm2ioda.py`.
```
Usage: ascat_ssm2ioda.py -i input_smos_file.nc -o output_ioda_file.nc -m maskout
```
For -i you can specify an input file and the converter will write it to one output file. For maskout option (-m) default/maskout, default means to keep all missing values and maskout means to not write out missing values.

## GOES

The GOES converter classes generate two IODAv2 data files from a group of raw data files for all 16 channels of GOES-16 or GOES-17 
LB1 products. The final result of this class is two IODAv2 formatted data files - one for Reflectance Factor (RF, ABI channels 1-6) 
and one for Brightness Temperature (BT, ABI channels 7-16). Since GOES-16 and GOES-17 are in a geostationary orbit, auxiliary files 
containing relevant variables and attributes for latitude, longitude, and various angles are accessed (or created if
it does not exist) through the latlon_file_path input argument for each satellite. This converter checks to see if the 
nadir for each satellite has changed and will create a new latlon file if a nadir change has occurred.  

```
Usage   goes_converter = GoesConverter(input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt, include_rf, resolution)
        goes_converter.convert()

Where   input_file_paths - A list of the absolute paths to all 16 ABI channels from the same hour
        latlon_file_path - The path to an existing GoesLatLon file or if it does not exist the path to write the file
        output_file_path_rf - The path to write the IODAv2 reflectance factor data file
        output_file_path_bt - The path to write the IODAv2 brightness temperature data file
        include_rf - Boolean value indicating whether to create the reflectance factor output data file: False (default)
        resolution - The resolution in km: 2 (default), 4, 8, 16, 32, 64
```
