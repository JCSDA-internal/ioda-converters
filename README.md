[![Build Status](https://travis-ci.com/JCSDA/ioda-converters.svg?branch=develop)](https://travis-ci.com/JCSDA/ioda-converters)
GNU:[![AWS-gnu](https://codebuild.us-east-1.amazonaws.com/badges?uuid=eyJlbmNyeXB0ZWREYXRhIjoieHZsZGhxZEo1a0diR0hCMVE5SCtpRlovVFJ3N1EyTi8wUkpneklzNjRiMWZyY01qNmxmRkZHMDZlWHAyNm1DSDdXTGJhaXVNM1IwK3c5b1B0ck5ib2VzPSIsIml2UGFyYW1ldGVyU3BlYyI6InBFd0NyeDdJN3Y5WTl0S0wiLCJtYXRlcmlhbFNldFNlcmlhbCI6MX0%3D&branch=develop)](https://us-east-1.console.aws.amazon.com/codesuite/codebuild/projects/automated-testing-ioda-conventers-gnu/history)
INTEL:[![AWS-intel](https://codebuild.us-east-1.amazonaws.com/badges?uuid=eyJlbmNyeXB0ZWREYXRhIjoiNmUzcndiY2VIVGtYcC92S2luakNsVGUrdUV5WjhnSGpYWWp2U3JVVERWM0pjSzNHeUg4c1lUTEV6R2VldDdPcmtyZzZHUHYvaFFHek5WV3hxNlJWQ3A4PSIsIml2UGFyYW1ldGVyU3BlYyI6IjZyU21lWUtRTkVEdG9Ld2ciLCJtYXRlcmlhbFNldFNlcmlhbCI6MX0%3D&branch=develop)](https://us-east-1.console.aws.amazon.com/codesuite/codebuild/projects/automated-testing-ioda-conventers-intel/history)
CLANG:[![AWS-clang](https://codebuild.us-east-1.amazonaws.com/badges?uuid=eyJlbmNyeXB0ZWREYXRhIjoicE8zR0dRZmo1NUV6TVVVSTJsY0RYejA0SlRIR1dGOXZBTDVQNVh5dy9vb0ViNXFEbENHZTFPN20wa3p6aHV2ZWhQOTRHUDNyYlc3TnJKdVloOGtqVTM0PSIsIml2UGFyYW1ldGVyU3BlYyI6IjBENU9vV00xRDI5L3MwRmYiLCJtYXRlcmlhbFNldFNlcmlhbCI6MX0%3D&branch=develop)](https://console.aws.amazon.com/codesuite/codebuild/projects/automated-testing-ioda-conventers-clang/history?region=us-east-1)

# ioda-converters

The intended way to use this repository is to install in `/usr/local` by default, and if you cannot write into `/usr/local` use the `--prefix` option of `ecbuild` to install in your home directory under `tools`.  Run the scripts from the place they are installed, not the source directory. That way the scripts can reference each other without providing a path.

For example,
```
ecbuild --prefix=$HOME/tools  /your/path/to/ioda-converters
make
make install
ctest
```
## bufr2nc

Python script, `bufr2nc.py`, for converting BUFR to netCDF4. bufr2nc is built upon the py-ncepbufr package.

```
Usage: bufr2nc.py [-h] [-m <max_num_msgs>] [-c] [-p] obs_type input_bufr output_netcdf
```
  * The idea is for bufr2nc.py to create separate netCDF files for different observation types

The short term plan is to handle aircraft, radiance, radiosonde and GPSRO observation types.
AOD observation type may also be included in the short term plans

Currently supported obs types

| Obs Type           | raw BUFR | prepBUFR |
|:-------------------|:--------:|:--------:|
| Aircraft           | Y        | Y        |
| Radiosonde         | N        | Y        |
| Radiance (AMSU-A)  | Y        | N/A      |
| GPSRO              | Y        | N/A      |
| AOD                | N        | N/A      |

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


## odbapi2nc

Python script, `odbapi2nc.py`, for converting Met Office or ECMWF ODB2 files to netCDF4 files formatted for use by IODA.
```
Usage: odbapi2nc.py [-h] [-c] [-q] [-t] [-v] [-b] input_odb2 definition_yaml output_netcdf
```
Definition YAML files currently created and tested:
* Met Office Radiosonde
* Met Office Aircraft
* Met Office AMSU-A from atovs report
* ECMWF Radiosonde
* ECMWF Aircraft

The current ODB library (called ODB API) only supports Python 2.7.
ECMWF will be releasing a new ODB library (called ODC) soon, that will support Python 3.
Our expectation is that ODC will show up in the next few weeks.

Until ODC arrives, we have to use python 2.7 for the ODB test and file conversion.

The ODB file conversion test will be disabled by default so that developers can continue to work in Python 3.
The ODB coding norms test will always be enabled.

When developing the ODB code, you will need to work inside the container (Singularity or CharlieCloud) and with Python 2.7.
To enable the ODB file conversion test, add the ENABLE_ODB_API option to ecbuild as follows:
~~~~~~~~
ecbuild -DENABLE_ODB_API=1 <other_ecbuild_options> <path_to_source_directory>
~~~~~~~~

## odbapi2json

Python script, `odbapi2json.py`, for converting Met Office ODB2 files to JSON files which can be used to load the data
to MongoDB.

This script used to work, but is currently not being maintained and no longer does. The code is being kept as a starting point if we want to update it later.
```
Usage: odbapi2json.py [-h] [-c] [-q] input_odbapi output_temp > output.json
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
