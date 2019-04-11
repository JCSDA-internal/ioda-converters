# ioda-converters

## bufr2nc

Python script, bufr2nc.py, for converting BUFR to netCDF4. bufr2nc is built upon the py-ncepbufr package.

Usage: usage: bufr2nc.py [-h] [-m <max_num_msgs>] [-c] [-p] obs_type input_bufr output_netcdf
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

Set of Python scripts to convert GSI ncdiag output to netcdf file that can be ingested into IODA.
The GSI output is a collection of many netcdf files containing obs and geovals data.
The flow consists of runnign three scripts in succession:
* cat_nc_files.py
    * This script concatenates files containing the same obs variable into single files
* select_nc_obs.py
    * This script selects subsets of observations from the output of the cat_nc_files.py script
    * This is used to create the small and medium test cases
* prep_nc_files.py
    * This script formats the output of select_nc_obs.py into files that can be ingested by IODA

See the script tools/build_gsi_nc_files.sh for an example of how to run the flow.
A fourth script, list_sid_raob.py, can be used to create the file containing a list of unique station ids (which is a file used by the select_nc_obs.py script for the Radiosonde and Aircraft obs types).

## marine
The marine converters all take the following format, with some converters taking additional optional arguments as noted:
 
 usage:` <converter.py> -i INPUT_FILE(S) -o OUTPUT_FILE -d YYYYMMDDHH`

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

* `ncep_classes.py` - Convert (prep-)BUFR with embedded BUFR table to IODA format. See [here](src/ncep/README.md) for usage.


## odbapi2nc

Python script, odbapi2nc.py, for converting Met Office or ECMWF ODB2 files to netCDF4 files formatted for use by IODA.

Usage: odbapi2nc.py [-h] [-c] [-q] [-t] [-v] input_odb2 definition_yaml output_netcdf

Definition YAML files currently created and tested:
* Met Office Radiosonde
* Met Office Aircraft
* Met Office AMSU-A from atovs report

## odbapi2json

Python script, odbapi2json.py, for converting Met Office ODB2 files to JSON files which can be used to load the data
to MongoDB.

This script used to work, but is currently not being maintained and no longer does. The code is being kept as a starting point if we
want to update it later.

Usage: odbapi2json.py [-h] [-c] [-q] input_odbapi output_temp > output.json


