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
