# ioda-converters

The intended way to use this repository is to install in /usr/local by default, and if you cannot write into /usr/local use the CMAKE_INSTALL_PREFIX to install in your home directory. Run the scripts from the place they are installed, not the source directory. That way the scripts can reference each other without providing a path.

For example,
ecbuild -DCMAKE_INSTALL_PREFIX=$HOME/tools  path_to_top_level_cmakelists_file
make install

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
Python scripts to convert GODAE data (binary) to netCDF4 file that can be ingested into IODA.
See [here](src/marine/godae/README.md) for usage.

## odbapi2nc

Python script, odbapi2nc.py, for converting Met Office or ECMWF ODB2 files to netCDF4 files formatted for use by IODA.

Usage: odbapi2nc.py [-h] [-c] [-q] [-t] [-v] input_odb2 definition_yaml output_netcdf

Definition YAML files currently created and tested:
* Met Office Radiosonde
* Met Office Aircraft
* Met Office AMSU-A from atovs report
* ECMWF Radiosonde
* ECMWF Aircraft

## odbapi2json

Python script, odbapi2json.py, for converting Met Office ODB2 files to JSON files which can be used to load the data
to MongoDB.

This script used to work, but is currently not being maintained and no longer does. The code is being kept as a starting point if we
want to update it later.

Usage: odbapi2json.py [-h] [-c] [-q] input_odbapi output_temp > output.json


