# viirs aod data converters.

The viirs_aod2ioda.py.in script in this directory is used to convert native NetCDF format for observations of optical depth from VIIRS AOD550 to IODA netCDF format. Note that the script takes AOD550 explicitly and does not take the 11 AOD channels from VIIRS.  

### Sample data:

Sample data sets can be retrieved from the testinput directory of JEDI/ioda-converter.

### Required python modules:

numpy, netCDF4, argparse, datetime, sys, os, collections and yaml.

### Python version:
The script is tesed with python 3+

### Usage:
```
$> ncep_classes.py -m  <maximum number of messages to keep>                \
                   -p  <path with the observations>                        \
                   -i  <name of the input BUFR file>                       \
                   -ot <observation type>                                  \
                   -d  <date in YYYYMMDDHH>                                \
                   -o  <output filename in nc (optional)>                  \
                   -Th <thining length (optional, to be removed)>          \
                   -Pr <BUFR or prepBUFR format (optional, to be removed)>
```

### Example:

`ncep_classes.py -m 1 -p /path/to/the/datafile -i xx120 -ot NC031120 -d 2019031000`

### Outputs:
The expected outputs are:


* The NetCDF file with the observations; if the name of the output file is not defined, the file name will have the following format: **ioda.ObservationType.YYYYMMDDHH.nc**. For the example above, the default output filename is ioda.NC031120.2019031000.nc

* The BUFR table in text format, if it does not exist, with the following name: **ObservationType.tbl**, e.g., NC031120.tbl
* The default dictionary from BUFR to ioda NetCDF for all the potentially available observations and metadata in the specific observation type, if it does not exist, with the following name: **ObservationType.dict**, e.g., NC031120.dict
* The default dictionary from BUFR to ioda NetCDF for the specific dataset, the name of the file is **intespec.yaml**, if it does not exist.
The structure of the intespec.yaml is the following:
```
- - height_of_waves     # netcdf variable name, by default the BUFR description is used with undescores instead of spaces, between the words.
  - HOWV                # BUFR variable name
  - 3                   # Data Types, for more info bufr2ncCommon.py
  - *id001
  - [-1]
```
The users can update the **intespec.yaml** according to their naming requirements for the NetCDF variable names, e.g., JEDI requires significant wave height: height_of_waves@ObsValue.

In case that the user provide the **intespec.yaml**, the provided file will be used for the netCDF naming conventions, e.g. CF netCDF naming convention.

High level flowchart of the BUFR to IODA NetCDF converter, and the importance of the dictionary existence:
![alt text](UMO_CS.jpg)
