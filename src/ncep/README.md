# NCEP data converters.

The ncep_classes.py script in this directory is used to convert (prep-) BUFR data format into IODA (netCDF4) format. The code is based on the bufrlib by NCEP and it has been tested mainly with marine observations. 

### Sample data:

Sample data sets can be retrieved from the operational NCEP's data tanks; and there is a sample (xx120) with the code.

### Required python modules:

numpy, ncepbufr, bufr2ncCommon, bufr2ncObsTypes, netCDF4, argparse, datetime, sys, os, collections and yaml.

### Python version:
The script is compatible with python2 and python3; it is tested with Python 2.7.5 and 3.7.0.

### Usage:
```
$> ncep_classes.py -m  <maximum number of messages to keep>                \
                   -p  <path with the observations>                        \
                   -i  <name of the input BUFR file>                       \
                   -ot <observation type>                                  \
                   -d  <date in YYYYMMDDHH>                                \
                   -l  <yaml file name>                                    \
                   -o  <output filename in nc (optional)>                  \
                   -Th <thining length (optional, to be removed)>          \
                   -Pr <BUFR or prepBUFR format (optional, to be removed)>
```

### Example:

`ncep_classes.py -m 5 -p /path/to/the/datafile -i xx120 -ot NC031120 -l 031120.yaml -d 2019031012`

### Outputs:
The expected outputs are:


* The NetCDF file with the observations; if the name of the output file is not defined, the file name will have the following format: **ioda.ObservationType.YYYYMMDDHH.nc**. For the example above, the default output filename is ioda.NC031120.2019031000.nc

* Maximum number of messages (-m) is optional. If -m is set, the output only contains the first m messages. If -m is not set, the whole bufr file is converted.   

* The BUFR table in text format, if it does not exist, with the following name: **ObservationType.tbl**, e.g., NC031120.tbl

* The default dictionary from BUFR to ioda NetCDF for all the potentially available observations and metadata in the specific observation type, if it does not exist, with the following name: **ObservationType.dict**, e.g., NC031120.dict

* The default dictionary from BUFR to ioda NetCDF is **ioda_dictionary**. If -l is set, the program will use the dictionary chosen by user. If -l is not set, ** ioda_dictionary** will be used.

The structure of the ioda_dictionary is the following:
```
- - height_of_waves     # netcdf variable name, by default the BUFR description is used with undescores instead of spaces, between the words.
  - HOWV                # BUFR variable name
  - 3                   # Data Types, for more info bufr2ncCommon.py
  - *id001
  - [-1]
```
The users can update the **ioda_dictionary** according to their naming requirements for the NetCDF variable names, e.g., JEDI requires significant wave height: height_of_waves@ObsValue.

In case that the user provide the **ioda_dictionary**, the provided file will be used for the netCDF naming conventions, e.g. CF netCDF naming convention.

High level flowchart of the BUFR to IODA NetCDF converter, and the importance of the dictionary existence:
![alt text](UMO_CS.jpg)
