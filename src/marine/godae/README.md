# GODAE data converters.

The scripts in this directory are used to convert GODAE data in binary format into IODA (netCDF4) format. The main readers in the python scripts are based on `ocn_obs.f` obtained from
[here](http://www.usgodae.org/ftp/outgoing/fnmoc/data/ocn/docs/).

### Sample data:

GODAE data can be obtained from [here](https://www.usgodae.org/ftp/outgoing/fnmoc/data/ocn/)

### Usage:

`$> profile2ioda.py -n /path/to/profile/datafile -d YYYYMMDDHH`
`$> ship2ioda.py -n /path/to/ship/datafile -d YYYYMMDDHH`
`$> trak2ioda.py -n /path/to/trak/datafile -d YYYYMMDDHH`

`YYYYMMDDHH` is the reference datetime of data in the datafile
This  will produce a corressponding IODA netCDF4 file with extension `(.nc)`
