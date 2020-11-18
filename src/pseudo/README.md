**gen_single_ob.py**

Usage: gen_single_ob.py -y /path/to/input.yaml

This script will generate a single observation IODA file based on the configuration in the input YAML file.

Example YAML:

```
obsdataout: testrun/singleob.nc
 lockeys:
   latitude: -20.0
   longitude: -140.0
   datetime: '2020-10-28T12:00:00Z'
 variable:
   name: 'surface_pressure'
   obsvalue: 100000
   obserr: 0.1
   preqc: 0
```
   
- obsdataout: relative path to where to save IODA netCDF file
- lockeys: list of key, value pairs of location variables for the observation (latitude, longitude, time, pressure, height, etc.)
- variable: section to define the observation
  - name: string of type of observation ('air_temperature', 'eastward_wind', 'surface_pressure', etc.) that meets IODA naming convention
  - obsvalue: value of observation
  - obserr: error assigned to observation
  - preqc: pre quality control integer (probably should be 0 for most/all cases)
