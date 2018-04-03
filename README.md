# bufr2nc

Python script, bufr2nc.py, for converting BUFR to netCDF4. bufr2nc is built upon the py-ncepbufr package.

Usage: bufr2nc.py <obs_type> <input_prepbufr> <output_netcdf>
  * The idea is for bufr2nc.py to create separate netCDF files for different observation types

The short term plan is to handle aircraft, radiance, radiosonde and GPSRO observation types.
AOD observation type may also be included in the short term plans

Currently supported obs types

| Obs Type           | raw BUFR | prepBUFR |
|:-------------------|:--------:|:--------:|
| Aircraft           | Y        | Y        |
| Radiosonde         | N        | Y        |
| Radiance (AMSU-A)  | Y        | N/A      |
| GPSRO              | N        | N/A      |
| AOD                | N        | N/A      |
