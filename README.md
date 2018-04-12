# bufr2nc

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
