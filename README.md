# bufr2nc

Python script, bufr2nc.py, for converting BUFR to netCDF4. bufr2nc is built upon the py-ncepbufr package.

Usage: bufr2nc.py <obs_type> <input_prepbufr> <output_netcdf>
  * The idea is for bufr2nc.py to create separate netCDF files for different observation types

The short term plan is to handle aircraft, radiance and radiosonde observation types.
It is also planned to be able to read and store the raw observation values.

Currently supported obs types

| Obs Type   | raw BUFR | prepBUFR |
|:-----------|:--------:|:--------:|
| Aircraft   | Y        | Y        |
| Radiosonde | N        | Y        |
| Radiance   | N        | N        |
