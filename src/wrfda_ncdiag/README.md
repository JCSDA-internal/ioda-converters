# wrfda-ncdiag

## wrfdancdiag.py
Currently the wrfdancdiag library supports four only one class of WRFDA netCDF diag files:
* Radiances

There are a series of dictionaries at the top of the file wrfdancdiag.py that can be modified to:
* add additional observing platforms for conventional data
* change the output variable name for IODA or UFO
* change the input variable name from WRFDA
* add additional sensors for radiances, ozone, or AOD observations

These dictionaries are:

all_LocKeyList - a list of all the possible Location Keys for the Ncwriter class. The format is:
    'WRFDAname': [('IODAname', 'vartype'){,('AnotherIODAname', 'vartype')}],
````
all_LocKeyList = {
    'date': [('datetime', 'string')],
    'lat': [('latitude', 'float')],
    'lon': [('longitude', 'float')],
    'elv': [('height_above_mean_sea_level', 'float')],
    'scanpos': [('scan_position', 'integer')],
    'satzen': [('sensor_zenith_angle', 'float'),('sensor_view_angle', 'float')],
    'satazi': [('sensor_azimuth_angle', 'float')],
    'solzen': [('solar_zenith_angle', 'float')],
    'solazi': [('solar_azimuth_angle', 'float')],
}
````

wrfda_add_vars(_uv) - additional fields from WRFDA for each variable that one may wish to add to the IODA observation files
````
wrfda_add_vars = {
    'tb_bak': 'WrfdaHofX',
    'tb_bak_clr': 'WrfdaHofXClrSky',
    'tb_omb': 'WrfdaOmB',
    'tb_err': 'WrfdaFinalObsError',
    'cloud_obs': 'WrfdaCloudObs',
    'cloud_mod': 'WrfdaCloudModel',
}
````
Note: that the output to the IODA observation file will be a variable named `brightness_temperatureCH@WrfdaHofX` with input from
the field `tb_bak` for example, where CH is the channel number

wrfdaint - list of wrfda_add_vars that should be written out as integers

rad_platform_sensors_ObsError - obs errors derived from radiance_info files of WRFDA
````
rad_platform_sensors_ObsError = {
    'goes-16-abi': [2.72, 1.79, 1.92, 1.74, wrfda_miss_float, 2.79, 3.08, 3.06, 2.82, 1.74],
    'himawari-8-ahi': [1.052, 1.700, 1.700, 1.350, 0.814, wrfda_miss_float, 0.871, 0.926, 0.933, 0.787],
}
````
