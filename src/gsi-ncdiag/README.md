# gsi-ncdiag

## gsincdiag.py
Currently the gsincdiag library supports four different classes of GSI netCDF diag files:
* Conventional
* Radiances
* Ozone

There are a series of dictionaries at the top of the file gsincdiag.py that can be modified to:
* add additional observing platforms for conventional data
* change the output variable name for IODA or UFO
* change the input variable name from GSI
* add additional sensors for radiances, ozone, or other observations

These dictionaries are:

conv_platforms - a dictionary where the key is the type of input GSI diag file for conventional observations (conv_ps)
and the value is a list of possible output platforms (surface, sondes, etc.)
```
conv_platforms = {
    "conv_ps": [
        'sfc',
        'sondes',
        'sfcship',
    ],
    }
````
bufrtypes - uv_ and conv_bufrtypes are dictionaries that for any observing platform (sondes, etc.) provide the BUFR
observation type codes to match for subsetting the observations.
```
conv_bufrtypes = {
    "aircraft": range(130, 140),
    "sondes": range(120, 123),
    "rass": [126],
    "sfcship": [180, 183],
    "sfc": [181, 187],
    "gps": [3, 4, 745],
}
```
all_LocKeyList - a list of all the possible Location Keys for the Ncwriter class. The format is:
    'GSIname': ('IODAname', 'vartype'),
````
all_LocKeyList = {
    'Station_ID': ('station_id', 'string'),
    'Time': ('datetime', 'string'),
    'Latitude': ('latitude', 'float'),
    'Longitude': ('longitude', 'float'),
}
````

conv_varnames - a dictionary containing lists of the output variable names for IODA
conv_gsivarnames - a dictionary containing lists of the input variable names from GSI
````
conv_varnames = {
    "tv": ["virtual_temperature"],
    "tsen": ["air_temperature"],
    "uv": ["eastward_wind", "northward_wind"],
    "ps": ["surface_pressure"],
    "q": ["specific_humidity"],
    "bend": ["bending_angle"],
    "refract": ["refractivity"],
}

conv_gsivarnames = {
    "tv": ["Observation"],
    "tsen": ["Observation"],
    "uv": ["u_Observation", "v_Observation"],
    "ps": ["Observation"],
    "q": ["Observation"],
    "bend": ["Observation"],
    "refract": ["Observation"],
}
````

gsi_add_vars(_uv) - additional fields from GSI for each variable that one may wish to add to the IODA observation files
````
gsi_add_vars = {
    'Forecast_adjusted': 'GsiHofXBc',
    'Forecast_unadjusted': 'GsiHofX',
    'Inverse_Observation_Error': 'GsiFinalObsError',
}
````
Note: that the output to the IODA observation file will be a variable named `air_temperature@GsiHofX` with input from
the field `Forecast_unadjusted` for example.

gsiint - list of gsi_add_vars that should be written out as integers

geovals_metadata_dict - location variables for the GeoVaLs files
````
geovals_metadata_dict = {
    'Latitude': 'latitude',
    'Longitude': 'longitude',
    'Time': 'time',
    'GSIName': 'GeoVaLsname',
}
````
rad_sensors - list of possible sensor names for radiance data.
oz_sensors

chan_metadata_dict - metadata for each channel of radiance data
````
chan_metadata_dict = {
    'sensor_chan': 'sensor_channel',
    'use_flag': 'gsi_use_flag',
    'frequency': 'sensor_band_central_radiation_frequency',
}
````
Note: that the output to the IODA observation file will be a variable named `sensor_channel@VarMetaData` with length = nchannels and with input from 
the field `sensor_chan` for example.

geovals_vars - dictionary containing the keys for GSI diag variable names and the value is the corresponding output name
for UFO.
````
geovals_vars = {
    'virtual_temperature': 'virtual_temperature',
    'height': 'height_above_mean_sea_level',
    'surface_roughness': 'surface_roughness_length',
    'surface_height': 'surface_geopotential_height',
    'landmask': 'Land_Fraction',
    'atmosphere_pressure_coordinate': 'air_pressure',
    'atmosphere_absorber_01': 'humidity_mixing_ratio',
    'atmosphere_absorber_02': 'mass_concentration_of_carbon_dioxide_in_air',
    'atmosphere_absorber_03': 'mass_concentration_of_ozone_in_air',
}
````
