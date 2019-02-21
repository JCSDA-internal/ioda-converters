loc_metadata_dict = {'Latitude':   'latitude',     \
                     'Longitude':  'longitude',    \
                     'Obs_Time':   'time',         \
	             'Sol_Zenith_Angle':  'sol_zenith_angle',  \
	             'Sol_Azimuth_Angle': 'sol_azimuth_angle', \
                     'Surface_type':  'surface_type', \
                     'MODIS_deep_blue_flag': 'modis_deep_blue_flag'}

geovals_metadata_dict = {'Latitude':   'latitude',     \
                         'Longitude':  'longitude',    \
                         'Obs_Time':   'time'}

chan_metadata_dict = {'sensor_chan':  'sensor_channel', \
                      'use_flag': 'gsi_use_flag',       \
                      'frequency':    'frequency',      \
                      'polarization': 'polarization',   \
                      'wavenumber': 'wavenumber'}

variables_dict = {'Observation'       : 'ObsValue',   \
                  'Obs_Minus_Forecast_adjusted' : 'GsiHofXBc', \
                  'Obs_Minus_Forecast_unadjusted' : 'GsiHofX', \
                  'GSI_QC_Flag' : 'GsiQC', \
                  'QC_Flag' : 'QC', \
                  'Observation_Error': 'GsiObsError'}

geovals_dict = {'air_temperature': 'air_temperature', \
                'air_pressure': 'air_pressure',               \
                'air_pressure_levels': 'air_pressure_levels', \
                'humidity_mixing_ratio': 'humidity_mixing_ratio', \
                'relative_humidity': 'relative_humidity', \
                'sulf': 'sulf', 'bc1': 'bc1', 'bc2': 'bc2',   \
                'oc1': 'oc1', 'oc2': 'oc2', 'dust1': 'dust1', \
                'dust2': 'dust2', 'dust3': 'dust3',           \
                'dust4': 'dust4', 'dust5': 'dust5',           \
                'seas1': 'seas1', 'seas2': 'seas2',           \
                'seas3': 'seas3', 'seas4': 'seas4',           \
                'sfc_height': 'sfc_height'}
