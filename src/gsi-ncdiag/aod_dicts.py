sensors = [
	'modis',
	'viirs',
]

LocKeyList = {
	'Latitude':   ('latitude','float'),
	'Longitude':  ('longitude','float'),
	'Obs_Time': ('offset_from_analysis_time','float'),
	'Sol_Zenith_Angle':('solar_zenith_angle','float'),
	'Sol_Azimuth_Angle':('solar_azimuth_angle','float'),
	'Surface_type' : ('surface_type','integer'),
	'MODIS_deep_blue_flag' : ('modis_deep_blue_flag','integer'),
}

geovals_metadata_dict = {
	'Latitude':   'latitude',
	'Longitude':  'longitude',
	'Time':   'time',
}

gsi_add_vars = {
	'Obs_Minus_Forecast_adjusted' : 'GsiHofXBc',
	'Obs_Minus_Forecast_unadjusted' : 'GsiHofX',
}

chan_metadata_dict = {
	'sensor_chan':  'sensor_channel',
	'use_flag': 'gsi_use_flag',
	'frequency':    'sensor_band_central_radiation_frequency',
	'polarization': 'polarization',
	'wavenumber': 'sensor_band_central_radiation_wavenumber',
}

geovals_vars = {
	'air_temperature': 'air_temperature',
	'air_pressure': 'air_pressure', 
	'air_pressure_levels': 'air_pressure_levels',
	'humidity_mixing_ratio': 'humidity_mixing_ratio',
	'sulf': 'sulf',
	'bc1': 'bc1',
	'bc2': 'bc2', 
	'oc1': 'oc1',
	'oc2': 'oc2',
	'dust1': 'dust1',
	'dust2': 'dust2',
	'dust3': 'dust3',
	'dust4': 'dust4',
	'dust5': 'dust5',
	'seas1': 'seas1',
	'seas2': 'seas2',
	'seas3': 'seas3',
	'seas4': 'seas4',
	'Sfc_height': 'Sfc_height',
}
