sensors = [
	'gome',
	'sbuv2',
]

LocKeyList = {
	'Latitude':   ('latitude','float'),
	'Longitude':  ('longitude','float'),
	'Reference_Pressure': ('air_pressure','float'),
	'Time': ('date_time','string'),
	'Scan_Position':('scan_position','float'),
	'Solar_Zenith_Angle':('solar_zenith_angle','float'),
	'Row_Anomaly_Index':('row_anomaly_index','float'),
}

geovals_metadata_dict = {
	'Latitude':   'latitude',
	'Longitude':  'longitude',
	'Time':   'time',
}

gsi_add_vars = {
	'Inverse_Observation_Error': 'GsiFinalObsError',
	'Obs_Minus_Forecast_adjusted' : 'GsiHofXBc',
	'Obs_Minus_Forecast_unadjusted' : 'GsiHofX',
}

geovals_vars = {
	'air_pressure_levels': 'air_pressure_levels',
	'mass_concentration_of_ozone_in_air': 'mass_concentration_of_ozone_in_air',
}

