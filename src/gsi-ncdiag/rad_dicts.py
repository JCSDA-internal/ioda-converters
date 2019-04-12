sensors = [
	'airs',
	'amsua',
	'atms',
	'hirs4',
	'iasi',
	'mhs',
	'seviri',
	'sndrd1','sndrd2','sndrd3','sndrd4',
        'cris-fsr',
	]

# LocKeyList = { 'gsiname':('IODAname','dtype')}
LocKeyList  = {
	'Latitude':   ('latitude','float'),
	'Longitude':  ('longitude','float'),
	'Elevation':  ('height_above_mean_sea_level','float'),
	'Obs_Time':   ('date_time','string'),
	'Scan_Position':     ('scan_position','float'),
	'Sat_Zenith_Angle':  ('sensor_zenith_angle','float'),
	'Sat_Azimuth_Angle': ('sensor_azimuth_angle','float'),
	'Sol_Zenith_Angle':  ('solar_zenith_angle','float'),
	'Sol_Azimuth_Angle': ('solar_azimuth_angle','float'),
	'Scan_Angle':        ('sensor_view_angle','float'),
}

chan_metadata_dict = {
	'sensor_chan':  'sensor_channel',
	'use_flag': 'gsi_use_flag',
	'frequency':    'sensor_band_central_radiation_frequency',
	'polarization': 'polarization',
	'wavenumber': 'sensor_band_central_radiation_wavenumber',
	'error_variance': 'ObsError',
	'mean_lapse_rate': 'mean_lapse_rate',
}

#variables_dict = {'Observation'       : 'ObsValue',   \
#                  'Obs_Minus_Forecast_adjusted' : 'GsiBc', \
#                  'Obs_Minus_Forecast_unadjusted' : 'GsiHofX', \
#                  'QC_Flag' : 'GsiQC', \
#                  'Inverse_Observation_Error': 'GsiFinalObsError'}

gsi_add_vars = {
        'Inverse_Observation_Error'      : 'GsiFinalObsError',
        'Obs_Minus_Forecast_adjusted' : 'GsiHofXBc',
        'Obs_Minus_Forecast_unadjusted' : 'GsiHofX',
}

geovals_metadata_dict = {
	'Latitude':   'latitude',
	'Longitude':  'longitude',
	'Obs_Time':   'time',
}

geovals_vars = {
	'air_temperature': 'air_temperature',
	'air_pressure': 'air_pressure', 
	'air_pressure_levels': 'air_pressure_levels',
	'atmosphere_absorber_01': 'humidity_mixing_ratio',
	'atmosphere_absorber_02': 'mass_concentration_of_carbon_dioxide_in_air',
	'atmosphere_absorber_03': 'mass_concentration_of_ozone_in_air',
	'atmosphere_mass_content_of_cloud_01':   'atmosphere_mass_content_of_cloud_liquid_water',
	'effective_radius_of_cloud_particle_01': 'effective_radius_of_cloud_liquid_water_particle',
	'atmosphere_mass_content_of_cloud_02':   'atmosphere_mass_content_of_cloud_ice',
	'effective_radius_of_cloud_particle_02': 'effective_radius_of_cloud_ice_particle',
	'Water_Fraction': 'Water_Fraction',
	'Land_Fraction':  'Land_Fraction',
	'Ice_Fraction': 'Ice_Fraction',
	'Snow_Fraction': 'Snow_Fraction',
	'Water_Temperature': 'Water_Temperature',
	'Land_Temperature':  'Land_Temperature',
	'Ice_Temperature':   'Ice_Temperature',
	'Snow_Temperature':  'Snow_Temperature',
	'Vegetation_Fraction': 'Vegetation_Fraction',
	'Sfc_Wind_Speed': 'Sfc_Wind_Speed',
	'Sfc_Wind_Direction': 'Sfc_Wind_Direction',
	'Lai': 'Lai',
	'Soil_Moisture': 'Soil_Moisture',
	'Soil_Temperature': 'Soil_Temperature',
	'Land_Type_Index': 'Land_Type_Index',
	'Vegetation_Type': 'Vegetation_Type',
	'Soil_Type': 'Soil_Type',
	'Snow_Depth': 'Snow_Depth',
	}

