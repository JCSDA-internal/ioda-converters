
loc_metadata_dict = {'Latitude':   'latitude',     \
                     'Longitude':  'longitude',    \
                     'Elevation':  'elevation',    \
                     'Obs_Time':   'time',         \
                     'Scan_Position':     'scan_position',     \
	             'Sat_Zenith_Angle':  'sat_zenith_angle',  \
	             'Sat_Azimuth_Angle': 'sat_azimuth_angle', \
	             'Sol_Zenith_Angle':  'sol_zenith_angle',  \
	             'Sol_Azimuth_Angle': 'sol_azimuth_angle', \
	             'Scan_Angle':        'scan_angle'}

geovals_metadata_dict = {'Latitude':   'latitude',     \
                         'Longitude':  'longitude',    \
                         'Obs_Time':   'time'}

chan_metadata_dict = {'sensor_chan':  'sensor_channel', \
                      'use_flag': 'gsi_use_flag',       \
                      'frequency':    'frequency',      \
                      'polarization': 'polarization',   \
                      'wavenumber': 'wavenumber',       \
                      'error_variance': 'ObsError',     \
                      'mean_lapse_rate': 'mean_lapse_rate'}

variables_dict = {'Observation'       : 'ObsValue',   \
                  'Obs_Minus_Forecast_adjusted' : 'GsiBc', \
                  'Obs_Minus_Forecast_unadjusted' : 'GsiHofX', \
                  'QC_Flag' : 'GsiQC', \
                  'Inverse_Observation_Error': 'GsiFinalObsError'}

geovals_dict = {'air_temperature': 'air_temperature', \
                'air_pressure': 'air_pressure',               \
                'air_pressure_levels': 'air_pressure_levels', \
        'atmosphere_absorber_01': 'humidity_mixing_ratio', \
        'atmosphere_absorber_02': 'mass_concentration_of_carbon_dioxide_in_air', \
        'atmosphere_absorber_03': 'mass_concentration_of_ozone_in_air', \
        'atmosphere_mass_content_of_cloud_01':   'atmosphere_mass_content_of_cloud_liquid_water',   \
        'effective_radius_of_cloud_particle_01': 'effective_radius_of_cloud_liquid_water_particle', \
        'atmosphere_mass_content_of_cloud_02':   'atmosphere_mass_content_of_cloud_ice', \
        'effective_radius_of_cloud_particle_02': 'effective_radius_of_cloud_ice_particle', \
        'Water_Fraction': 'Water_Fraction',\
        'Land_Fraction':  'Land_Fraction', \
        'Ice_Fraction': 'Ice_Fraction', \
        'Snow_Fraction': 'Snow_Fraction', \
        'Water_Temperature': 'Water_Temperature', \
        'Land_Temperature':  'Land_Temperature',  \
        'Ice_Temperature':   'Ice_Temperature',   \
        'Snow_Temperature':  'Snow_Temperature',  \
        'Vegetation_Fraction': 'Vegetation_Fraction', \
        'Sfc_Wind_Speed': 'Sfc_Wind_Speed', \
        'Sfc_Wind_Direction': 'Sfc_Wind_Direction', \
        'Lai': 'Lai', \
        'Soil_Moisture': 'Soil_Moisture', \
        'Soil_Temperature': 'Soil_Temperature', \
        'Land_Type_Index': 'Land_Type_Index', \
        'Vegetation_Type': 'Vegetation_Type', \
        'Soil_Type': 'Soil_Type', \
        'Snow_Depth': 'Snow_Depth'}

