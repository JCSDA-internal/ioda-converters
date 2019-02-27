
metadata_dict = {'Station_ID': 'station_id',   \
                 'Latitude':   'latitude',     \
                 'Longitude':  'longitude',    \
                 'Station_Elevation': 'station_elevation', \
                 'Pressure':   'air_pressure', \
                 'Height':     'height',       \
                 'Time':       'time',         \
                 'Wind_Reduction_Factor_at_10m': 'gsi_wind_red_factor'}

geovals_metadata_dict = {'Latitude':   'latitude',     \
                         'Longitude':  'longitude',    \
                         'Time':       'time'}

variables_dict = {'Observation_Type'  : 'ObsType',    \
                  'Prep_QC_Mark'      : 'PreQC',      \
                  'Prep_Use_Flag'     : 'PreUseFlag', \
                  'Analysis_Use_Flag' : 'GsiUseFlag', \
                  'Nonlinear_QC_Rel_Wgt' : 'GsiQCWeight', \
                  'Errinv_Input'      : 'ObsError',   \
                  'Errinv_Adjust'     : 'GsiAdjustObsError', \
                  'Errinv_Final'      : 'GsiFinalObsError',  \
                  'Observation'       : 'ObsValue',   \
                  'Obs_Minus_Forecast_adjusted' : 'GsiHofXBc', \
                  'Obs_Minus_Forecast_unadjusted' : 'GsiHofX'}

uv_variables_dict = {'u_Observation': 'eastward_wind@ObsValue',  \
                     'v_Observation': 'northward_wind@ObsValue', \
                     'u_Obs_Minus_Forecast_adjusted' : 'eastward_wind@GsiHofXBc',   \
                     'v_Obs_Minus_Forecast_adjusted' : 'northward_wind@GsiHofXBc',  \
                     'u_Obs_Minus_Forecast_unadjusted' : 'eastward_wind@GsiHofX',   \
                     'v_Obs_Minus_Forecast_unadjusted' : 'northward_wind@GsiHofX'}

geovals_vars  = ('virtual_temperature', 'atmosphere_ln_pressure_coordinate', \
                 'air_temperature', 'specific_humidity', 'northward_wind',   \
                 'eastward_wind', 'geopotential_height', 'height', 'surface_pressure')

