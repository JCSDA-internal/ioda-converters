conv_platforms = {
	"conv_ps":[
		'sfc',
		'sondes',
		'sfcship',
		],
	"conv_q":[
		'aircraft',
		'sondes',
		'sfcship',
		'sfc',
		],
	"conv_t":[
		'aircraft',
		'sondes',
		'rass',
		'sfcship',
		'sfc',
		],
	"conv_uv":[
		'aircraft',
		'sondes',
		'satwind',
		'vadwind',
		'windprof',
		'sfcship',
		'sfc',
		'scatwind',
		],
	"conv_gps":[
		'gps',
		]
}


# note in python range, last number is not used so second values are +1 bufr codes
uv_bufrtypes = {
	"aircraft":range(230,240),
	"sondes":range(220,223),
	"satwind":range(240,261),
	"vadwind":[224],
	"windprof":range(227,230),
	"sfcship":[280,282,284],
	"sfc":[281,287],
	"scatwind":[290],
}

bufrtypes = { 
        "aircraft":range(130,140),
        "sondes":range(120,123),
        "rass":[126],
        "sfcship":[180,183],
        "sfc":[181,187],
	"gps":[3,4,745],
}

# LocKeyList = { 'gsiname':('IODAname','dtype')}
LocKeyList = {
		'Station_ID':	('station_id','string'),
		'Time':		('offset_from_analysis_time','float'),
		'Latitude':	('latitude','float'),
		'Longitude':	('longitude','float'),
		'Station_Elevation': ('station_elevation','float'),
		'Pressure':	('air_pressure','float'),
		'Height':	('height','float'),
		}

varnames = {
	"tv"  : ["virtual_temperature"],
	"tsen": ["air_temperature"],
	"uv"  : ["eastward_wind", "northward_wind"],
	"ps"  : ["surface_pressure"],
	"q"   : ["specific_humidity"],
	"bend" : ["bending_angle"],
	"refract": ["refractivity"],
}

gsivarnames = {
	"tv"  : ["Observation"],
	"tsen": ["Observation"],
	"uv"  : ["u_Observation","v_Observation"],
	"ps"  : ["Observation"],
	"q"   : ["Observation"],
	"bend" : ["Observation"],
	"refract": ["Observation"],
}

#metadata_dict = {
#	'Station_ID': 'station_id',
#	'Latitude':   'latitude',
#	'Longitude':  'longitude',
#	'Station_Elevation': 'station_elevation',
#	'Pressure':   'air_pressure',
#	'Height':     'height',
#	'Time':       'time',
#	'Wind_Reduction_Factor_at_10m': 'gsi_wind_red_factor',
#}

#geovals_metadata_dict = {
#	'Latitude':   'latitude',
#	'Longitude':  'longitude',
#	'Time':       'time',
#}
gsi_add_vars = {
	'Observation_Type'  : 'ObsType',
	'Prep_Use_Flag'     : 'PreUseFlag',
	'Analysis_Use_Flag' : 'GsiUseFlag',
	'Nonlinear_QC_Rel_Wgt' : 'GsiQCWeight',
	'Errinv_Adjust'     : 'GsiAdjustObsError',
	'Errinv_Final'      : 'GsiFinalObsError',
	'Obs_Minus_Forecast_adjusted' : 'GsiHofXBc',
	'Obs_Minus_Forecast_unadjusted' : 'GsiHofX',
}

#variables_dict = {
#	'Observation_Type'  : 'ObsType',
#	'Prep_QC_Mark'      : 'PreQC',
#	'Prep_Use_Flag'     : 'PreUseFlag',
#	'Analysis_Use_Flag' : 'GsiUseFlag',
#	'Nonlinear_QC_Rel_Wgt' : 'GsiQCWeight',
#	'Errinv_Input'      : 'ObsError',
#	'Errinv_Adjust'     : 'GsiAdjustObsError',
#	'Errinv_Final'      : 'GsiFinalObsError',
#	'Observation'       : 'ObsValue', 
#	'Obs_Minus_Forecast_adjusted' : 'GsiHofXBc',
#	'Obs_Minus_Forecast_unadjusted' : 'GsiHofX',
#}
#
#uv_variables_dict = {
#	'u_Observation': 'eastward_wind@ObsValue',
#	'v_Observation': 'northward_wind@ObsValue',
#	'u_Obs_Minus_Forecast_adjusted' : 'eastward_wind@GsiHofXBc',
#	'v_Obs_Minus_Forecast_adjusted' : 'northward_wind@GsiHofXBc',
#	'u_Obs_Minus_Forecast_unadjusted' : 'eastward_wind@GsiHofX',
#	'v_Obs_Minus_Forecast_unadjusted' : 'northward_wind@GsiHofX',
#}
#
#geovals_vars  = (
#	'virtual_temperature', 'atmosphere_ln_pressure_coordinate',
#	'air_temperature', 'specific_humidity', 'northward_wind',
#	'eastward_wind', 'geopotential_height', 'height', 'surface_pressure',
#)

