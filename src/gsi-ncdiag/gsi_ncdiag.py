# gsi_ncdiag.py
# a collection of classes, and supporting information
# to read in GSI netCDF diagnostic files and rewrite them
# into JEDI UFO GeoVaLs and IODA observation files
###############################################################################
###############################################################################
# dictionaries and lists

import os
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

import numpy as np
import datetime as dt
import netCDF4 as nc

import ioda_conv_ncio as iconv

__ALL__ = ['conv_platforms']

conv_platforms = {
    "conv_ps": [
        'sfc',
        'sondes',
        'sfcship',
    ],
    "conv_q": [
        'aircraft',
        'sondes',
        'sfcship',
        'sfc',
    ],
    "conv_t": [
        'aircraft',
        'sondes',
        'rass',
        'sfcship',
        'sfc',
    ],
    "conv_uv": [
        'sfc',
        'aircraft',
        'sondes',
        'vadwind',
        'windprof',
        'sfcship',
        'satwind',
        'scatwind',
    ],
    "conv_gps": [
        'gps',
    ],
    "conv_sst": [
        'sst',
    ]
}

# note in python range, last number is not used so second values are +1
# bufr codes
uv_bufrtypes = {
    "aircraft": range(230, 240),
    "sondes": range(220, 223),
    "satwind": range(240, 261),
    "vadwind": [224],
    "windprof": range(227, 230),
    "sfcship": [280, 282, 284],
    "sfc": [281, 287],
    "scatwind": [290],
}

conv_bufrtypes = {
    "aircraft": range(130, 140),
    "sondes": range(120, 123),
    "rass": [126],
    "sfcship": [180, 183],
    "sfc": [181, 187],
    "gps": [3, 4, 42, 43, 745, 825],
    "sst": [181, 182, 183, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202],
}

# LocKeyList = { 'gsiname':('IODAname','dtype')}
all_LocKeyList = {
    'Station_ID': ('station_id', 'string'),
    'Time': ('datetime', 'string'),
    'Latitude': ('latitude', 'float'),
    'Longitude': ('longitude', 'float'),
    'Station_Elevation': ('station_elevation', 'float'),
    'Pressure': ('air_pressure', 'float'),
    'Height': ('height', 'float'),
    'Elevation': ('height_above_mean_sea_level', 'float'),
    'Obs_Time': ('datetime', 'string'),
    'Scan_Position': ('scan_position', 'float'),
    'Sat_Zenith_Angle': ('sensor_zenith_angle', 'float'),
    'Sat_Azimuth_Angle': ('sensor_azimuth_angle', 'float'),
    'Sol_Zenith_Angle': ('solar_zenith_angle', 'float'),
    'Sol_Azimuth_Angle': ('solar_azimuth_angle', 'float'),
    'Scan_Angle': ('sensor_view_angle', 'float'),
    'Surface_type': ('surface_type', 'integer'),
    'MODIS_deep_blue_flag': ('modis_deep_blue_flag', 'integer'),
    'Reference_Pressure': ('air_pressure', 'float'),
    'Solar_Zenith_Angle': ('solar_zenith_angle', 'float'),
    'Row_Anomaly_Index': ('row_anomaly_index', 'float'),
    'TopLevelPressure': ('top_level_pressure', 'float'),
    'BottomLevelPressure': ('bottom_level_pressure', 'float'),
    'XoverR': ('radar_azimuth', 'float'),
    'YoverR': ('radar_tilt', 'float'),
    'ZoverR': ('radar_dir3', 'float'),
    'Vterminal': ('vterminal', 'float'),
}

checkuv = {
    "eastward_wind": "u",
    "northward_wind": "v",
}

conv_varnames = {
    "tv": ["virtual_temperature"],
    "tsen": ["air_temperature"],
    "uv": ["eastward_wind", "northward_wind"],
    "ps": ["surface_pressure"],
    "q": ["specific_humidity"],
    "bend": ["bending_angle"],
    "refract": ["refractivity"],
    "sst": ["sea_surface_temperature"],
}

conv_gsivarnames = {
    "tv": ["Observation"],
    "tsen": ["Observation"],
    "uv": ["u_Observation", "v_Observation"],
    "ps": ["Observation"],
    "q": ["Observation"],
    "bend": ["Observation"],
    "refract": ["Observation"],
    "sst": ["Observation"],
}

gsi_add_vars_allsky = {
    'Observation_Type': 'ObsType',
    'Prep_Use_Flag': 'PreUseFlag',
    'Analysis_Use_Flag': 'GsiUseFlag',
    'Nonlinear_QC_Rel_Wgt': 'GsiQCWeight',
    'Errinv_Adjust': 'GsiAdjustObsError',
    'Errinv_Final': 'GsiFinalObsError',
    'Forecast_adjusted': 'GsiHofXBc',
    'Forecast_unadjusted': 'GsiHofX',
    'Forecast_unadjusted_clear': 'GsiHofXClr',
    'Inverse_Observation_Error': 'GsiFinalObsError',
    'Bias_Correction': 'GsiBc',
    'hxdbz': 'GsiHofX',
    'hxrw': 'GsiHofX',
}

gsi_add_qcvars_allsky = {
    'ObsBias': 'GsiObsBias',
    'Inverse_Observation_Error_after_jsfcchk': 'GsiObsError_after_jsfcchk',
    'Inverse_Observation_Error_after_sdoei': 'GsiObsError_after_sdoei',
    'Inverse_Observation_Error_after_grosschk': 'GsiObsError_after_grosschk',
    'Inverse_Observation_Error_sdoei': 'GsiObsError_sdoei',
    'Inverse_Observation_Error_grosschk': 'GsiObsError_grosschk',
}

gsi_add_vars = {
    'ObsBias': 'GsiObsBias',
    'Observation_Type': 'ObsType',
    'Prep_Use_Flag': 'PreUseFlag',
    'Analysis_Use_Flag': 'GsiUseFlag',
    'Nonlinear_QC_Rel_Wgt': 'GsiQCWeight',
    'Errinv_Adjust': 'GsiAdjustObsError',
    'Errinv_Final': 'GsiFinalObsError',
    'Forecast_adjusted': 'GsiHofXBc',
    'Forecast_unadjusted': 'GsiHofX',
    'Inverse_Observation_Error': 'GsiFinalObsError',
    'Bias_Correction': 'GsiBc',
    'hxdbz': 'GsiHofX',
    'hxrw': 'GsiHofX',
}

gsi_add_qcvars = {
    'Bias_Correction': 'GsiObsBias',
    'Inverse_Observation_Error_jsfc': 'GsiObsError_jsfc',
    'Inverse_Observation_Error_clddet': 'GsiObsError_clddet',
    'Inverse_Observation_Error_nsstret': 'GsiObsError_nsstret',
    'Inverse_Observation_Error_grosschk': 'GsiObsError_grosschk',
    'Inverse_Observation_Error_after_wavenum': 'GsiObsError_after_wavenum',
    'Inverse_Observation_Error_after_range': 'GsiObsError_after_rangechk',
    'Inverse_Observation_Error_after_topo': 'GsiObsError_after_topo',
    'Inverse_Observation_Error_after_transmittop': 'GsiObsError_after_transmittop',
    'Inverse_Observation_Error_after_clddet': 'GsiObsError_after_clddet',
    'Inverse_Observation_Error_after_nsstret': 'GsiObsError_after_nsstret',
    'Inverse_Observation_Error_after_jsfcchk': 'GsiObsError_after_jsfcchk',
}

gsi_add_vars_uv = {
    'Observation_Type': 'ObsType',
    'Prep_Use_Flag': 'PreUseFlag',
    'Analysis_Use_Flag': 'GsiUseFlag',
    'Nonlinear_QC_Rel_Wgt': 'GsiQCWeight',
    'Errinv_Adjust': 'GsiAdjustObsError',
    'Errinv_Final': 'GsiFinalObsError',
    'u_Forecast_adjusted': 'GsiHofXBc',
    'u_Forecast_unadjusted': 'GsiHofX',
    'v_Forecast_adjusted': 'GsiHofXBc',
    'v_Forecast_unadjusted': 'GsiHofX',
}

radar_qc = {
    'obsdbz': 'dbzuse',
    'obsrw': 'rwuse',
}

radar_err = {
    'obsdbz': 'dbzerror',
    'obsrw': 'rwerror',
}

# values that should be integers
gsiint = [
    'PreUseFlag',
    'GsiUseFlag',
    'ObsType',
    'Analysis_Use_Flag',
]

geovals_metadata_dict = {
    'Latitude': 'latitude',
    'Longitude': 'longitude',
    'Time': 'time',
    'Obs_Time': 'time',
}

obsdiag_metadata_dict = {
    'Latitude': 'latitude',
    'Longitude': 'longitude',
    'Time': 'time',
    'Obs_Time': 'time',
}

rad_sensors = [
    'airs',
    'amsua',
    'atms',
    'hirs4',
    'iasi',
    'mhs',
    'seviri',
    'sndrd1', 'sndrd2', 'sndrd3', 'sndrd4',
    'cris-fsr',
    'ssmis',
    'abi',
    'ahi',
    'avhrr',
    'saphir',
]

radar_sensors = [
    'radar',
]

chan_metadata_dict = {
    'sensor_chan': 'sensor_channel',
    'use_flag': 'gsi_use_flag',
    'frequency': 'sensor_band_central_radiation_frequency',
    'polarization': 'polarization',
    'wavenumber': 'sensor_band_central_radiation_wavenumber',
    'error_variance': 'ObsError',
    'mean_lapse_rate': 'mean_lapse_rate',
}

# geovals_vars = {gsiname:geoval_name}
geovals_vars = {
    'virtual_temperature': 'virtual_temperature',
    'atmosphere_ln_pressure_coordinate': 'atmosphere_ln_pressure_coordinate',
    'specific_humidity': 'specific_humidity',
    'northward_wind': 'northward_wind',
    'eastward_wind': 'eastward_wind',
    'geopotential_height': 'geopotential_height',
    'height': 'height_above_mean_sea_level',
    'tropopause_pressure': 'tropopause_pressure',
    'surface_pressure': 'surface_pressure',
    'surface_temperature': 'surface_temperature',
    'sea_surface_temperature': 'sea_surface_temperature',
    'surface_roughness': 'surface_roughness_length',
    'surface_height': 'surface_geopotential_height',
    'landmask': 'land_area_fraction',
    'air_temperature': 'air_temperature',
    'air_pressure': 'air_pressure',
    'atmosphere_pressure_coordinate': 'air_pressure',
    'atmosphere_pressure_coordinate_interface': 'air_pressure_levels',
    'air_pressure_levels': 'air_pressure_levels',
    'atmosphere_absorber_01': 'humidity_mixing_ratio',
    'atmosphere_absorber_02': 'mole_fraction_of_carbon_dioxide_in_air',
    'atmosphere_absorber_03': 'mole_fraction_of_ozone_in_air',
    'atmosphere_mass_content_of_cloud_01': 'mass_content_of_cloud_liquid_water_in_atmosphere_layer',
    'effective_radius_of_cloud_particle_01': 'effective_radius_of_cloud_liquid_water_particle',
    'atmosphere_mass_content_of_cloud_02': 'mass_content_of_cloud_ice_in_atmosphere_layer',
    'effective_radius_of_cloud_particle_02': 'effective_radius_of_cloud_ice_particle',
    'Water_Fraction': 'water_area_fraction',
    'Land_Fraction': 'land_area_fraction',
    'Ice_Fraction': 'ice_area_fraction',
    'Snow_Fraction': 'surface_snow_area_fraction',
    'Vegetation_Fraction': 'vegetation_area_fraction',
    'Water_Temperature': 'surface_temperature_where_sea',
    'Land_Temperature': 'surface_temperature_where_land',
    'Ice_Temperature': 'surface_temperature_where_ice',
    'Snow_Temperature': 'surface_temperature_where_snow',
    'tsavg5': 'average_surface_temperature_within_field_of_view',
    'Sfc_Wind_Speed': 'surface_wind_speed',
    'Sfc_Wind_Direction': 'surface_wind_from_direction',
    'Lai': 'leaf_area_index',
    'Soil_Moisture': 'volume_fraction_of_condensed_water_in_soil',
    'Soil_Temperature': 'soil_temperature',
    'Land_Type_Index': 'land_type_index',
    'Vegetation_Type': 'vegetation_type_index',
    'Soil_Type': 'soil_type',
    'Snow_Depth': 'surface_snow_thickness',
    'humidity_mixing_ratio': 'humidity_mixing_ratio',
    'Sfc_Height': 'surface_geopotential_height',
    'mass_concentration_of_ozone_in_air': 'mole_fraction_of_ozone_in_air',
    'Wind_Reduction_Factor_at_10m': 'wind_reduction_factor_at_10m',
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
    'dbzges': 'equivalent_reflectivity_factor',
    'upward_air_velocity': 'upward_air_velocity',
}

obsdiag_vars = {
    'Jacobian_Surface_Temperature': 'brightness_temperature_jacobian_surface_temperature',
    'Jacobian_Surface_Emissivity': 'brightness_temperature_jacobian_surface_emissivity',
    'Jacobian_Temperature': 'brightness_temperature_jacobian_air_temperature',
    'Jacobian_Moisture': 'brightness_temperature_jacobian_humidity_mixing_ratio',
    'Layer_Optical_Depth': 'optical_thickness_of_atmosphere_layer',
    'Layer_to_Space_Transmittance': 'transmittances_of_atmosphere_layer',
    'Weighting_Function': 'weightingfunction_of_atmosphere_layer',
    'Pressure_Level_WeightFuncMax': 'pressure_level_at_peak_of_weightingfunction',
    'Forecast_unadjusted_clear': 'brightness_temperature_assuming_clear_sky',
}

aod_sensors = [
    'modis',
    'viirs',
]

oz_sensors = [
    'gome',
    'sbuv2',
    'omi',
    'ompsnp',
    'ompstc8',
]

# units
# 'IODA/UFO_variable_name': 'Unit'
units_values = {
    'virtual_temperature': 'K',
    'atmosphere_ln_pressure_coordinate': '1',
    'specific_humidity': '1',
    'northward_wind': 'm s-1',
    'eastward_wind': 'm s-1',
    'geopotential_height': 'm',
    'height_above_mean_sea_level': 'm',
    'surface_pressure': 'Pa',
    'sea_surface_temperature': 'K',
    'surface_temperature': 'K',
    'surface_roughness_length': 'm',
    'surface_geopotential_height': 'm',
    'land_area_fraction': '1',
    'air_temperature': 'K',
    'air_pressure': 'Pa',
    'air_pressure_levels': 'Pa',
    'humidity_mixing_ratio': '1',
    'mole_fraction_of_carbon_dioxide_in_air': '1',
    'mole_fraction_of_ozone_in_air': '1',
    'atmosphere_mass_content_of_cloud_liquid_water': 'kg m-2',
    'effective_radius_of_cloud_liquid_water_particle': 'm',
    'atmosphere_mass_content_of_cloud_ice': 'kg m-2',
    'effective_radius_of_cloud_ice_particle': 'm',
    'water_area_fraction': '1',
    'land_area_fraction': '1',
    'ice_area_fraction': '1',
    'surface_snow_area_fraction': '1',
    'vegetation_area_fraction': '1',
    'surface_temperature_where_sea': 'K',
    'surface_temperature_where_land': 'K',
    'surface_temperature_where_ice': 'K',
    'surface_temperature_where_snow': 'K',
    'surface_wind_speed': 'm s-1',
    'wind_speed': 'm s-1',
    'surface_wind_from_direction': 'degree',
    'leaf_area_index': '1',
    'volume_fraction_of_condensed_water_in_soil': '1',
    'soil_temperature': 'K',
    'land_type_index': '1',
    'vegetation_type_index': '1',
    'soil_type': '1',
    'surface_snow_thickness': 'm',
    'humidity_mixing_ratio': '1',
    'wind_reduction_factor_at_10m': '1',
    'sulf': '1',
    'bc1': '1',
    'bc2': '1',
    'oc1': '1',
    'oc2': '1',
    'dust1': '1',
    'dust2': '1',
    'dust3': '1',
    'dust4': '1',
    'dust5': '1',
    'seas1': '1',
    'seas2': '1',
    'seas3': '1',
    'seas4': '1',
    'latitude': 'degrees_north',
    'longitude': 'degrees_east',
    'station_elevation': 'm',
    'height': 'm',
    'height_above_mean_sea_level': 'm',
    'scan_position': '1',
    'sensor_zenith_angle': 'degree',
    'sensor_azimuth_angle': 'degree',
    'solar_zenith_angle': 'degree',
    'solar_azimuth_angle': 'degree',
    'modis_deep_blue_flag': '1',
    'row_anomaly_index': '1',
    'top_level_pressure': 'Pa',
    'bottom_level_pressure': 'Pa',
    'tropopause_pressure': 'Pa',
    'brightness_temperature_jacobian_surface_temperature': '1',
    'brightness_temperature_jacobian_surface_emissivity': 'K',
    'brightness_temperature_jacobian_air_temperature': '1',
    'brightness_temperature_jacobian_humidity_mixing_ratio': 'K/g/Kg ',
    'optical_thickness_of_atmosphere_layer': '1',
    'clw_retrieved_from_observation': 'kg/m/m',
    'clw_retrieved_from_background': 'kg/m/m',
    'scat_retrieved_from_observation': '1',
}

# @TestReference
# fields from GSI to compare to computations done in UFO
# Note: For conventional data, the combine script would fail if the
# input subset files (_m, and _s) contain test variables.
test_fields_conv = {
    'wind_speed': ('wind_speed', 'float'),
}

test_fields = {}

test_fields_allsky = {
    'clwp_amsua': ('clw_retrieved_from_observation', 'float'),
    'clw_guess_retrieval': ('clw_retrieved_from_background', 'float'),
    'clw_symmetric_amount': ('clw_symmetric_amount', 'float'),
    'scat_amsua': ('scat_retrieved_from_observation', 'float'),
}
test_fields_with_channels_allsky = {
    'BC_Constant': ('constant_bias_correction_term', 'float'),
    'BC_Scan_Angle': ('scan_angle_bias_correction_term', 'float'),
    'BC_Cloud_Liquid_Water': ('cloud_liquid_water_bias_correction_term', 'float'),
    'BC_Lapse_Rate_Squared': ('lapse_rate_squared_bias_correction_term', 'float'),
    'BC_Lapse_Rate': ('lapse_rate_bias_correction_term', 'float'),
    'BC_Cosine_Latitude_times_Node': ('cosine_of_latitude_times_orbit_node_bias_correction_term', 'float'),
    'BC_Sine_Latitude': ('sine_of_latitude_bias_correction_term', 'float'),
    'BC_Emissivity': ('emissivity_bias_correction_term', 'float'),
    'BC_Scan_Angle_4th_order': ('scan_angle_4th_order_bias_correction_term', 'float'),
    'BC_Scan_Angle_3rd_order': ('scan_angle_3rd_order_bias_correction_term', 'float'),
    'BC_Scan_Angle_2nd_order': ('scan_angle_2nd_order_bias_correction_term', 'float'),
    'BC_Scan_Angle_1st_order': ('scan_angle_1st_order_bias_correction_term', 'float'),
    'BC_Scan_Position_total': ('scan_angle_total_bias_correction_term', 'float'),
    'BCPred_Constant': ('constant_bias_correction_predictor', 'float'),
    'BCPred_Scan_Angle': ('scan_angle_bias_correction_predictor', 'float'),
    'BCPred_Cloud_Liquid_Water': ('cloud_liquid_water_bias_correction_predictor', 'float'),
    'BCPred_Lapse_Rate_Squared': ('lapse_rate_squared_bias_correction_predictor', 'float'),
    'BCPred_Lapse_Rate': ('lapse_rate_bias_correction_predictor', 'float'),
    'BCPred_Cosine_Latitude_times_Node': ('cosine_of_latitude_times_orbit_node_bias_correction_predictor', 'float'),
    'BCPred_Sine_Latitude': ('sine_of_latitude_bias_correction_predictor', 'float'),
    'BCPred_Emissivity': ('emissivity_bias_correction_predictor', 'float'),
    'BCPred_Scan_Angle_4th_order': ('scan_angle_4th_order_bias_correction_predictor', 'float'),
    'BCPred_Scan_Angle_3rd_order': ('scan_angle_3rd_order_bias_correction_predictor', 'float'),
    'BCPred_Scan_Angle_2nd_order': ('scan_angle_2nd_order_bias_correction_predictor', 'float'),
    'BCPred_Scan_Angle_1st_order': ('scan_angle_1st_order_bias_correction_predictor', 'float'),
    'Hydrometeor_Affected_Channels': ('Hydrometeor_Affected_Channels', 'float'),
    'Input_Observation_Error': ('ObsError', 'float'),
    'Cloud_Match_Index': ('Cloud_Match_Index', 'float'),
    'Error_Inflation_Factor_sdoei': ('error_inflation_factor_sdoei', 'float'),
}
test_fields_with_channels = {
    'Error_Inflation_Factor_Topo': ('error_inflation_factor_topo', 'float'),
    'Error_Inflation_Factor_Transmittop': ('error_inflation_factor_transmittop', 'float'),
    'Error_Inflation_Factor_Wavenum': ('error_inflation_factor_wavenum', 'float'),
    'Error_Inflation_Factor_Jsfc': ('error_inflation_factor_jsfc', 'float'),
    'Error_Inflation_Factor_Grosschk': ('error_inflation_factor_grosschk', 'float'),
    'Transmittance_at_Top': ('tao_top', 'float'),
    'Transmittance_at_Sfc': ('tao_sfc', 'float'),
    'Cloudy_Channel': ('cloudy_channel', 'integer'),
    'Transmittance_at_Cloud_Top': ('tao_cldtop', 'float'),
    'NSST_Retrieval_check': ('nsstret_check', 'integer'),
}


class BaseGSI:
    EPSILON = 9e-12
    FLOAT_FILL = nc.default_fillvals['f4']
    INT_FILL = nc.default_fillvals['i4']
    @staticmethod
    def _as_array(netcdf_var):
        return np.array(netcdf_var[:])

    def var(self, var_name):
        """
        Data array.  Return a numpy array based on variable name
        """
        return self._as_array(self.df[var_name])


# conventional observations
class Conv(BaseGSI):
    """ class Conv - conventional observations

                Use this class to read in conventional observations
                from GSI netCDF diag files

    Functions:

    Attributes:
      filename    - string path to file
      validtime   - datetime object of valid observation time
      nobs        - number of observations
    """
    def __init__(self, filename):
        self.filename = filename
        splitfname = self.filename.split('/')[-1].split('_')
        if 'conv' in splitfname:
            i = splitfname.index('conv')
            self.obstype = "_".join(splitfname[i:i + 2])
        else:
            raise ValueError("Observation is not a conventional type...")
        # below is because T has both T and Tv, others should just be 'value'
        # but flexibility for later (GPS?)
        if self.obstype == 'conv_t':
            self.obsvars = ['tv', 'tsen']
        elif self.obstype == 'conv_gps':
            self.obsvars = ['bend', 'refract']
        else:
            self.obsvars = [splitfname[i + 1]]

    def read(self):
        # get valid time
        df = nc.Dataset(self.filename)
        tstr = str(df.getncattr('date_time'))
        self.validtime = dt.datetime.strptime(tstr, "%Y%m%d%H")
        # number of observations
        self.nobs = len(df['Observation_Type'][:])
        self.df = df

    def close(self):
        self.df.close()

    def toGeovals(self, OutDir, clobber=True):
        """ toGeovals(OutDir,clobber=True)
        if model state fields are in the GSI diag file, create
        GeoVaLs in an output file for use by JEDI/UFO
        """
        # note, this is a temporary construct and thus, there is no
        # ioda_conv_ncio or equivalent to handle the format
        # get list of platforms to process for the given obstype
        try:
            platforms = conv_platforms[self.obstype]
        except BaseException:
            print(self.obstype + " is not currently supported. Exiting.")
            return
        # loop through obsvariables and platforms to do processing
        for v in self.obsvars:
            for p in platforms:
                outname = OutDir + '/' + p + '_' + v + '_geoval_' + \
                    self.validtime.strftime("%Y%m%d%H") + '.nc4'
                if (v == 'sst'):
                    outname = OutDir + '/' + v + '_geoval_' + \
                        self.validtime.strftime("%Y%m%d%H") + '.nc4'
                if (p == 'windprof' or p == 'satwind' or p == 'scatwind' or p == 'vadwind'):
                    outname = OutDir + '/' + p + '_geoval_' + \
                        self.validtime.strftime("%Y%m%d%H") + '.nc4'
                if not clobber:
                    if (os.path.exists(outname)):
                        print("File exists. Skipping and not overwriting:%s" % outname)
                        continue
                OutVars = []
                InVars = []
                for ncv in self.df.variables:
                    if ncv in geovals_vars:
                        OutVars.append(geovals_vars[ncv])
                        InVars.append(ncv)

                idx = grabobsidx(self.df, p, v)
                if (np.sum(idx) == 0):
                    print("No matching observations for Platform:%s Var:%s" % (p, v))
                    continue
                print("Platform:%s Var:%s #Obs:%d" % (p, v, np.sum(idx)))
                # set up output file
                ncout = nc.Dataset(outname, 'w', format='NETCDF4')
                ncout.setncattr(
                    "date_time", np.int32(
                        self.validtime.strftime("%Y%m%d%H")))
                # get nlocs
                nlocs = np.sum(idx)
                ncout.createDimension("nlocs", nlocs)
                # other dims
                if (v != "sst"):
                    ncout.createDimension(
                        "nlevs", self.df.dimensions["atmosphere_pressure_coordinate_arr_dim"].size)
                dimname = "Station_ID_maxstrlen"
                ncout.createDimension(dimname, self.df.dimensions[dimname].size)
                dimname = "Observation_Class_maxstrlen"
                ncout.createDimension(dimname, self.df.dimensions[dimname].size)
                for var in self.df.variables.values():
                    vname = var.name
                    if (vname in geovals_metadata_dict.keys()) or (
                            vname in geovals_vars.keys()):
                        vdata = var[...].data
                        dims = tuple([len(self.df.dimensions[d]) for d in var.dimensions])
                        vdata = np.frombuffer(vdata, dtype=var.dtype)
                        vdata = np.reshape(vdata, dims)
                        if vname in geovals_metadata_dict.keys():
                            dims = ("nlocs",) + var.dimensions[1:]
                            var_out = ncout.createVariable(geovals_metadata_dict[vname], vdata.dtype, dims)
                            var_out[...] = vdata[idx, ...]
                        if vname in geovals_vars.keys():
                            if (len(var.dimensions) == 1):
                                dims = ("nlocs",)
                            else:
                                dims = ("nlocs", "nlevs")
                            var_out = ncout.createVariable(geovals_vars[vname], vdata.dtype, dims)
                            var_out[...] = vdata[idx, ...]
                ncout.close()

    def toIODAobs(self, OutDir, clobber=True, platforms=None):
        """ toIODAobs(OutDir,clobber=True)
        output observations from the specified GSI diag file
        to the JEDI/IODA observation format
        """
        if not platforms:
            # get list of platforms to process for the given obstype
            try:
                platforms = conv_platforms[self.obstype]
            except BaseException:
                print(self.obstype + " is not currently supported. Exiting.")
                return
        # loop through obsvariables and platforms to do processing
        for v in self.obsvars:
            for p in platforms:
                # set up a NcWriter class
                outname = OutDir + '/' + p + '_' + v + '_obs_' + \
                    self.validtime.strftime("%Y%m%d%H") + '.nc4'
                if (v == 'sst'):
                    outname = OutDir + '/' + v + '_obs_' + \
                        self.validtime.strftime("%Y%m%d%H") + '.nc4'
                if (p == 'windprof' or p == 'satwind' or p == 'scatwind' or p == 'vadwind'):
                    outname = OutDir + '/' + p + '_obs_' + \
                        self.validtime.strftime("%Y%m%d%H") + '.nc4'
                if not clobber:
                    if (os.path.exists(outname)):
                        print("File exists. Skipping and not overwriting: %s" % outname)
                        continue
                LocKeyList = []
                TestKeyList = []
                LocVars = []
                TestVars = []
                AttrData = {}
                varDict = defaultdict(lambda: defaultdict(dict))
                outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
                loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
                var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
                test_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
                test_fields_ = test_fields_conv
                # get list of location variable for this var/platform
                for ncv in self.df.variables:
                    if ncv in all_LocKeyList:
                        LocKeyList.append(all_LocKeyList[ncv])
                        LocVars.append(ncv)
                # get list of TestReference variables for this var/platform
                for ncv in self.df.variables:
                    if ncv in test_fields_:
                        TestKeyList.append(test_fields_[ncv])
                        TestVars.append(ncv)
                # grab obs to process
                idx = grabobsidx(self.df, p, v)
                if (np.sum(idx) == 0):
                    print("No matching observations for Platform:%s Var:%s" % (p, v))
                    continue
                print("Platform:%s Var:%s #Obs:%d" % (p, v, np.sum(idx)))

                writer = iconv.NcWriter(outname, LocKeyList, TestKeyList=TestKeyList)

                outvars = conv_varnames[v]
                for value in outvars:
                    varDict[value]['valKey'] = value, writer.OvalName()
                    varDict[value]['errKey'] = value, writer.OerrName()
                    varDict[value]['qcKey'] = value, writer.OqcName()

                for o in range(len(outvars)):
                    obsdata = self.var(conv_gsivarnames[v][o])[idx]
                    obserr = self.var('Errinv_Input')[idx]
                    mask = obserr < self.EPSILON
                    obserr[~mask] = 1.0 / obserr[~mask]
                    obserr[mask] = self.FLOAT_FILL
                    obserr[obserr > 4e8] = self.FLOAT_FILL
                    try:
                        obsqc = self.var('Prep_QC_Mark')[idx]
                    except BaseException:
                        obsqc = np.ones_like(obsdata) * 2
                    if (v == 'uv'):
                        gsivars = gsi_add_vars_uv
                    else:
                        gsivars = gsi_add_vars

                    for key, value in gsivars.items():
                        if key in self.df.variables:
                            df_key = self.var(key)
                            gvname = outvars[o], value
                            # some special actions need to be taken depending on
                            # var name...
                            if ("Forecast" in key) and (v == 'uv'):
                                if (checkuv[outvars[o]] != key[0]):
                                    continue
                            if "Errinv" in key:
                                tmp = df_key[idx]
                                mask = tmp < self.EPSILON
                                tmp[~mask] = 1.0 / tmp[~mask]
                                tmp[mask] = self.FLOAT_FILL
                            else:
                                tmp = df_key[idx]
                            if value in gsiint:
                                tmp = tmp.astype(int)
                                tmp[tmp > 4e4] = self.INT_FILL
                            else:
                                tmp[tmp > 4e8] = self.FLOAT_FILL
                            outdata[gvname] = tmp
                    # store values in output data dictionary
                    outdata[varDict[outvars[o]]['valKey']] = obsdata
                    outdata[varDict[outvars[o]]['errKey']] = obserr
                    outdata[varDict[outvars[o]]['qcKey']] = obsqc.astype(int)

                for lvar in LocVars:
                    loc_mdata_name = all_LocKeyList[lvar][0]
                    if lvar == 'Station_ID':
                        tmp = self.var(lvar)[idx]
                        StationIDs = [b''.join(tmp[a]) for a in range(len(tmp))]
                        loc_mdata[loc_mdata_name] = writer.FillNcVector(StationIDs, "string")
                    elif lvar == 'Time':  # need to process into time stamp strings #"%Y-%m-%dT%H:%M:%SZ"
                        tmp = self.var(lvar)[idx]
                        obstimes = [self.validtime + dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                        obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                        loc_mdata[loc_mdata_name] = writer.FillNcVector(obstimes, "datetime")
                    # special logic for missing station_elevation and height for surface obs
                    elif lvar in ['Station_Elevation', 'Height']:
                        if p == 'sfc':
                            tmp = self.var(lvar)[idx]
                            tmp[tmp == 9999.] = self.FLOAT_FILL
                            tmp[tmp == 10009.] = self.FLOAT_FILL  # for u,v sfc Height values that are 10+9999
                            # GSI sfc obs are at 0m agl, but operator assumes 2m agl, correct output to 2m agl
                            # this is correctly 10m agl though for u,v obs
                            if lvar == 'Height' and self.obstype in ['conv_t', 'conv_q']:
                                elev = self.var('Station_Elevation')[idx]
                                hgt = elev + 2.
                                hgt[hgt > 9998.] = self.FLOAT_FILL
                                tmp = hgt
                            loc_mdata[loc_mdata_name] = tmp
                        elif p == 'sondes' or p == 'aircraft' or p == 'satwind':
                            tmp = self.var(lvar)[idx]
                            tmp[tmp > 4e8] = self.FLOAT_FILL  # 1e11 is fill value for sondes, etc.
                            loc_mdata[loc_mdata_name] = tmp
                        else:
                            loc_mdata[loc_mdata_name] = self.var(lvar)[idx]
                    else:
                        loc_mdata[loc_mdata_name] = self.var(lvar)[idx]
                # put the TestReference fields in the structure for writing out
                for tvar in TestVars:
                    if tvar in test_fields_:
                        test_mdata_name = test_fields_[tvar][0]
                        tmp = self.var(tvar)[idx]
                        tmp[tmp > 4e8] = self.FLOAT_FILL
                        test_mdata[test_mdata_name] = tmp
                # record info
                SIDUnique, idxs, invs = np.unique(StationIDs, return_index=True, return_inverse=True, axis=0)
                loc_mdata['record_number'] = invs

                # var metadata
                var_mdata['variable_names'] = writer.FillNcVector(outvars, "string")

                AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")

                # writer metadata
                nvars = len(outvars)
                nlocs = len(StationIDs)

                writer._nvars = nvars
                writer._nlocs = nlocs

                writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, units_values, test_mdata)

                print("ProcessedL %d Conventional obs processed to: %s" % (len(obsdata), outname))


def grabobsidx(obsdata, platform, var):
    """ grabobsidx(obsdata,platform,var):
    obsdata  - netCDF dataset object
    platform - string of observation type: 'sondes','sfc',etc.
    var      - string of variable type: 'tsen','tv','q', etc.

    returns idx - indices of observations to write out
    """
    code = obsdata['Observation_Type'][:]
    if var in ['tsen', 'tv']:
        iqt = obsdata['Setup_QC_Mark'][:]
        if var == 'tsen':
            idx2 = (iqt != 0)
        elif var == 'tv':
            idx2 = (iqt == 0)
    elif var in ['bend', 'refract']:
        igps = obsdata['GPS_Type'][:]
        if var == 'bend':
            idx2 = (igps != 0)
        elif var == 'refract':
            idx2 = (igps == 0)
    else:
        # to be consistent
        idx2 = (code > -999)
    # grab np logical based off of conv_dicts entry
    if var == 'uv':
        codes = uv_bufrtypes[platform]
    else:
        codes = conv_bufrtypes[platform]
    idx = np.logical_and(np.in1d(code, codes), idx2)

    return idx


# satellite radiance observations
class Radiances(BaseGSI):
    """ class Radiances - satellite radiance observations

                Use this class to read in satellite radiance observations
                from GSI netCDF diag files

    Functions:

    Attributes:
      filename    - string path to file
      validtime   - datetime object of valid observation time
      nobs        - number of observations


    """

    def __init__(self, filename):
        self.filename = filename
        splitfname = self.filename.split('/')[-1].split('_')
        i = False
        for s in rad_sensors:
            if s in splitfname:
                i = splitfname.index(s)
                self.obstype = "_".join(splitfname[i:i + 2])
        if not i:
            raise ValueError("Observation is not a radiance type...")

    def read(self):
        # get valid time
        df = nc.Dataset(self.filename)
        tstr = str(df.getncattr('date_time'))
        self.validtime = dt.datetime.strptime(tstr, "%Y%m%d%H")
        # sensor and satellite
        self.sensor = df.getncattr('Observation_type')
        self.satellite = df.getncattr('Satellite')
        # number of observations
        self.nobs = len(df.dimensions['nobs'])
        self.nchans = len(df.dimensions['nchans'])
        self.df = df

    def close(self):
        self.df.close()

    def toGeovals(self, OutDir, clobber=True):
        """ toGeovals(OutDir,clobber=True)
        if model state fields are in the GSI diag file, create
        GeoVaLs in an output file for use by JEDI/UFO
        """
        # note, this is a temporary construct and thus, there is no
        # ioda_conv_ncio or equivalent to handle the format
        # set up output file
        outname = OutDir + '/' + self.sensor + '_' + self.satellite + \
            '_geoval_' + self.validtime.strftime("%Y%m%d%H") + '.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting:")
                print(outname)
                return
        OutVars = []
        InVars = []
        for ncv in self.df.variables:
            if ncv in geovals_vars:
                OutVars.append(geovals_vars[ncv])
                InVars.append(ncv)

        # set up output file
        ncout = nc.Dataset(outname, 'w', format='NETCDF4')
        ncout.setncattr("date_time", np.int32(self.validtime.strftime("%Y%m%d%H")))
        ncout.setncattr("satellite", self.satellite)
        ncout.setncattr("sensor", self.sensor)
        # get nlocs
        nlocs = self.nobs / self.nchans
        ncout.createDimension("nlocs", nlocs)
        # other dims
        ncout.createDimension("nlevs", self.df.dimensions["air_temperature_arr_dim"].size)
        ncout.createDimension("nlevsp1", self.df.dimensions["air_pressure_levels_arr_dim"].size)

        for var in self.df.variables.values():
            vname = var.name
            if vname in geovals_metadata_dict.keys():
                dims = ("nlocs",)
                var_out = ncout.createVariable(geovals_metadata_dict[vname], var.dtype, dims)
                vdata = var[:]
                vdata = vdata[::self.nchans]
                var_out[:] = vdata
            elif vname in geovals_vars.keys():
                if (len(var.dimensions) == 1):
                    dims = ("nlocs",)
                elif "_levels" in vname:
                    dims = ("nlocs", "nlevsp1")
                else:
                    dims = ("nlocs", "nlevs")
                var_out = ncout.createVariable(geovals_vars[vname], var.dtype, dims)
                vdata = var[...]
                vdata = vdata[::self.nchans, ...]
                var_out[...] = vdata
            else:
                pass
        ncout.close()

    def toObsdiag(self, OutDir, clobber=True):
        """ toObsdiag(OutDir,clobber=True)
        if model state fields are in the GSI diag file, create
        Obsdiag in an output file for use by JEDI/UFO
        """
        # note, this is a temporary construct and thus, there is no
        # ioda_conv_ncio or equivalent to handle the format

        # set up output file
        outname = OutDir + '/' + self.sensor + '_' + self.satellite + \
            '_obsdiag_' + self.validtime.strftime("%Y%m%d%H") + '.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting: %s" % outname)
                return
        OutVars = []
        InVars = []
        for ncv in self.df.variables:
            if ncv in obsdiag_vars:
                OutVars.append(obsdiag_vars[ncv])
                InVars.append(ncv)

        # set up output file
        ncout = nc.Dataset(outname, 'w', format='NETCDF4')
        ncout.setncattr("date_time", np.int32(self.validtime.strftime("%Y%m%d%H")))
        ncout.setncattr("satellite", self.satellite)
        ncout.setncattr("sensor", self.sensor)
        # get nlocs
        nlocs = self.nobs / self.nchans
        ncout.createDimension("nlocs", nlocs)
        # other dims
        nlevs = self.df.dimensions["air_pressure_arr_dim"].size
        nlevsp1 = self.df.dimensions["air_pressure_levels_arr_dim"].size

        ncout.createDimension("nlevs", self.df.dimensions["air_pressure_arr_dim"].size)

        # get channel info and list
        chan_number = self.darr('sensor_chan')
        chan_number = chan_number[chan_number >= 0]
        chan_indx = self.var('Channel_Index')
        nchans = len(chan_number)
        nlocs = int(self.nobs / nchans)
        chanlist = chan_number

        # get data
        for var in self.df.variables.values():
            vname = var.name
            if vname in obsdiag_metadata_dict.keys():
                dims = ("nlocs",)
                var_out = ncout.createVariable(obsdiag_metadata_dict[vname], var.dtype, dims)
                vdata = var[:]
                vdata = vdata[::self.nchans]
                var_out[:] = vdata
            elif vname in obsdiag_vars.keys():
                # print("toObsdiag: var.shape = ", var.shape)
                if (len(var.dimensions) == 1):
                    dims = ("nlocs",)
                    for c in range(len(chanlist)):
                        var_name = obsdiag_vars[vname]+"_"+"{:d}".format(chanlist[c])
                        idx = chan_indx == c+1
                        if (np.sum(idx) == 0):
                            print("No matching observations for: %s" % value)
                            continue
                        var_out = ncout.createVariable(var_name, var.dtype, dims)
                        vdata = var[:]
                        vdata = vdata[idx]
                        var_out[:] = vdata
                elif "_levels" in vname:
                    dims = ("nlocs", "nlevsp1")
                else:
                    dims = ("nlocs", "nlevs")
                    for c in range(len(chanlist)):
                        var_name = obsdiag_vars[vname]+"_"+"{:d}".format(chanlist[c])
                        idx = chan_indx == c+1
                        if (np.sum(idx) == 0):
                            print("No matching observations for: %s" % value)
                            continue
                        var_out = ncout.createVariable(var_name, var.dtype, dims)
                        vdata = var[...]
                        vdata = vdata[idx, ...]
                        var_out[...] = vdata
            else:
                pass
        ncout.close()

    def toIODAobs(self, OutDir, ObsBias, QCVars, TestRefs, clobber=True):
        """ toIODAobs(OutDir,clobber=True)
        output observations from the specified GSI diag file
        to the JEDI/IODA observation format
        """

        print("Input Parameters: ObsBias=%s QCVars=%s TestRefs=%s" % (ObsBias, QCVars, TestRefs))
        # set up a NcWriter class
        outname = OutDir + '/' + self.sensor + '_' + self.satellite + \
            '_obs_' + self.validtime.strftime("%Y%m%d%H") + '.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting: %s" % outname)
                return
        LocKeyList = []
        TestKeyList = []
        LocVars = []
        TestVars = []
        AttrData = {}
        varDict = defaultdict(lambda: defaultdict(dict))
        outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        test_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        if self.sensor == "amsua":
            test_fields_ = test_fields_allsky
            test_fields_with_channels_ = test_fields_with_channels_allsky
        elif self.sensor == "atms":
            test_fields_ = test_fields_allsky
            test_fields_with_channels_ = test_fields_with_channels_allsky
        else:
            test_fields_ = test_fields
            test_fields_with_channels_ = test_fields_with_channels

        # get list of location variable for this var/platform
        for ncv in self.df.variables:
            if ncv in all_LocKeyList:
                LocKeyList.append(all_LocKeyList[ncv])
                LocVars.append(ncv)

        # get list of TestReference variables for this var/platform
        for ncv in self.df.variables:
            if ncv in test_fields_:
                TestKeyList.append(test_fields_[ncv])
                TestVars.append(ncv)
            if ncv in test_fields_with_channels_:
                TestKeyList.append(test_fields_with_channels_[ncv])
                TestVars.append(ncv)

        # for now, record len is 1 and the list is empty?
        if (TestRefs):
            writer = iconv.NcWriter(outname, LocKeyList, TestKeyList=TestKeyList)
        else:
            writer = iconv.NcWriter(outname, LocKeyList)

        chan_number = self.var('sensor_chan')
        chan_number = chan_number[chan_number >= 0]
        chan_indx = self.var('Channel_Index')
        nchans = len(chan_number)
        nlocs = int(self.nobs / nchans)

        chanlist = chan_number
        for a in chanlist:
            value = "brightness_temperature_{:d}".format(a)
            varDict[value]['valKey'] = value, writer.OvalName()
            varDict[value]['errKey'] = value, writer.OerrName()
            varDict[value]['qcKey'] = value, writer.OqcName()
            units_values[value] = 'K'
            if (ObsBias):
                valuebc = [
                    "constant_{:d}".format(a),
                    "scan_angle_{:d}".format(a),
                    "cloud_liquid_water_{:d}".format(a),
                    "lapse_rate_squared_{:d}".format(a),
                    "lapse_rate_{:d}".format(a),
                    "cosine_of_latitude_times_orbit_node_{:d}".format(a),
                    "sine_of_latitude_{:d}".format(a),
                    "emissivity_{:d}".format(a),
                    "scan_angle_4th_order_{:d}".format(a),
                    "scan_angle_3rd_order_{:d}".format(a),
                    "scan_angle_2nd_order_{:d}".format(a),
                    "scan_angle_1st_order_{:d}".format(a),
                ]
                ibc = 0
                for vbc in valuebc:
                    varDict[vbc]['bctKey'] = vbc, writer.ObiastermName()
                    varDict[vbc]['bcpKey'] = vbc, writer.ObiaspredName()
                    ibc += 1
        obsdata = self.var('Observation')
        obserr = self.var('Input_Observation_Error')
        obsqc = self.var('QC_Flag').astype(int)
        if (ObsBias):
            nametbc = [
                'BC_Constant',
                'BC_Scan_Angle',
                'BC_Cloud_Liquid_Water',
                'BC_Lapse_Rate_Squared',
                'BC_Lapse_Rate',
                'BC_Cosine_Latitude_times_Node',
                'BC_Sine_Latitude',
                'BC_Emissivity',
                'BC_Scan_Angle_4th_order',
                'BC_Scan_Angle_3rd_order',
                'BC_Scan_Angle_2nd_order',
                'BC_Scan_Angle_1st_order',
            ]
            namepbc = [
                'BCPred_Constant',
                'BCPred_Scan_Angle',
                'BCPred_Cloud_Liquid_Water',
                'BCPred_Lapse_Rate_Squared',
                'BCPred_Lapse_Rate',
                'BCPred_Cosine_Latitude_times_Node',
                'BCPred_Sine_Latitude',
                'BCPred_Emissivity',
                'BCPred_Scan_Angle_4th_order',
                'BCPred_Scan_Angle_3rd_order',
                'BCPred_Scan_Angle_2nd_order',
                'BCPred_Scan_Angle_1st_order',
            ]
            obsbiasterm = []
            ii = 0
            for nbc in nametbc:
                obsbiasterm.append(self.var(nametbc[ii]))
                ii += 1

            obsbiaspred = []
            ii = 0
            for nbc in namepbc:
                obsbiaspred.append(self.var(namepbc[ii]))
                ii += 1
        for lvar in LocVars:
            loc_mdata_name = all_LocKeyList[lvar][0]
            if lvar == 'Obs_Time':
                tmp = self.var(lvar)[::nchans]
                obstimes = [self.validtime + dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                loc_mdata[loc_mdata_name] = writer.FillNcVector(obstimes, "datetime")
            else:
                tmp = self.var(lvar)[::nchans]
                tmp[tmp > 4e8] = self.FLOAT_FILL
                loc_mdata[loc_mdata_name] = tmp

        # put the TestReference fields in the structure for writing out
        for tvar in TestVars:
            if tvar in test_fields_with_channels_:
                for ii, ch in enumerate(chanlist):
                    test_mdata_name = test_fields_with_channels_[tvar][0]+"_{:d}".format(ch)
                    tmp = self.var(tvar)[:]
                    tmp[tmp > 4e8] = self.FLOAT_FILL
                    idx = chan_indx == ii+1
                    outvals = tmp[idx]
                    test_mdata[test_mdata_name] = outvals
            if tvar in test_fields_:
                test_mdata_name = test_fields_[tvar][0]
                tmp = self.var(tvar)[::nchans]
                tmp[tmp > 4e8] = self.FLOAT_FILL
                test_mdata[test_mdata_name] = tmp

        gsi_add_radvars = gsi_add_vars
        if (QCVars):
            gsi_add_radvars.update(gsi_add_qcvars)

        if self.sensor == "amsua":
            gsi_add_radvars = gsi_add_vars_allsky
            if (QCVars):
                gsi_add_radvars.update(gsi_add_qcvars_allsky)

        if self.sensor == "atms":
            gsi_add_radvars = gsi_add_vars_allsky
            if (QCVars):
                gsi_add_radvars.update(gsi_add_qcvars_allsky)

        # check for additional GSI output for each variable
        for gsivar, iodavar in gsi_add_radvars.items():
            if gsivar in self.df.variables:
                if "Inverse" in gsivar:
                    tmp = self.var(gsivar)
                    # fix for if some reason 1/small does not result in inf but zero
                    mask = tmp < self.EPSILON
                    tmp[~mask] = 1.0 / tmp[~mask]
                    tmp[mask] = self.FLOAT_FILL
                else:
                    tmp = self.var(gsivar)
                if gsivar in gsiint:
                    tmp = tmp.astype(int)
                else:
                    tmp[tmp > 4e8] = self.FLOAT_FILL
                for ii, ch in enumerate(chanlist):
                    varname = "brightness_temperature_{:d}".format(ch)
                    gvname = varname, iodavar
                    idx = chan_indx == ii+1
                    outvals = tmp[idx]
                    outdata[gvname] = outvals

        # loop through channels for subset
        var_names = []
        for c in range(len(chanlist)):
            value = "brightness_temperature_{:d}".format(chanlist[c])
            var_names.append(value)
            idx = chan_indx == c+1
            if (np.sum(idx) == 0):
                print("No matching observations for: %s" % value)
                continue
            obsdatasub = obsdata[idx]
            obsdatasub[obsdatasub > 9e5] = self.FLOAT_FILL
            obserrsub = obserr[idx]
            obsqcsub = obsqc[idx]
            obsqcsub[obsdatasub > 9e5] = self.INT_FILL

            # store values in output data dictionary
            outdata[varDict[value]['valKey']] = obsdatasub
            outdata[varDict[value]['errKey']] = obserrsub
            outdata[varDict[value]['qcKey']] = obsqcsub.astype(int)
            if (ObsBias):
                valuebc = [
                    "constant_{:d}".format(chanlist[c]),
                    "scan_angle_{:d}".format(chanlist[c]),
                    "cloud_liquid_water_{:d}".format(chanlist[c]),
                    "lapse_rate_squared_{:d}".format(chanlist[c]),
                    "lapse_rate_{:d}".format(chanlist[c]),
                    "cosine_of_latitude_times_orbit_node_{:d}".format(chanlist[c]),
                    "sine_of_latitude_{:d}".format(chanlist[c]),
                    "emissivity_{:d}".format(chanlist[c]),
                    "scan_angle_4th_order_{:d}".format(chanlist[c]),
                    "scan_angle_3rd_order_{:d}".format(chanlist[c]),
                    "scan_angle_2nd_order_{:d}".format(chanlist[c]),
                    "scan_angle_1st_order_{:d}".format(chanlist[c]),
                ]
                ii = 0
                for value in valuebc:
                    obsbiastermsub = obsbiasterm[ii][idx]
                    obsbiaspredsub = obsbiaspred[ii][idx]
                    obsbiastermsub[obsbiastermsub > 9e5] = self.FLOAT_FILL
                    obsbiaspredsub[obsbiaspredsub > 9e5] = self.FLOAT_FILL

                    # store values in output data dictionary
                    outdata[varDict[value]['bctKey']] = obsbiastermsub
                    outdata[varDict[value]['bcpKey']] = obsbiaspredsub
                    ii += 1
        # var metadata
        var_mdata['variable_names'] = writer.FillNcVector(var_names, "string")
        for key, value2 in chan_metadata_dict.items():
            try:
                var_mdata[value2] = self.var(key)
            except IndexError:
                pass

        # dummy record metadata, for now
        loc_mdata['record_number'] = np.full((nlocs), 1, dtype='i4')

        # global attributes

        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor

        # set dimension lengths in the writer since we are bypassing
        # ExtractObsData
        writer._nvars = nchans
        writer._nlocs = nlocs
        if (TestRefs):
            writer.BuildNetcdf(outdata, loc_mdata, var_mdata,
                               AttrData, units_values, test_mdata)
        else:
            writer.BuildNetcdf(outdata, loc_mdata, var_mdata,
                               AttrData, units_values)

        print("Satellite radiance obs processed, wrote to: %s" % outname)


# atmospheric composition observations

class AOD(BaseGSI):
    """ class AOD - aerosol optical depth satellite observations

        Use this class to read in AOD satellite observations
        from GSI netCDF diag files

    Functions:
    Attributes:
        filename    - string path to file
        validtime   - datetime object of valid observation time
        nobs        - number of observations
    """
    def __init__(self, filename):
        self.filename = filename
        splitfname = self.filename.split('/')[-1].split('_')
        i = False
        for s in aod_sensors:
            if s in splitfname:
                i = splitfname.index(s)
                self.obstype = "_".join(splitfname[i:i+3])
        if not self.obstype:
            raise ValueError("Observation is not AOD type...")
        # sensor and satellite
        self.sensor = splitfname[i]
        self.satellite = splitfname[i+2]

    def read(self):
        # get valid time
        df = nc.Dataset(self.filename)
        tstr = str(df.getncattr('date_time'))
        self.validtime = dt.datetime.strptime(tstr, "%Y%m%d%H")
        # number of observations
        self.nobs = len(df.dimensions['nobs'])
        self.nchans = len(df.dimensions['nchans'])
        self.df = df

    def toGeovals(self, OutDir, clobber=True):
        """ toGeovals(OutDir,clobber=True)
        if model state fields are in the GSI diag file, create
        GeoVaLs in an output file for use by JEDI/UFO
        """
        # note, this is a temporary construct and thus, there is no
        # ioda_conv_ncio or equivalent to handle the format

        # set up output file
        outname = OutDir+'/'+self.obstype+'_geoval_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting: %s" % outname)
                return
        OutVars = []
        InVars = []
        for ncv in self.df.variables:
            if ncv in geovals_vars:
                OutVars.append(geovals_vars[ncv])
                InVars.append(ncv)

        # set up output file
        ncout = nc.Dataset(outname, 'w', format='NETCDF4')
        ncout.setncattr("date_time", np.int32(self.validtime.strftime("%Y%m%d%H")))
        ncout.setncattr("satellite", self.satellite)
        ncout.setncattr("sensor", self.sensor)
        # get nlocs
        nlocs = self.nobs
        ncout.createDimension("nlocs", nlocs)
        # other dims
        ncout.createDimension("nlevs", self.df.dimensions["air_temperature_arr_dim"].size)
        ncout.createDimension("nlevsp1", self.df.dimensions["air_pressure_levels_arr_dim"].size)
        for var in self.df.variables.values():
            vname = var.name
            if vname in geovals_metadata_dict.keys():
                dims = ("nlocs",)
                var_out = ncout.createVariable(geovals_metadata_dict[vname], var.dtype, dims)
                vdata = var[:]
                var_out[:] = vdata
            elif vname in geovals_vars.keys():
                if (len(var.dimensions) == 1):
                    dims = ("nlocs",)
                elif "_levels" in vname:
                    dims = ("nlocs", "nlevsp1")
                else:
                    dims = ("nlocs", "nlevs")
                var_out = ncout.createVariable(geovals_vars[vname], var.dtype, dims)
                vdata = var[...]
                var_out[...] = vdata
            else:
                pass
        ncout.close()

    def toIODAobs(self, OutDir, clobber=True):
        """
        toIODAobs(OutDir,clobber=True)
        output observations from the specified GSI diag file
        to the JEDI/IODA observation format
        """
        # set up a NcWriter class
        outname = OutDir+'/'+self.obstype+'_obs_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting:%s" % outname)
                return
        LocKeyList = []
        LocVars = []
        AttrData = {}
        varDict = defaultdict(lambda: defaultdict(dict))
        outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        # get list of location variable for this var/platform
        for ncv in self.df.variables:
            if ncv in all_LocKeyList:
                LocKeyList.append(all_LocKeyList[ncv])
                LocVars.append(ncv)

        # for now, record len is 1 and the list is empty?
        writer = iconv.NcWriter(outname, LocKeyList)

        chan_number = self.var('sensor_chan')
        chan_number = chan_number[chan_number >= 0]
        chan_indx = self.var('Channel_Index')
        nchans = len(chan_number)
        nlocs = self.nobs / nchans
        chanlist = chan_indx[:nchans]
        for a in chanlist:
            value = "aerosol_optical_depth_{:d}".format(a)
            varDict[value]['valKey'] = value, writer.OvalName()
            varDict[value]['errKey'] = value, writer.OerrName()
            varDict[value]['qcKey'] = value, writer.OqcName()

        obsdata = self.var('Observation')
        obserr = 1.0/self.var('Observation_Error')
        obsqc = self.var('QC_Flag').astype(int)

        gsivars = gsi_add_vars

        # loop through channels for subset
        var_names = []
        for c in range(len(chanlist)):
            value = "aerosol_optical_depth_{:d}".format(chanlist[c])
            var_names.append(value)
            idx = chan_indx == chanlist[c]
            obsdatasub = obsdata[idx]
            obserrsub = obserr[idx]
            obsqcsub = obsqc[idx]
            for lvar in LocVars:
                loc_mdata_name = all_LocKeyList[lvar][0]
                if lvar == 'Obs_Time':
                    tmp = self.var(lvar)[idx]
                    obstimes = [self.validtime+dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                    obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                    loc_mdata[loc_mdata_name] = writer.FillNcVector(obstimes, "datetime")
                else:
                    loc_mdata[loc_mdata_name] = self.var(lvar)[idx]
            gsimeta = {}
            for key, value2 in gsivars.items():
                # some special actions need to be taken depending on var name...
                if "Inverse" in key:
                    try:
                        gsimeta[key] = 1.0/self.var(key)[idx]
                    except IndexError:
                        pass
                else:
                    try:
                        gsimeta[key] = self.var(key)[idx]
                    except IndexError:
                        pass

            # store values in output data dictionary
            outdata[varDict[value]['valKey']] = obsdatasub
            outdata[varDict[value]['errKey']] = obserrsub
            outdata[varDict[value]['qcKey']] = obsqcsub

            # add additional GSI variables that are not needed long term but useful for testing
            for key, value2 in gsivars.items():
                gvname = value, value2
                if value2 in gsiint:
                    try:
                        outdata[gvname] = gsimeta[key].astype(int)
                    except KeyError:
                        pass
                else:
                    try:
                        outdata[gvname] = gsimeta[key]
                    except KeyError:
                        pass

        # var metadata
        var_mdata['variable_names'] = writer.FillNcVector(var_names, "string")
        for key, value2 in chan_metadata_dict.items():
            try:
                var_mdata[value2] = self.var(key)[:nchans]
            except IndexError:
                pass

        # dummy record metadata, for now
        loc_mdata['record_number'] = np.full((nlocs), 1, dtype='i4')

        # global attributes
        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor

        # set dimension lengths in the writer since we are bypassing ExtractObsData
        writer._nvars = nchans
        writer._nlocs = nlocs

        writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, units_values)
        print("AOD obs processed, wrote to:%s" % outname)


class Ozone(BaseGSI):
    """ class Ozone - ozone satellite observations

                Use this class to read in ozone satellite observations
                from GSI netCDF diag files

    Functions:

    Attributes:
      filename    - string path to file
      validtime   - datetime object of valid observation time
      nobs        - number of observations

    """
    def __init__(self, filename):
        self.filename = filename
        splitfname = self.filename.split('/')[-1].split('_')
        i = False
        for s in oz_sensors:
            if s in splitfname:
                i = splitfname.index(s)
                self.obstype = "_".join(splitfname[i:i+2])
        if not i:
            raise ValueError("Observation is not an ozone type...")
        # sensor and satellite
        self.sensor = splitfname[i]
        self.satellite = splitfname[i+1]

    def read(self):
        # get valid time
        df = nc.Dataset(self.filename)
        tstr = str(df.getncattr('date_time'))
        self.validtime = dt.datetime.strptime(tstr, "%Y%m%d%H")
        # number of observations
        self.nobs = len(df.dimensions['nobs'])
        self.df = df

    def toGeovals(self, OutDir, clobber=True):
        """ toGeovals(OutDir,clobber=True)
        if model state fields are in the GSI diag file, create
        GeoVaLs in an output file for use by JEDI/UFO
        """
        # note, this is a temporary construct and thus, there is no
        # ioda_conv_ncio or equivalent to handle the format

        # set up output file
        outname = OutDir+'/'+self.sensor+'_'+self.satellite+'_geoval_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting: %s" % outname)
                return
        OutVars = []
        InVars = []
        for ncv in self.df.variables:
            if ncv in geovals_vars:
                OutVars.append(geovals_vars[ncv])
                InVars.append(ncv)

        # set up output file
        ncout = nc.Dataset(outname, 'w', format='NETCDF4')
        ncout.setncattr("date_time", np.int32(self.validtime.strftime("%Y%m%d%H")))
        ncout.setncattr("satellite", self.satellite)
        ncout.setncattr("sensor", self.sensor)
        # get nlocs
        nlocs = self.nobs
        ncout.createDimension("nlocs", nlocs)
        # other dims
        ncout.createDimension("nlevs", self.df.dimensions["mass_concentration_of_ozone_in_air_arr_dim"].size)
        ncout.createDimension("nlevsp1", self.df.dimensions["air_pressure_levels_arr_dim"].size)
        for var in self.df.variables.values():
            vname = var.name
            if vname in geovals_metadata_dict.keys():
                dims = ("nlocs",)
                var_out = ncout.createVariable(geovals_metadata_dict[vname], var.dtype, dims)
                vdata = var[:]
                var_out[:] = vdata
            elif vname in geovals_vars.keys():
                if (len(var.dimensions) == 1):
                    dims = ("nlocs",)
                elif "_levels" in vname:
                    dims = ("nlocs", "nlevsp1")
                else:
                    dims = ("nlocs", "nlevs")
                var_out = ncout.createVariable(geovals_vars[vname], var.dtype, dims)
                vdata = var[...]
                var_out[...] = vdata
            else:
                pass
        ncout.close()

    def toIODAobs(self, OutDir, clobber=True):
        """ toIODAobs(OutDir,clobber=True)
        output observations from the specified GSI diag file
        to the JEDI/IODA observation format
        """
        # set up a NcWriter class
        outname = OutDir+'/'+self.sensor+'_'+self.satellite+'_obs_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting: %s" % outname)
                return
        LocKeyList = []
        LocVars = []
        AttrData = {}
        varDict = defaultdict(lambda: defaultdict(dict))
        outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        # get list of location variable for this var/platform
        for ncv in self.df.variables:
            if ncv in all_LocKeyList:
                LocKeyList.append(all_LocKeyList[ncv])
                LocVars.append(ncv)
        # for now, record len is 1 and the list is empty?
        writer = iconv.NcWriter(outname, LocKeyList)

        nlocs = self.nobs
        vname = "mole_fraction_of_ozone_in_air"
        varDict[vname]['valKey'] = vname, writer.OvalName()
        varDict[vname]['errKey'] = vname, writer.OerrName()
        varDict[vname]['qcKey'] = vname, writer.OqcName()

        obsdata = self.var('Observation')
        tmp = self.var('Input_Observation_Error')
        tmp[tmp < self.EPSILON] = 0
        obserr = tmp
        obserr[np.isinf(obserr)] = self.FLOAT_FILL
        obsqc = self.var('Analysis_Use_Flag').astype(int)
        locKeys = []
        for lvar in LocVars:
            loc_mdata_name = all_LocKeyList[lvar][0]
            if lvar == 'Time':
                tmp = self.var(lvar)
                obstimes = [self.validtime+dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                loc_mdata[loc_mdata_name] = writer.FillNcVector(obstimes, "datetime")
            else:
                tmp = self.var(lvar)
                tmp[tmp > 4e8] = self.FLOAT_FILL
                loc_mdata[loc_mdata_name] = tmp

        for gsivar, iodavar in gsi_add_vars.items():
            # some special actions need to be taken depending on var name...
            if gsivar in self.df.variables:
                if "Inverse" in gsivar:
                    tmp = self.var(gsivar)
                    # fix for if some reason 1/small does not result in inf but zero
                    mask = tmp < self.EPSILON
                    tmp[~mask] = 1.0 / tmp[~mask]
                    tmp[mask] = self.FLOAT_FILL
                else:
                    tmp = self.var(gsivar)
                if gsivar in gsiint:
                    tmp = tmp.astype(int)
                else:
                    tmp[tmp > 4e8] = self.FLOAT_FILL
                gvname = vname, iodavar
                outdata[gvname] = tmp
        # observation data
        outdata[varDict[vname]['valKey']] = obsdata
        outdata[varDict[vname]['errKey']] = obserr
        outdata[varDict[vname]['qcKey']] = obsqc

        # dummy record metadata, for now
        loc_mdata['record_number'] = np.full((nlocs), 1, dtype='i4')

        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor
        # set dimension lengths in the writer since we are bypassing
        # ExtractObsData
        writer._nvars = 1
        writer._nlocs = nlocs

        writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, units_values)
        print("Ozone obs processed, wrote to: %s" % outname)


class Radar(BaseGSI):
    """ class Radar - reflectivity and radial wind observations

                Use this class to read in radar observations
                from GSI netCDF diag files

    Functions:

    Attributes:
      filename    - string path to file
      validtime   - datetime object of valid observation time
      nobs        - number of observations

    """
    def __init__(self, filename):
        self.filename = filename
        splitfname = self.filename.split('/')[-1].split('_')
        i = False
        for s in radar_sensors:
            if s in splitfname:
                i = splitfname.index(s)
                self.obstype = "_".join(splitfname[i:i+2])
        if not i:
            raise ValueError("Observation is not a radar type...")
        # sensor and satellite
        self.sensor = splitfname[i]
        self.obstype = splitfname[i+1]

    def read(self):
        # get valid time
        df = nc.Dataset(self.filename)
        tstr = str(df.getncattr('date_time'))
        self.validtime = dt.datetime.strptime(tstr, "%Y%m%d%H")
        # number of observations
        self.nobs = len(df.dimensions['nobs'])
        self.df = df

    def toGeovals(self, OutDir, clobber=True):
        """ toGeovals(OutDir,clobber=True)
        if model state fields are in the GSI diag file, create
        GeoVaLs in an output file for use by JEDI/UFO
        """
        # note, this is a temporary construct and thus, there is no
        # ioda_conv_ncio or equivalent to handle the format

        # set up output file
        outname = OutDir+'/'+self.sensor+'_'+self.obstype+'_geoval_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting: %s" % outname)
                return
        OutVars = []
        InVars = []
        for ncv in self.df.variables:
            if ncv in geovals_vars:
                OutVars.append(geovals_vars[ncv])
                InVars.append(ncv)

        # set up output file
        ncout = nc.Dataset(outname, 'w', format='NETCDF4')
        ncout.setncattr("date_time", np.int32(self.validtime.strftime("%Y%m%d%H")))
        # get nlocs
        nlocs = self.nobs
        ncout.createDimension("nlocs", nlocs)
        # other dims
        ncout.createDimension("nlevs", self.df.dimensions["nlevs"].size)
        # ncout.createDimension("nlevsp1", self.df.dimensions["air_pressure_levels_arr_dim"].size)
        for var in self.df.variables.values():
            vname = var.name
            if vname in geovals_metadata_dict.keys():
                dims = ("nlocs",)
                var_out = ncout.createVariable(geovals_metadata_dict[vname], var.dtype, dims)
                vdata = var[:]
                var_out[:] = vdata
            elif vname in geovals_vars.keys():
                if (len(var.dimensions) == 1):
                    dims = ("nlocs",)
                elif "_levels" in vname:
                    dims = ("nlocs", "nlevsp1")
                else:
                    dims = ("nlocs", "nlevs")
                var_out = ncout.createVariable(geovals_vars[vname], var.dtype, dims)
                vdata = var[...]
                var_out[...] = vdata
            else:
                pass
        ncout.close()

    def toIODAobs(self, OutDir, clobber=True):
        """ toIODAobs(OutDir,clobber=True)
        output observations from the specified GSI diag file
        to the JEDI/IODA observation format
        """
        # set up a NcWriter class
        outname = OutDir+'/'+self.sensor+'_'+self.obstype+'_obs_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting:%s" % outname)
                return
        LocKeyList = []
        LocVars = []
        AttrData = {}
        varDict = defaultdict(lambda: defaultdict(dict))
        outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        # get list of location variable for this var/platform
        for ncv in self.df.variables:
            if ncv in all_LocKeyList:
                LocKeyList.append(all_LocKeyList[ncv])
                LocVars.append(ncv)
        # for now, record len is 1 and the list is empty?
        writer = iconv.NcWriter(outname, LocKeyList)

        nlocs = self.nobs
        if self.obstype == "dbz":
            radar_varnames = {
                'obsdbz': 'equivalent_reflectivity_factor',
            }
        elif self.obstype == "rw":
            radar_varnames = {
                'obsrw': 'radial_velocity',
            }

        for key, value in radar_varnames.items():
            varDict[value]['valKey'] = value, writer.OvalName()
            varDict[value]['errKey'] = value, writer.OerrName()
            varDict[value]['qcKey'] = value, writer.OqcName()

            obsdata = self.var(key)
            errvarname = radar_err[key]
            qcvarname = radar_qc[key]
            obserr = self.var(errvarname)
            obserr[np.isinf(obserr)] = self.FLOAT_FILL
            obsqc = self.var(qcvarname).astype(int)
            # observation data
            outdata[varDict[value]['valKey']] = obsdata
            outdata[varDict[value]['errKey']] = obserr
            outdata[varDict[value]['qcKey']] = obsqc
            vname = value
            for gsivar, iodavar in gsi_add_vars.items():
                # some special actions need to be taken depending on var name...
                if gsivar in self.df.variables:
                    if "Inverse" in gsivar:
                        tmp = self.var(gsivar)
                        # fix for if some reason 1/small does not result in inf but zero
                        mask = tmp < self.EPSILON
                        tmp[~mask] = 1.0 / tmp[~mask]
                        tmp[mask] = self.FLOAT_FILL
                    else:
                        tmp = self.var(gsivar)[:]
                    if gsivar in gsiint:
                        tmp = tmp.astype(int)
                    else:
                        tmp[tmp > 4e8] = self.FLOAT_FILL
                    gvname = vname, iodavar
                    outdata[gvname] = tmp
        locKeys = []
        for lvar in LocVars:
            loc_mdata_name = all_LocKeyList[lvar][0]
            if lvar == 'Time':
                tmp = self.var(lvar)[:]
                obstimes = [self.validtime+dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                loc_mdata[loc_mdata_name] = writer.FillNcVector(obstimes, "datetime")
            else:
                tmp = self.var(lvar)[:]
                tmp[tmp > 4e8] = self.FLOAT_FILL
                loc_mdata[loc_mdata_name] = tmp

        # dummy record metadata, for now
        loc_mdata['record_number'] = np.full((nlocs), 1, dtype='i4')

        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        AttrData["sensor"] = self.sensor
        # set dimension lengths in the writer since we are bypassing
        # ExtractObsData
        writer._nvars = 1
        writer._nlocs = nlocs

        writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, units_values)
        print("Radar obs processed, wrote to: %s" % outname)
