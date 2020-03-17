# wrfda_ncdiag.py
# a collection of classes, and supporting information
# to read in WRFDA netCDF diagnostic files and rewrite them
# into JEDI UFO GeoVaLs and IODA observation files
###############################################################################
###############################################################################
from collections import defaultdict, OrderedDict
import datetime as dt
import errno
import ioda_conv_ncio as iconv
import netCDF4 as nc
import numpy as np
from orddicts import DefaultOrderedDict
import os
import sys

# dictionaries and lists
# LocKeyList = { 'wrfdaname':[('IODAname','dtype')]}

wrfda_miss_float = -888888.
wrfda_miss_int = -888888

all_LocKeyList = {
    'date': [('datetime', 'string')],
    'lat': [('latitude', 'float')],
    'lon': [('longitude', 'float')],
    'elv': [('height_above_mean_sea_level', 'float')],
    'scanpos': [('scan_position', 'float')],
    'satzen': [('sensor_zenith_angle', 'float'),
               ('sensor_view_angle', 'float')],
    'satazi': [('sensor_azimuth_angle', 'float')],
    'solzen': [('solar_zenith_angle', 'float')],
    'solazi': [('solar_azimuth_angle', 'float')],
    'cloud_frac': [('cloud_area_fraction', 'float')],
}

wrfda_add_vars = {
    # 'tb_bak': 'WrfdaHofX',
    # 'tb_bak_clr': 'WrfdaHofXClrSky',
    'tb_omb': 'WrfdaOmB',
    'tb_err': 'WrfdaFinalObsError',
    # 'cloud_obs': 'WrfdaCloudObs',
    # 'cloud_mod': 'WrfdaCloudModel',
}

rad_platform_sensor_combos = [
    # 'eos-2-airs',
    # 'fy3-1-mwhs',
    # 'fy3-1-mwts',
    # 'fy3-2-mwhs',
    # 'fy3-2-mwts',
    # 'fy3-3-mwhs2',
    # 'gcom-w-1-amsr2',
    'goes-16-abi',
    'goes-17-abi',
    'himawari-8-ahi',
    # 'jpss-0-atms',
    # 'metop-1-amsua',
    # 'metop-1-iasi',
    # 'metop-1-mhs',
    # 'metop-2-amsua',
    # 'metop-2-iasi',
    # 'metop-2-mhs',
    # 'msg-2-seviri',
    # 'msg-3-seviri',
    # 'noaa-15-amsua',
    # 'noaa-16-amsua',
    # 'noaa-16-amsub',
    # 'noaa-17-amsub',
    # 'noaa-17-hirs',
    # 'noaa-18-amsua',
    # 'noaa-18-hirs',
    # 'noaa-18-mhs',
    # 'noaa-19-amsua',
    # 'noaa-19-mhs'
]

wrfda2crtm_satellite_map = {
    # 'eos-2': 'aqua',
    # 'fy3-1': 'fy3a',
    # 'fy3-2': 'fy3b',
    # 'fy3-3': 'fy3c',
    # 'gcom-w-1': 'gcom-w1',
    'goes-16': 'g16',
    'goes-17': 'g17',
    'himawari-8': 'himawari8',
    # 'jpss-0': 'npp',
    # 'metop-1': 'metop-a',
    # 'metop-2': 'metop-b',
    # 'msg-2': 'm09',
    # 'msg-3': 'm10',
    # 'noaa-15': 'n15',
    # 'noaa-16': 'n16',
    # 'noaa-17': 'n17',
    # 'noaa-18': 'n18',
    # 'noaa-19': 'n19',
    # 'noaa-20': 'n20',
}

sensor_chanlist_dict = {
    'abi': list(range(7, 17)),
    'ahi': list(range(7, 17)),
}

rad_platform_sensor_ObsError = {
    'goes-16-abi': [2.720, 1.790, 1.920, 1.740, 5.000, wrfda_miss_float, 3.080, 3.060, 2.820, 1.740],
    'goes-17-abi': [wrfda_miss_float]*10,
    'himawari-8-ahi': [1.052, 1.700, 1.700, 1.350, 0.814, wrfda_miss_float, 0.871, 0.926, 0.933, 0.787],
}
# Note: ABI/AHI channel 12 is sensitive to O3, which is not included in WRFDA RTM interfaces
#       Therefore ObsError is specified as missing

# units (exact copy from gsi_ncdiag.py)
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
    'cloud_area_fraction': '1',
    'scan_position': '1',
    'sensor_azimuth_angle': 'degree',
    'sensor_zenith_angle': 'degree',
    'sensor_view_angle': 'degree',
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
}

# @TestReference
# fields from WRFDA to compare to computations done in UFO
test_fields = {
}

###############################################################################
###############################################################################


# satellite radiance observations
class Radiances:
    """ class Radiances - satellite radiance observations

                Use this class to read in satellite radiance observations
                from WRFDA netCDF diag files

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
        for s in rad_platform_sensor_combos:
            if s in splitfname:
                self.platform_sensor = s
                i = splitfname.index(s)
        if not i:
            raise ValueError("Observation is not a radiance type...")

    def read(self):
        # get valid time
        df = nc.Dataset(self.filename)
        tstr = self.filename.split('/')[-1].split('_')[-1].split('.')[0]
        self.validtime = dt.datetime.strptime(tstr, "%Y%m%d%H")
        # sensor and satellite
        self.sensor = self.platform_sensor.split('-')[-1]
        satellite = "-".join(self.platform_sensor.split('-')[0:-1])
        if satellite in wrfda2crtm_satellite_map:
            self.satellite = wrfda2crtm_satellite_map[satellite]
        else:
            print("ERROR: Satellite not found in wrfda2crtm_satellite_map:")
            print(satellite)
            sys.exit(1)

        # number of observations
        self.nlocs = len(df.dimensions['npixel'])
        self.nchans = len(df.dimensions['nchan'])
        self.nobs = self.nlocs * self.nchans
        self.df = df

    def close(self):
        self.df.close()

    def toIODAobs(self, OutDir, clobber=True, dateSubDirs=False):
        """ toIODAobs(OutDir,clobber=True)
     output observations from the specified WRFDA diag file
     to the JEDI/IODA observation format
        """
        fullOutDir = OutDir
        if dateSubDirs:
            # place files for individual dates in separate directories
            fullOutDir = fullOutDir + '/' + self.validtime.strftime("%Y%m%d%H")

        try:
            os.makedirs(fullOutDir)
        except OSError as exc:
            if exc.errno == errno.EEXIST and os.path.isdir(fullOutDir):
                pass

        # set up a NcWriter class
        outname = fullOutDir + '/' + self.sensor + '_' + self.satellite + \
            '_obs_' + self.validtime.strftime("%Y%m%d%H") + '.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting:")
                print(outname)
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
        # get list of location variable for this var/platform
        for ncv in self.df.variables:
            if ncv in all_LocKeyList:
                for val in all_LocKeyList[ncv]:
                    LocKeyList.append(val)
                    LocVars.append(ncv)

        # get list of TestReference variables for this var/platform
        for ncv in self.df.variables:
            if ncv in test_fields:
                TestKeyList.append(test_fields[ncv])
                TestVars.append(ncv)

        # for now, record len is 1 and the list is empty?
        recKey = 0
        writer = iconv.NcWriter(outname, LocKeyList, TestKeyList=TestKeyList)

        if self.sensor in sensor_chanlist_dict:
            chanlist = sensor_chanlist_dict[self.sensor]
        else:
            chanlist = list(range(1, self.nchan+1))
        nchans = len(chanlist)

        for chan in chanlist:
            value = "brightness_temperature_{:d}".format(chan)
            varDict[value]['valKey'] = value, writer.OvalName()
            varDict[value]['errKey'] = value, writer.OerrName()
            varDict[value]['qcKey'] = value, writer.OqcName()
            units_values[value] = 'K'

        for ivar, lvar in enumerate(LocVars):
            loc_mdata_name = LocKeyList[ivar][0]
            loc_mdata_type = LocKeyList[ivar][1]
            if lvar == 'date':
                tmp = self.df[lvar][:]
                obstimes = [dt.datetime.strptime("".join(a.astype(str)), "%Y-%m-%d_%H:%M:%S") for a in tmp]
                obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                loc_mdata[loc_mdata_name] = writer.FillNcVector(obstimes, "datetime")
            else:
                if loc_mdata_type == 'float':
                    tmp = self.df[lvar][:].astype(float)
                    tmp[tmp <= wrfda_miss_float] = nc.default_fillvals['f4']
                else:
                    tmp = self.df[lvar][:]
                loc_mdata[loc_mdata_name] = tmp

        # put the TestReference fields in the structure for writing out
        for tvar in TestVars:
            test_mdata_name = test_fields[tvar][0]
            tmp = self.df[tvar][:]
            tmp[tmp <= wrfda_miss_float] = nc.default_fillvals['f4']
            test_mdata[test_mdata_name] = tmp

        # check for additional WRFDA output for each variable
        for wrfdavar, iodavar in wrfda_add_vars.items():
            if wrfdavar in self.df.variables:
                tmp = np.transpose(np.asarray(self.df[wrfdavar]))

                tmp[tmp <= wrfda_miss_float] = nc.default_fillvals['f4']
                for c, chan in enumerate(chanlist):
                    varname = "brightness_temperature_{:d}".format(chan)
                    gvname = varname, iodavar
                    outvals = tmp[c]
                    outdata[gvname] = outvals

        # tb_obs, tb_err, and tb_qc are nlocs x nchan
        # --> using transpose speeds up access below
        obsdata = np.transpose(np.asarray(self.df['tb_obs']))
        obserr = rad_platform_sensor_ObsError[self.platform_sensor]
        # obserr  = np.transpose(np.asarray(self.df['tb_err']))
        obsqc = np.transpose(np.asarray(self.df['tb_qc']))

        # loop through channels for subset
        var_names = []
        for c, chan in enumerate(chanlist):
            value = "brightness_temperature_{:d}".format(chan)
            var_names.append(value)

            obsdatasub = obsdata[c]
            obsdatasub[obsdatasub <= wrfda_miss_float] = nc.default_fillvals['f4']

            obserrsub = np.full(self.nlocs, obserr[c])
            obserrsub[obserrsub <= wrfda_miss_float] = nc.default_fillvals['f4']

            obsqcsub = obsqc[c]
            obsqcsub[obsqcsub <= wrfda_miss_int] = nc.default_fillvals['i4']

            # store values in output data dictionary
            outdata[varDict[value]['valKey']] = obsdatasub
            outdata[varDict[value]['errKey']] = obserrsub
            outdata[varDict[value]['qcKey']] = obsqcsub.astype(int)

        # var metadata
        var_mdata['variable_names'] = writer.FillNcVector(var_names, "string")
        var_mdata['sensor_channel'] = np.asarray(chanlist)

        # global attributes

        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor

        # set dimension lengths in the writer since we are bypassing
        # ExtractObsData
        writer._nvars = nchans
        writer._nlocs = self.nlocs

        writer.BuildNetcdf(outdata, loc_mdata, var_mdata,
                           AttrData, units_values, test_mdata)
        print("Satellite radiance obs processed, wrote to:")
        print(outname)
