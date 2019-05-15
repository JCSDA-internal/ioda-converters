# gsi_ncdiag.py
# a collection of classes, and supporting information
# to read in GSI netCDF diagnostic files and rewrite them
# into JEDI UFO GeoVaLs and IODA observation files
###############################################################################
###############################################################################
# dictionaries and lists
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
        'aircraft',
        'sondes',
        'satwind',
        'vadwind',
        'windprof',
        'sfcship',
        'sfc',
        'scatwind',
    ],
    "conv_gps": [
        'gps',
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
    "gps": [3, 4, 745],
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
}

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

gsi_add_vars = {
    'Observation_Type': 'ObsType',
    'Prep_Use_Flag': 'PreUseFlag',
    'Analysis_Use_Flag': 'GsiUseFlag',
    'Nonlinear_QC_Rel_Wgt': 'GsiQCWeight',
    'Errinv_Adjust': 'GsiAdjustObsError',
    'Errinv_Final': 'GsiFinalObsError',
    'Forecast_adjusted': 'GsiHofXBc',
    'Forecast_unadjusted': 'GsiHofX',
    'Inverse_Observation_Error': 'GsiFinalObsError',
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

# values that should be integers
gsiint = [
    'PreUseFlag',
    'GsiUseFlag',
]

# geovals_vars = {gsiname:geoval_name}
geovals_vars = {
    'virtual_temperature': 'virtual_temperature',
    'atmosphere_ln_pressure_coordinate': 'atmosphere_ln_pressure_coordinate',
    'air_temperature': 'air_temperature',
    'specific_humidity': 'specific_humidity',
    'northward_wind': 'northward_wind',
    'eastward_wind': 'eastward_wind',
    'geopotential_height': 'geopotential_height',
    'height': 'height_above_mean_sea_level',
    'surface_pressure': 'surface_pressure',
    'surface_temperature': 'surface_temperature',
    'surface_roughness': 'surface_roughness_length',
    'surface_height': 'surface_geopotential_height',
    'landmask': 'Land_Fraction',
}

geovals_metadata_dict = {
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

geovals_vars = {
    'air_temperature': 'air_temperature',
    'air_pressure': 'air_pressure',
    'air_pressure_levels': 'air_pressure_levels',
    'atmosphere_absorber_01': 'humidity_mixing_ratio',
    'atmosphere_absorber_02': 'mass_concentration_of_carbon_dioxide_in_air',
    'atmosphere_absorber_03': 'mass_concentration_of_ozone_in_air',
    'atmosphere_mass_content_of_cloud_01': 'atmosphere_mass_content_of_cloud_liquid_water',
    'effective_radius_of_cloud_particle_01': 'effective_radius_of_cloud_liquid_water_particle',
    'atmosphere_mass_content_of_cloud_02': 'atmosphere_mass_content_of_cloud_ice',
    'effective_radius_of_cloud_particle_02': 'effective_radius_of_cloud_ice_particle',
    'Water_Fraction': 'Water_Fraction',
    'Land_Fraction': 'Land_Fraction',
    'Ice_Fraction': 'Ice_Fraction',
    'Snow_Fraction': 'Snow_Fraction',
    'Water_Temperature': 'Water_Temperature',
    'Land_Temperature': 'Land_Temperature',
    'Ice_Temperature': 'Ice_Temperature',
    'Snow_Temperature': 'Snow_Temperature',
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
    'mass_concentration_of_ozone_in_air': 'mass_concentration_of_ozone_in_air',
}

aod_sensors = [
    'modis',
    'viirs',
]

oz_sensors = [
    'gome',
    'sbuv2',
]

###############################################################################
###############################################################################


# conventional observations
class Conv:
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
        import netCDF4 as nc
        import datetime as dt
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
        import numpy as np
        import netCDF4 as nc
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
                if not clobber:
                    if (os.path.exists(outname)):
                        print("File exists. Skipping and not overwriting:")
                        print(outname)
                        continue
                OutVars = []
                InVars = []
                for ncv in self.df.variables:
                    if ncv in geovals_vars:
                        OutVars.append(geovals_vars[ncv])
                        InVars.append(ncv)

                idx = grabobsidx(self.df, p, v)
                if (np.sum(idx) == 0):
                    print("No matching observations for:")
                    print("Platform:" + p + " Var:" + v)
                    continue
                print(str(np.sum(idx))+" matching observations for:")
                print("Platform:" + p + " Var:" + v)
                # set up output file
                ncout = nc.Dataset(outname, 'w', format='NETCDF4')
                ncout.setncattr(
                    "date_time", np.int32(
                        self.validtime.strftime("%Y%m%d%H")))
                # get nlocs
                nlocs = np.sum(idx)
                ncout.createDimension("nlocs", nlocs)
                # other dims
                ncout.createDimension(
                    "nlevs", self.df.dimensions["atmosphere_ln_pressure_coordinate_arr_dim"].size)
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

    def toIODAobs(self, OutDir, clobber=True):
        """ toIODAobs(OutDir,clobber=True)
     output observations from the specified GSI diag file
     to the JEDI/IODA observation format
        """
        import ioda_conv_ncio as iconv
        import os
        from collections import defaultdict, OrderedDict
        from orddicts import DefaultOrderedDict
        import numpy as np
        import datetime as dt
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
                if not clobber:
                    if (os.path.exists(outname)):
                        print("File exists. Skipping and not overwriting:")
                        print(outname)
                        continue
                RecKeyList = []
                LocKeyList = []
                LocVars = []
                AttrData = {}
                varDict = defaultdict(lambda: defaultdict(dict))
                outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
                # get list of location variable for this var/platform
                for ncv in self.df.variables:
                    if ncv in all_LocKeyList:
                        LocKeyList.append(all_LocKeyList[ncv])
                        LocVars.append(ncv)
                LocKeyList.append(
                    ('ObsIndex', 'integer'))  # to ensure unique obs

                # grab obs to process
                idx = grabobsidx(self.df, p, v)
                if (np.sum(idx) == 0):
                    print("No matching observations for:")
                    print("Platform:" + p + " Var:" + v)
                    continue

                # for now, record len is 1 and the list is empty?
                recKey = 0
                writer = iconv.NcWriter(outname, RecKeyList, LocKeyList)

                outvars = conv_varnames[v]
                for value in outvars:
                    varDict[value]['valKey'] = value, writer.OvalName()
                    varDict[value]['errKey'] = value, writer.OerrName()
                    varDict[value]['qcKey'] = value, writer.OqcName()

                for o in range(len(outvars)):
                    obsdata = self.df[conv_gsivarnames[v][o]][idx]
                    obserr = 1.0 / self.df['Errinv_Input'][idx]
                    try:
                        obsqc = self.df['Prep_QC_Mark'][idx]
                    except BaseException:
                        obsqc = np.ones_like(obsdata) * 2
                    if (v == 'uv'):
                        gsivars = gsi_add_vars_uv
                    else:
                        gsivars = gsi_add_vars
                    gsimeta = {}
                    for key, value in gsivars.items():
                        # some special actions need to be taken depending on
                        # var name...
                        if "Errinv" in key:
                            try:
                                gsimeta[key] = 1.0 / self.df[key][idx]
                            except IndexError:
                                pass
                        else:
                            try:
                                gsimeta[key] = self.df[key][idx]
                            except IndexError:
                                pass
                    locKeys = []
                    for lvar in LocVars:
                        if lvar == 'Station_ID':
                            tmp = self.df[lvar][idx]
                            locKeys.append([b''.join(tmp[a])
                                            for a in range(len(tmp))])
                        elif lvar == 'Time':  # need to process into time stamp strings #"%Y-%m-%dT%H:%M:%SZ"
                            tmp = self.df[lvar][idx]
                            obstimes = [self.validtime + dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                            obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                            locKeys.append(obstimes)
                        else:
                            locKeys.append(self.df[lvar][idx])
                    # again to ensure unique obs
                    locKeys.append(np.arange(1, len(obsdata) + 1))
                    locKeys = np.swapaxes(np.array(locKeys), 0, 1)
                    locKeys = [tuple(a) for a in locKeys]
                    for i in range(len(obsdata)):
                        # observation data
                        outdata[recKey][locKeys[i]][varDict[outvars[o]]['valKey']] = obsdata[i]
                        # observation error
                        outdata[recKey][locKeys[i]][varDict[outvars[o]]['errKey']] = obserr[i]
                        # observation prep qc mark
                        outdata[recKey][locKeys[i]][varDict[outvars[o]]['qcKey']] = int(obsqc[i])
                        # add additional GSI variables that are not needed long
                        # term but useful for testing
                        for key, value in gsivars.items():
                            gvname = outvars[o], value
                            if value in gsiint:
                                try:
                                    outdata[recKey][locKeys[i]][gvname] = int(gsimeta[key][i])
                                except KeyError:
                                    pass
                            else:
                                try:
                                    outdata[recKey][locKeys[i]][gvname] = gsimeta[key][i]
                                except KeyError:
                                    pass

                AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
                (ObsVars, RecMdata, LocMdata, VarMdata) = writer.ExtractObsData(outdata)
                writer.BuildNetcdf(ObsVars, RecMdata, LocMdata, VarMdata, AttrData)
                print(str(len(obsdata))+" Conventional obs processed, wrote to:")
                print(outname)


def grabobsidx(obsdata, platform, var):
    """ grabobsidx(obsdata,platform,var):
    obsdata  - netCDF dataset object
    platform - string of observation type: 'sondes','sfc',etc.
    var      - string of variable type: 'tsen','tv','q', etc.

    returns idx - indices of observations to write out
    """
    import numpy as np

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
class Radiances:
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
        import netCDF4 as nc
        import datetime as dt
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
        import numpy as np
        import netCDF4 as nc

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

    def toIODAobs(self, OutDir, clobber=True):
        """ toIODAobs(OutDir,clobber=True)
     output observations from the specified GSI diag file
     to the JEDI/IODA observation format
        """
        import ioda_conv_ncio as iconv
        import os
        from collections import defaultdict, OrderedDict
        from orddicts import DefaultOrderedDict
        import numpy as np
        import datetime as dt
        import netCDF4 as nc
        # set up a NcWriter class
        outname = OutDir + '/' + self.sensor + '_' + self.satellite + \
            '_obs_' + self.validtime.strftime("%Y%m%d%H") + '.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting:")
                print(outname)
                return
        RecKeyList = []
        LocKeyList = []
        LocVars = []
        AttrData = {}
        varDict = defaultdict(lambda: defaultdict(dict))
        outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        rec_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        # get list of location variable for this var/platform
        for ncv in self.df.variables:
            if ncv in all_LocKeyList:
                LocKeyList.append(all_LocKeyList[ncv])
                LocVars.append(ncv)
        # LocKeyList.append(('ObsIndex','integer')) # to ensure unique obs
        # for now, record len is 1 and the list is empty?
        recKey = 0
        writer = iconv.NcWriter(outname, RecKeyList, LocKeyList)

        chan_number = self.df['sensor_chan'][:]
        chan_number = chan_number[chan_number >= 0]
        chan_indx = self.df['Channel_Index'][:]
        nchans = len(chan_number)
        nlocs = int(self.nobs / nchans)
        chanlist = chan_indx[:nchans]
        for a in chanlist:
            value = "brightness_temperature_{:d}".format(a)
            varDict[value]['valKey'] = value, writer.OvalName()
            varDict[value]['errKey'] = value, writer.OerrName()
            varDict[value]['qcKey'] = value, writer.OqcName()

        obsdata = self.df['Observation'][:]
        obserr = self.df['error_variance'][:]
        obsqc = self.df['QC_Flag'][:].astype(int)
        gsivars = gsi_add_vars

        # loop through channels for subset
        var_names = []
        for c in range(len(chanlist)):
            value = "brightness_temperature_{:d}".format(chanlist[c])
            var_names.append(value)
            print(self.obstype, value)
            idx = chan_indx == chanlist[c]
            if (np.sum(idx) == 0):
                print("No matching observations for:")
                print(value)
                continue
            obsdatasub = obsdata[idx]
            obsdatasub[obsdatasub > 9e5] = np.abs(nc.default_fillvals['f4'])
            obserrsub = np.full(nlocs, obserr[c])
            obsqcsub = obsqc[idx]
            for lvar in LocVars:
                loc_mdata_name = all_LocKeyList[lvar][0]
                if lvar == 'Obs_Time':
                    tmp = self.df[lvar][idx]
                    obstimes = [self.validtime + dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                    obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                    loc_mdata[loc_mdata_name] = writer.FillNcVector(obstimes, "datetime")
                else:
                    loc_mdata[loc_mdata_name] = self.df[lvar][idx]
            gsimeta = {}
            for key, value2 in gsivars.items():
                # some special actions need to be taken depending on var
                # name...
                if "Inverse" in key:
                    try:
                        outvals = 1.0 / self.df[key][idx]
                        outvals[np.isinf(outvals)] = np.abs(nc.default_fillvals['f4'])
                        gsimeta[key] = outvals
                    except IndexError:
                        pass
                else:
                    try:
                        gsimeta[key] = self.df[key][idx]
                    except IndexError:
                        pass

            # store values in output data dictionary
            outdata[varDict[value]['valKey']] = obsdatasub
            outdata[varDict[value]['errKey']] = obserrsub
            outdata[varDict[value]['qcKey']] = obsqcsub.astype(int)

            # add additional GSI variables that are not needed long term but
            # useful for testing
            for key, value2 in gsivars.items():
                gvname = value, value2
                if value2 in gsiint:
                    try:
                        outdata[gvname] = gsimeta[key].astype(int)
                        print(gvname)
                        print(outdata[gvname])
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
                var_mdata[value2] = self.df[key][:nchans]
            except IndexError:
                pass

        # dummy record metadata, for now
        nrecs = 1
        rec_mdata['rec_id'] = np.asarray([999], dtype='i4')
        loc_mdata['record_number'] = np.full((nlocs), 1, dtype='i4')

        # global attributes

        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor

        # set dimension lengths in the writer since we are bypassing
        # ExtractObsData
        writer._nrecs = nrecs
        writer._nvars = nchans
        writer._nlocs = nlocs

        writer.BuildNetcdf(outdata, rec_mdata, loc_mdata, var_mdata, AttrData)
        print("Satellite radiance obs processed, wrote to:")
        print(outname)


# atmospheric composition observations
class AOD:
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
        import netCDF4 as nc
        import datetime as dt
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
        import numpy as np
        import netCDF4 as nc

        # set up output file
        outname = OutDir+'/'+self.obstype+'_geoval_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
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
        """ toIODAobs(OutDir,clobber=True)
   output observations from the specified GSI diag file
   to the JEDI/IODA observation format
"""
        import ioda_conv_ncio as iconv
        import os
        from collections import defaultdict, OrderedDict
        from orddicts import DefaultOrderedDict
        import numpy as np
        import datetime as dt
        # set up a NcWriter class
        outname = OutDir+'/'+self.obstype+'_obs_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting:")
                print(outname)
                return
        RecKeyList = []
        LocKeyList = []
        LocVars = []
        AttrData = {}
        varDict = defaultdict(lambda: defaultdict(dict))
        outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        rec_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        # get list of location variable for this var/platform
        for ncv in self.df.variables:
            if ncv in all_LocKeyList:
                LocKeyList.append(all_LocKeyList[ncv])
                LocVars.append(ncv)
        # for now, record len is 1 and the list is empty?
        recKey = 0
        writer = iconv.NcWriter(outname, RecKeyList, LocKeyList)

        chan_number = self.df['sensor_chan'][:]
        chan_number = chan_number[chan_number >= 0]
        chan_indx = self.df['Channel_Index'][:]
        nchans = len(chan_number)
        nlocs = self.nobs / nchans
        chanlist = chan_indx[:nchans]
        for a in chanlist:
            value = "aerosol_optical_depth_{:d}".format(a)
            varDict[value]['valKey'] = value, writer.OvalName()
            varDict[value]['errKey'] = value, writer.OerrName()
            varDict[value]['qcKey'] = value, writer.OqcName()

        obsdata = self.df['Observation'][:]
        obserr = 1.0/self.df['Observation_Error'][:]
        obsqc = self.df['QC_Flag'][:].astype(int)

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
                    tmp = self.df[lvar][idx]
                    obstimes = [self.validtime+dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                    obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                    loc_mdata[loc_mdata_name] = writer.FillNcVector(obstimes, "datetime")
                else:
                    loc_mdata[loc_mdata_name] = self.df[lvar][idx]
            gsimeta = {}
            for key, value2 in gsivars.items():
                # some special actions need to be taken depending on var name...
                if "Inverse" in key:
                    try:
                        gsimeta[key] = 1.0/self.df[key][idx]
                    except IndexError:
                        pass
                else:
                    try:
                        gsimeta[key] = self.df[key][idx]
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
                var_mdata[value2] = self.df[key][:nchans]
            except IndexError:
                pass

        # dummy record metadata, for now
        nrecs = 1
        rec_mdata['rec_id'] = np.asarray([999], dtype='i4')
        loc_mdata['record_number'] = np.full((nlocs), 1, dtype='i4')

        # global attributes
        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor

        # set dimension lengths in the writer since we are bypassing ExtractObsData
        writer._nrecs = nrecs
        writer._nvars = nchans
        writer._nlocs = nlocs

        writer.BuildNetcdf(outdata, rec_mdata, loc_mdata, var_mdata, AttrData)
        print("AOD obs processed, wrote to:")
        print(outname)


class Ozone:
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
        import netCDF4 as nc
        import datetime as dt
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
        import numpy as np
        import netCDF4 as nc

        # set up output file
        outname = OutDir+'/'+self.sensor+'_'+self.satellite+'_geoval_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
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
        import ioda_conv_ncio as iconv
        import os
        from collections import defaultdict, OrderedDict
        from orddicts import DefaultOrderedDict
        import numpy as np
        import datetime as dt
        # set up a NcWriter class
        outname = OutDir+'/'+self.sensor+'_'+self.satellite+'_obs_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
            if (os.path.exists(outname)):
                print("File exists. Skipping and not overwriting:")
                print(outname)
                return
        RecKeyList = []
        LocKeyList = []
        LocVars = []
        AttrData = {}
        varDict = defaultdict(lambda: defaultdict(dict))
        outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        # get list of location variable for this var/platform
        for ncv in self.df.variables:
            if ncv in all_LocKeyList:
                LocKeyList.append(all_LocKeyList[ncv])
                LocVars.append(ncv)
        # for now, record len is 1 and the list is empty?
        recKey = 0
        writer = iconv.NcWriter(outname, RecKeyList, LocKeyList)

        nlocs = self.nobs
        vname = "mass_concentration_of_ozone_in_air"
        varDict[vname]['valKey'] = vname, writer.OvalName()
        varDict[vname]['errKey'] = vname, writer.OerrName()
        varDict[vname]['qcKey'] = vname, writer.OqcName()

        obsdata = self.df['Observation'][:]
        obserr = 1.0 / self.df['Inverse_Observation_Error'][:]
        obsqc = self.df['Analysis_Use_Flag'][:].astype(int)
        locKeys = []
        for lvar in LocVars:
            if lvar == 'Time':
                tmp = self.df[lvar][:]
                obstimes = [self.validtime+dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
                obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
                locKeys.append(obstimes)
            else:
                locKeys.append(self.df[lvar][:])

        locKeys = np.swapaxes(np.array(locKeys), 0, 1)
        locKeys = [tuple(a) for a in locKeys]

        gsimeta = {}
        for key, value2 in gsi_add_vars.items():
            # some special actions need to be taken depending on var name...
            if "Inverse" in key:
                try:
                    gsimeta[key] = 1.0/self.df[key][:]
                except IndexError:
                    pass
            else:
                try:
                    gsimeta[key] = self.df[key][:]
                except IndexError:
                    pass
        # not sure how to do this without a loop since it's a dict...
        for i in range(len(obsdata)):
            # observation data
            outdata[recKey][locKeys[i]][varDict[vname]['valKey']] = obsdata[i]
            outdata[recKey][locKeys[i]][varDict[vname]['errKey']] = obserr[i]
            outdata[recKey][locKeys[i]][varDict[vname]['qcKey']] = obsqc[i]
            # add additional GSI variables that are not needed long term but useful for testing
            for key, value2 in gsi_add_vars.items():
                gvname = vname, value2
                if value2 in gsiint:
                    try:
                        outdata[recKey][locKeys[i]][gvname] = int(gsimeta[key][i])
                    except KeyError:
                        pass
                else:
                    try:
                        outdata[recKey][locKeys[i]][gvname] = gsimeta[key][i]
                    except KeyError:
                        pass

        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor
        (ObsVars, RecMdata, LocMdata, VarMdata) = writer.ExtractObsData(outdata)
        writer.BuildNetcdf(ObsVars, RecMdata, LocMdata, VarMdata, AttrData)
        print("Ozone obs processed, wrote to:")
        print(outname)
