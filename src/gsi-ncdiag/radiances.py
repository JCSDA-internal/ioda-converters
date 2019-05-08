# satellite radiance observations
import rad_dicts as rd


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
        for s in rd.sensors:
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
            if ncv in rd.geovals_vars:
                OutVars.append(rd.geovals_vars[ncv])
                InVars.append(ncv)

        # set up output file
        ncout = nc.Dataset(outname, 'w', format='NETCDF4')
        ncout.setncattr(
            "date_time", np.int32(
                self.validtime.strftime("%Y%m%d%H")))
        ncout.setncattr("satellite", self.satellite)
        ncout.setncattr("sensor", self.sensor)
        # get nlocs
        nlocs = self.nobs / self.nchans
        ncout.createDimension("nlocs", nlocs)
        # other dims
        ncout.createDimension(
            "nlevs", self.df.dimensions["air_temperature_arr_dim"].size)
        ncout.createDimension(
            "nlevsp1",
            self.df.dimensions["air_pressure_levels_arr_dim"].size)
        for var in self.df.variables.values():
            vname = var.name
            if vname in rd.geovals_metadata_dict.keys():
                dims = ("nlocs",)
                var_out = ncout.createVariable(
                    rd.geovals_metadata_dict[vname], var.dtype, dims)
                vdata = var[:]
                vdata = vdata[::self.nchans]
                var_out[:] = vdata
            elif vname in rd.geovals_vars.keys():
                if (len(var.dimensions) == 1):
                    dims = ("nlocs",)
                elif "_levels" in vname:
                    dims = ("nlocs", "nlevsp1")
                else:
                    dims = ("nlocs", "nlevs")
                var_out = ncout.createVariable(
                    rd.geovals_vars[vname], var.dtype, dims)
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
        import rad_dicts as rd
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
            if ncv in rd.LocKeyList:
                LocKeyList.append(rd.LocKeyList[ncv])
                LocVars.append(ncv)
        # LocKeyList.append(('ObsIndex','integer')) # to ensure unique obs
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
            value = "brightness_temperature_{:d}".format(a)
            varDict[value]['valKey'] = value, writer.OvalName()
            varDict[value]['errKey'] = value, writer.OerrName()
            varDict[value]['qcKey'] = value, writer.OqcName()

        obsdata = self.df['Observation'][:]
        obserr = self.df['error_variance'][:]
        obsqc = self.df['QC_Flag'][:]
        gsivars = rd.gsi_add_vars

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
            obserrsub = np.full((nlocs), obserr[c])
            obsqcsub = obsqc[idx]
            for lvar in LocVars:
                loc_mdata_name = rd.LocKeyList[lvar][0]
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
                    outvals = 1.0 / self.df[key][idx]
                    outvals[np.isinf(outvals)] = np.abs(nc.default_fillvals['f4'])
                    gsimeta[key] = outvals
                elif "unadjusted" in key:
                    gsimeta[key] = obsdatasub - self.df[key][idx]
                elif "_adjusted" in key:
                    gsimeta[key] = self.df[key][idx] - \
                        self.df["Obs_Minus_Forecast_unadjusted"][idx]
                else:
                    gsimeta[key] = self.df[key][idx]

            # store values in output data dictionary
            outdata[varDict[value]['valKey']] = obsdatasub
            outdata[varDict[value]['errKey']] = obserrsub
            outdata[varDict[value]['qcKey']] = obsqcsub

            # add additional GSI variables that are not needed long term but
            # useful for testing
            for key, value2 in gsivars.items():
                gvname = value, value2
                outdata[gvname] = gsimeta[key]

        # var metadata
        var_mdata['variable_names'] = writer.FillNcVector(var_names, "string")
        for key, value2 in rd.chan_metadata_dict.items():
            var_mdata[value2] = self.df[key][:nchans]

        # dummy record metadata, for now
        nrecs = 1
        rec_mdata['rec_id'] = np.asarray([999], dtype='i4')
        loc_mdata['record_number'] = np.full((nlocs), 1, dtype='i4')

        # global attributes

        AttrData["date_time_string"] = self.validtime.strftime(
            "%Y-%m-%dT%H:%M:%SZ")
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
