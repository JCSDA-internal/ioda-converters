# atmospheric composition observations
import oz_dicts as ozd
import aod_dicts as aodd

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
  def __init__(self,filename):
    self.filename = filename
    splitfname = self.filename.split('/')[-1].split('_')
    i = False
    for s in aodd.sensors:
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
    self.validtime = dt.datetime.strptime(tstr,"%Y%m%d%H")
    # number of observations
    self.nobs = len(df.dimensions['nobs'])
    self.nchans = len(df.dimensions['nchans'])
    self.df = df

  def toGeovals(self,OutDir,clobber=True):
    """ toGeovals(OutDir,clobber=True) 
 if model state fields are in the GSI diag file, create
 GeoVaLs in an output file for use by JEDI/UFO
    """
    # note, this is a temporary construct and thus, there is no
    # ioda_conv_ncio or equivalent to handle the format
    import numpy as np
    import netCDF4 as nc

    # set up output file 
    outname = OutDir+'/'+self.obstype+'_geovals_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
    if not clobber:
      if (os.path.exists(outname)):
        print("File exists. Skipping and not overwriting:")
        print(outname)
        return
    OutVars = []
    InVars = []
    for ncv in self.df.variables:
      if ncv in aodd.geovals_vars:
        OutVars.append(aodd.geovals_vars[ncv])
        InVars.append(ncv) 

    # set up output file
    ncout = nc.Dataset(outname,'w',format='NETCDF4')
    ncout.setncattr("date_time",int(self.validtime.strftime("%Y%m%d%H")))
    ncout.setncattr("satellite",self.satellite)
    ncout.setncattr("sensor",self.sensor)
    # get nlocs
    nlocs = self.nobs 
    ncout.createDimension("nlocs",nlocs)
    # other dims 
    ncout.createDimension("nlevs",self.df.dimensions["air_temperature_arr_dim"].size)
    ncout.createDimension("nlevsp1",self.df.dimensions["air_pressure_levels_arr_dim"].size)
    for var in self.df.variables.values():
      vname = var.name
      if vname in aodd.geovals_metadata_dict.keys():
        dims = ("nlocs",)
        var_out = ncout.createVariable(aodd.geovals_metadata_dict[vname], var.dtype, dims)
        vdata = var[:]
        var_out[:] = vdata
      elif vname in aodd.geovals_vars.keys():
        if (len(var.dimensions) == 1):
          dims = ("nlocs",)
        elif "_levels" in vname:
          dims = ("nlocs", "nlevsp1")
        else:
          dims = ("nlocs", "nlevs")
        var_out = ncout.createVariable(aodd.geovals_vars[vname], var.dtype, dims)
        vdata = var[...]
        var_out[...] = vdata
      else:
        pass
    ncout.close() 

  def toIODAobs(self,OutDir,clobber=True):
    """ toIODAobs(OutDir,clobber=True)
 output observations from the specified GSI diag file
 to the JEDI/IODA observation format
    """ 
    import ioda_conv_ncio as iconv
    import os
    from collections import defaultdict
    import numpy as np
    import datetime as dt
    # set up a NcWriter class
    outname = OutDir+'/'+self.obstype+'_obs_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
    if not clobber:
      if (os.path.exists(outname)):
        print("File exists. Skipping and not overwriting:")
        print(outname)
        return
    RecKeyList = [] ; LocKeyList = [] ; LocVars = [] ; AttrData = {}
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: defaultdict(dict))
    # get list of location variable for this var/platform
    for ncv in self.df.variables:
      if ncv in aodd.LocKeyList:
        LocKeyList.append(aodd.LocKeyList[ncv])
        LocVars.append(ncv)
    #LocKeyList.append(('ObsIndex','integer')) # to ensure unique obs
    # for now, record len is 1 and the list is empty?
    recKey=0
    writer = iconv.NcWriter(outname,RecKeyList,LocKeyList)

    chan_number = self.df['sensor_chan'][:]
    chan_number = chan_number[chan_number>=0]
    chan_indx = self.df['Channel_Index'][:]
    nchans = len(chan_number)
    nlocs = self.nobs /nchans
    chanlist = chan_indx[:nchans+1] 
    for a in chanlist:
      value = "aerosol_optical_depth_{:d}".format(a)
      varDict[value]['valKey'] = value, writer.OvalName()
      varDict[value]['errKey'] = value, writer.OerrName()
      varDict[value]['qcKey'] = value, writer.OqcName()

    obsdata = self.df['Observation'][:]
    obserr = 1.0/self.df['Observation_Error'][:]
    obsqc = self.df['QC_Flag'][:]

    gsivars = aodd.gsi_add_vars
    locKeys = []
    for l in LocVars:
      if l == 'Obs_Time':
        tmp = self.df[l][:]
        obstimes = [self.validtime+dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
        obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
        locKeys.append(obstimes)
      else:
        locKeys.append(self.df[l][:])
    #locKeys.append(np.arange(1,len(obsdata)+1)) # again to ensure unique obs
    locKeys = np.swapaxes(np.array(locKeys),0,1)
    locKeys = [tuple(a) for a in locKeys]

   # loop through channels for subset
    for c in range(len(chanlist)):
      value = "aerosol_optical_depth_{:d}".format(chanlist[c])
      idx = chan_indx == chanlist[c]
      obsdatasub = obsdata[idx]
      obserrsub = obserr[idx]
      obsqcsub = obsqc[idx]
      gsimeta = {}
      for key, value2 in gsivars.items():
        # some special actions need to be taken depending on var name...
        if "Inverse" in key:
          gsimeta[key] = 1.0/self.df[key][idx]
        elif "unadjusted" in key:
          gsimeta[key] = obsdatasub - self.df[key][idx]
        elif "_adjusted" in key:
          gsimeta[key] = self.df[key][idx] - self.df["Obs_Minus_Forecast_unadjusted"][idx]
        else:
          gsimeta[key] = self.df[key][idx]
      # not sure how to do this without a loop since it's a dict...
      for i in range(len(obsdatasub)):
        j = (c*len(obsdatasub)) + i
        # observation data
        outdata[recKey][locKeys[j]][varDict[value]['valKey']] = obsdatasub[i]
        # observation error
        outdata[recKey][locKeys[j]][varDict[value]['errKey']] = obserrsub[i]
        # observation prep qc mark
        outdata[recKey][locKeys[j]][varDict[value]['qcKey']] = obsqcsub[i]
        # add additional GSI variables that are not needed long term but useful for testing
        for key, value2 in gsivars.items():
          gvname = value,value2
          outdata[recKey][locKeys[j]][gvname] = gsimeta[key][i]
      # metadata 
      for key, value2 in aodd.chan_metadata_dict.items():
        outdata[recKey]['VarMetaData'][(value,value2)] = self.df[key][c]


    AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
    AttrData["satellite"] = self.satellite
    AttrData["sensor"] = self.sensor
    writer.BuildNetcdf(outdata,AttrData)
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
  def __init__(self,filename):
    self.filename = filename
    splitfname = self.filename.split('/')[-1].split('_')
    i = False
    for s in ozd.sensors:
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
    self.validtime = dt.datetime.strptime(tstr,"%Y%m%d%H")
    # number of observations
    self.nobs = len(df.dimensions['nobs'])
    self.df = df
    
  def toGeovals(self,OutDir,clobber=True):
    """ toGeovals(OutDir,clobber=True) 
 if model state fields are in the GSI diag file, create
 GeoVaLs in an output file for use by JEDI/UFO
    """
    # note, this is a temporary construct and thus, there is no
    # ioda_conv_ncio or equivalent to handle the format
    import numpy as np
    import netCDF4 as nc

    # set up output file 
    outname = OutDir+'/'+self.sensor+'_'+self.satellite+'_geovals_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
    if not clobber:
      if (os.path.exists(outname)):
        print("File exists. Skipping and not overwriting:")
        print(outname)
        return
    OutVars = []
    InVars = []
    for ncv in self.df.variables:
      if ncv in ozd.geovals_vars:
        OutVars.append(ozd.geovals_vars[ncv])
        InVars.append(ncv) 

    # set up output file
    ncout = nc.Dataset(outname,'w',format='NETCDF4')
    ncout.setncattr("date_time",int(self.validtime.strftime("%Y%m%d%H")))
    ncout.setncattr("satellite",self.satellite)
    ncout.setncattr("sensor",self.sensor)
    # get nlocs
    nlocs = self.nobs 
    ncout.createDimension("nlocs",nlocs)
    # other dims 
    ncout.createDimension("nlevs",self.df.dimensions["mass_concentration_of_ozone_in_air_arr_dim"].size)
    ncout.createDimension("nlevsp1",self.df.dimensions["air_pressure_levels_arr_dim"].size)
    for var in self.df.variables.values():
      vname = var.name
      if vname in ozd.geovals_metadata_dict.keys():
        dims = ("nlocs",)
        var_out = ncout.createVariable(ozd.geovals_metadata_dict[vname], var.dtype, dims)
        vdata = var[:]
        var_out[:] = vdata
      elif vname in ozd.geovals_vars.keys():
        if (len(var.dimensions) == 1):
          dims = ("nlocs",)
        elif "_levels" in vname:
          dims = ("nlocs", "nlevsp1")
        else:
          dims = ("nlocs", "nlevs")
        var_out = ncout.createVariable(ozd.geovals_vars[vname], var.dtype, dims)
        vdata = var[...]
        var_out[...] = vdata
      else:
        pass
    ncout.close() 

  def toIODAobs(self,OutDir,clobber=True):
    """ toIODAobs(OutDir,clobber=True)
 output observations from the specified GSI diag file
 to the JEDI/IODA observation format
    """ 
    import ioda_conv_ncio as iconv
    import os
    from collections import defaultdict
    import numpy as np
    import datetime as dt
    # set up a NcWriter class
    outname = OutDir+'/'+self.sensor+'_'+self.satellite+'_obs_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
    if not clobber:
      if (os.path.exists(outname)):
        print("File exists. Skipping and not overwriting:")
        print(outname)
        return
    RecKeyList = [] ; LocKeyList = [] ; LocVars = [] ; AttrData = {}
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: defaultdict(dict))
    # get list of location variable for this var/platform
    for ncv in self.df.variables:
      if ncv in ozd.LocKeyList:
        LocKeyList.append(ozd.LocKeyList[ncv])
        LocVars.append(ncv)
    #LocKeyList.append(('ObsIndex','integer')) # to ensure unique obs
    # for now, record len is 1 and the list is empty?
    recKey=0
    writer = iconv.NcWriter(outname,RecKeyList,LocKeyList)
     
    nlocs = self.nobs
    value = "mass_concentration_of_ozone_in_air"
    varDict[value]['valKey'] = value, writer.OvalName()
    varDict[value]['errKey'] = value, writer.OerrName()
    varDict[value]['qcKey'] = value, writer.OqcName()

    obsdata = self.df['Observation'][:]
    gsivars = ozd.gsi_add_vars
    locKeys = []
    for l in LocVars:
      if l == 'Obs_Time':
        tmp = self.df[l][:]
        obstimes = [self.validtime+dt.timedelta(hours=float(tmp[a])) for a in range(len(tmp))]
        obstimes = [a.strftime("%Y-%m-%dT%H:%M:%SZ") for a in obstimes]
        locKeys.append(obstimes)
      else:
        locKeys.append(self.df[l][:])

    #locKeys.append(np.arange(1,len(obsdata)+1)) # again to ensure unique obs
    locKeys = np.swapaxes(np.array(locKeys),0,1)
    locKeys = [tuple(a) for a in locKeys]

    gsimeta = {}
    for key, value2 in gsivars.items():
      # some special actions need to be taken depending on var name...
      if "Inverse" in key:
        gsimeta[key] = 1.0/self.df[key][:]
      elif "unadjusted" in key:
        gsimeta[key] = obsdata - self.df[key][:]
      elif "_adjusted" in key:
        gsimeta[key] = self.df[key][:] - self.df["Obs_Minus_Forecast_unadjusted"][:]
      else:
        gsimeta[key] = self.df[key][:] 
    # not sure how to do this without a loop since it's a dict...
    for i in range(len(obsdata)):
      # observation data
      outdata[recKey][locKeys[i]][varDict[value]['valKey']] = obsdata[i]
      # add additional GSI variables that are not needed long term but useful for testing
      for key, value2 in gsivars.items():
        gvname = value,value2
        outdata[recKey][locKeys[i]][gvname] = gsimeta[key][i] 
      
      
    AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
    AttrData["satellite"] = self.satellite
    AttrData["sensor"] = self.sensor
    writer.BuildNetcdf(outdata,AttrData)
    print("Ozone obs processed, wrote to:")
    print(outname)

