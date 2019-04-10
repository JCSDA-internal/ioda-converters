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
  def __init__(self,filename):
    self.filename = filename
    splitfname = self.filename.split('/')[-1].split('_')
    if 'conv' in splitfname:
      i = splitfname.index('conv')
      self.obstype = "_".join(splitfname[i:i+2])
    else:
      raise ValueError("Observation is not a conventional type...")
    # below is because T has both T and Tv, others should just be 'value' but flexibility for later (GPS?)
    if self.obstype == 'conv_t':
      self.obsvars = ['tv','tsen']
    elif self.obstype == 'conv_gps':
      self.obsvars = ['bend','refract']
    else:
      self.obsvars = [splitfname[i+i]]

  def read(self):
    import netCDF4 as nc
    import datetime as dt
    # get valid time
    df = nc.Dataset(self.filename)
    tstr = str(df.getncattr('date_time'))
    self.validtime = dt.datetime.strptime(tstr,"%Y%m%d%H")
    # number of observations
    self.nobs = len(df['Observation_Type'][:])
    self.df = df
    
  def toGeovals(self,OutDir,clobber=True):
    """ toGeovals(OutDir,clobber=True) 
 if model state fields are in the GSI diag file, create
 GeoVaLs in an output file for use by JEDI/UFO
    """
    # note, this is a temporary construct and thus, there is no
    # ioda_conv_ncio or equivalent to handle the format
    import conv_dicts as cd
    import numpy as np
    import netCDF4 as nc
    # get list of platforms to process for the given obstype
    try: 
      platforms = cd.conv_platforms[self.obstype]
    except:
      print(self.obstype+" is not currently supported. Exiting.")
      return
    # loop through obsvariables and platforms to do processing
    for v in self.obsvars:
      for p in platforms:
        outname = OutDir+'/'+p+'_'+v+'_geovals_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
          if (os.path.exists(outname)):
            print("File exists. Skipping and not overwriting:")
            print(outname)
            continue
        OutVars = []
        InVars = []
        for ncv in self.df.variables:
          if ncv in cd.geovals_vars:
            OutVars.append(cd.geovals_vars[ncv])
            InVars.append(ncv) 

        idx = grabobsidx(self.df,p,v)
        if (np.sum(idx)==0):
          print("No matching observations for:")
          print("Platform:"+p+" Var:"+v)
          continue 
        # set up output file
	ncout = nc.Dataset(outname,'w',format='NETCDF4')
        ncout.setncattr("date_time",int(self.validtime.strftime("%Y%m%d%H")))
        # get nlocs
        nlocs = np.sum(idx)
        ncout.createDimension("nlocs",nlocs)
        # other dims 
        ncout.createDimension("nlevs",self.df.dimensions["atmosphere_ln_pressure_coordinate_arr_dim"].size)
        dimname = "Station_ID_maxstrlen" 
        ncout.createDimension(dimname, self.df.dimensions[dimname].size)
        dimname = "Observation_Class_maxstrlen"
        ncout.createDimension(dimname, self.df.dimensions[dimname].size)
        for var in self.df.variables.values():
          vname = var.name
          if (vname in cd.geovals_metadata_dict.keys()) or (vname in cd.geovals_vars.keys()):
            vdata = var[...].data
            dims = tuple([len(self.df.dimensions[d]) for d in var.dimensions])
            vdata = np.frombuffer(vdata,dtype=var.dtype)
            vdata = np.reshape(vdata,dims)
            if vname in cd.geovals_metadata_dict.keys():
              dims = ("nlocs",)+var.dimensions[1:]
              var_out = ncout.createVariable(cd.geovals_metadata_dict[vname], vdata.dtype, dims)
              var_out[...] = vdata[idx,...]
            if vname in cd.geovals_vars.keys():
              if (len(var.dimensions) == 1):
                dims = ("nlocs",)
              else:
                dims = ("nlocs", "nlevs")
              var_out = ncout.createVariable(cd.geovals_vars[vname], vdata.dtype, dims)
              var_out[...] = vdata[idx,...]
              # also output pressure not just ln pressure
              if vname == "atmosphere_ln_pressure_coordinate":
                var_out = ncout.createVariable(u"air_pressure", vdata.dtype, dims)
                var_out[...] = 1000.*np.exp(vdata[idx,...])
        ncout.close() 

  def toIODAobs(self,OutDir,clobber=True):
    """ toIODAobs(OutDir,clobber=True)
 output observations from the specified GSI diag file
 to the JEDI/IODA observation format
    """ 
    import ioda_conv_ncio as iconv
    import os
    from collections import defaultdict
    import conv_dicts as cd
    import numpy as np
    # get list of platforms to process for the given obstype
    try: 
      platforms = cd.conv_platforms[self.obstype]
    except:
      print(self.obstype+" is not currently supported. Exiting.")
      return
    # loop through obsvariables and platforms to do processing
    for v in self.obsvars:
      for p in platforms:
        # set up a NcWriter class
        outname = OutDir+'/'+p+'_'+v+'_obs_'+self.validtime.strftime("%Y%m%d%H")+'.nc4'
        if not clobber:
          if (os.path.exists(outname)):
            print("File exists. Skipping and not overwriting:")
            print(outname)
            continue
        RecKeyList = [] ; LocKeyList = [] ; LocVars = [] ; AttrData = {}
        varDict = defaultdict(lambda: defaultdict(dict))
        outdata = defaultdict(lambda: defaultdict(dict))
        # get list of location variable for this var/platform
        for ncv in self.df.variables:
          if ncv in cd.LocKeyList:
            LocKeyList.append(cd.LocKeyList[ncv])
            LocVars.append(ncv)
        LocKeyList.append(('ObsIndex','integer')) # to ensure unique obs

        # grab obs to process
        idx = grabobsidx(self.df,p,v)
        if (np.sum(idx)==0):
          print("No matching observations for:")
          print("Platform:"+p+" Var:"+v)
          continue

        # for now, record len is 1 and the list is empty?
        recKey=0
        writer = iconv.NcWriter(outname,RecKeyList,LocKeyList)
         
        outvars = cd.varnames[v]
        for value in outvars:
          varDict[value]['valKey'] = value, writer.OvalName()
          varDict[value]['errKey'] = value, writer.OerrName()
          varDict[value]['qcKey'] = value, writer.OqcName()

        for o in range(len(outvars)):
          obsdata = self.df[cd.gsivarnames[v][o]][idx]
          obserr = 1.0/self.df['Errinv_Input'][idx]
          try:
            obsqc = self.df['Prep_QC_Mark'][idx]
          except:
            obsqc = np.ones_like(obsdata)*2
          gsivars = cd.gsi_add_vars
          gsimeta = {}
          for key, value in gsivars.items():
            # some special actions need to be taken depending on var name...
            if "Errinv" in key:
              gsimeta[key] = 1.0/self.df[key][idx]
            elif "Forecast" in key:
              if "u_Obs" in key:
                gsimeta[key] = self.df['u_Observation'][idx] - self.df[key][idx]
              elif "v_Obs" in key:
                gsimeta[key] = self.df['v_Observation'][idx] - self.df[key][idx]
              else:
                gsimeta[key] = self.df['Observation'][idx] - self.df[key][idx]
            else:
              gsimeta[key] = self.df[key][idx] 
          locKeys = []
          for l in LocVars:
            if l == 'Station_ID':
              tmp = self.df[l][idx]
              locKeys.append([''.join(tmp[a]) for a in range(len(tmp))])
            else:
              locKeys.append(self.df[l][idx])
          locKeys.append(np.arange(1,len(obsdata)+1)) # again to ensure unique obs
          locKeys = np.swapaxes(np.array(locKeys),0,1)
          locKeys = [tuple(a) for a in locKeys]
          # not sure how to do this without a loop since it's a dict...
          for i in range(len(obsdata)):
            # observation data
            outdata[recKey][locKeys[i]][varDict[outvars[o]]['valKey']] = obsdata[i]
            # observation error
            outdata[recKey][locKeys[i]][varDict[outvars[o]]['errKey']] = obserr[i]
            # observation prep qc mark
            outdata[recKey][locKeys[i]][varDict[outvars[o]]['qcKey']] = obsqc[i]
            # add additional GSI variables that are not needed long term but useful for testing
            for key, value in gsivars.items():
              gvname = outvars[o],value
              outdata[recKey][locKeys[i]][gvname] = gsimeta[key][i] 
          
        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        writer.BuildNetcdf(outdata,AttrData)
        print("Conventional obs processed, wrote to:")
        print(outname)

def grabobsidx(obsdata,platform,var):
  """ grabobsidx(obsdata,platform,var):
  obsdata  - netCDF dataset object
  platform - string of observation type: 'sondes','sfc',etc.
  var      - string of variable type: 'tsen','tv','q', etc.

  returns idx - indices of observations to write out
  """
  import numpy as np
  import conv_dicts as cd

  code = obsdata['Observation_Type'][:]
  if var in ['tsen','tv']:
    iqt = obsdata['Setup_QC_Mark'][:]
    if var == 'tsen':
      idx2 = (iqt != 0)
    elif var == 'tv':
      idx2 = (iqt == 0)
  elif var in ['bend','refract']:
    igps = obsdata['GPS_Type'][:]
    if var == 'bend':
      idx2 = (igps !=0)
    elif var == 'refract':
      idx2 = (igps ==0) 
  else:
    # to be consistent
    idx2 = (code > -999)
  # grab np logical based off of conv_dicts entry
  if var == 'uv':
    codes = cd.uv_bufrtypes[platform]
  else:
    codes = cd.bufrtypes[platform]
  idx = np.logical_and(np.in1d(code,codes),idx2)

  return idx

