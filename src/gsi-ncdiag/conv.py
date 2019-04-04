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
    data	- data in format for NcWriter
    

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
    else:
      self.obsvars = [splitfname[i+2]]

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
    

  def toIODAobs(self,OutDir,clobber=True):
    """ toIODAobs(OutDir,clobber=True)
 output observations from the specified GSI diag file
 to the JEDI/IODA observation format
    """ 
    import ioda_conv_ncio as iconv
    import os
    import conv_dicts as cd
    from collections import defaultdict
    import numpy as np
    # get list of platforms to process for the given obstype
    platforms = cd.conv_platforms[self.obstype]
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
        # for now, record len is 1 and the list is empty?
        recKey=0
        writer = iconv.NcWriter(outname,RecKeyList,LocKeyList)
         
        outvars = cd.varnames[v]
        for value in outvars:
          varDict[value]['valKey'] = value, writer.OvalName()
          varDict[value]['errKey'] = value, writer.OerrName()
          varDict[value]['qcKey'] = value, writer.OqcName()

        # grab obs to process
        idx = grabobsidx(self.df,p,v)
        for o in range(len(outvars)):
	  obsdata = self.df[cd.gsivarnames[v][o]][idx]
	  obserr = 1.0/self.df['Errinv_Input'][idx]
	  obsqc = self.df['Prep_QC_Mark'][idx]
          gsivars = cd.gsi_add_vars
          gsimeta = {}
          for key, value in gsivars.items():
            gsimeta[key] = self.df[key][idx] 
            print(gsimeta[key].shape)
          locKeys = []
          for l in LocVars:
            if l == 'Station_ID':
              tmp = self.df[l][idx]
              locKeys.append([''.join(tmp[a]) for a in range(len(tmp))])
            else:
              locKeys.append(self.df[l][idx])
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
              print(gvname)
              outdata[recKey][locKeys[i]][gvname] = gsimeta[key][i] 
          
        AttrData["date_time_string"] = self.validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
        writer.BuildNetcdf(outdata,AttrData)

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

