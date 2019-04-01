conv_vars = ["tv","tsen","uv","ps","q"]

class GSIDiagFile:
  """ class GSIDiagFile - useful information for the specified
                     GSI netCDF diag file

  Functions:
   self.split(OutDir,clobber=True)
   self.genObs(OutDir,clobber=True)
   self.genGeovals(OutDir,clobber=True)

  Attributes:
   self.filename = string filename provided upon generation
   self.validtime = datetime object from GSI netCDF diag metadata
   self.obstype = uses filename to guess obstype (ex: conv_t; iasi_metop-a)
   self.nobs = number of obs in file
   self.ncdata = pointer to netCDF file in memory
   self.needs_split = logical if it is a file that needs split()
  """
  def __init__(self,filename):
    import netCDF4 as nc
    import datetime as dt
    self.filename = filename
    # get valid time 
    df = nc.Dataset(self.filename)
    tstr = str(df.getncattr('date_time'))
    self.validtime = dt.datetime.strptime(tstr,"%Y%m%d%H")
    # use some filename tricks to guess observation type, not ideal
    # but without metadata it's currently impossible to know for sure
    self.obstype="_".join(self.filename.split('/')[-1].split('_')[1:3])
    # number of observations
    self.nobs = len(df['Observation_Type'][:])
    self.ncdata = df
    self.needs_split = False
    if "conv" in self.obstype:
      self.needs_split = True

  def split(self,OutDir,clobber=True):
    """ split - splits GSI ncdiag files into files
            for separate observing platforms
    Required Parameters:
    OutDir  - string path to directory for output files
    
    Optional Parameters:
    clobber - default=True, if false will not overwrite file
    """
    print("Splitting "+self.filename)
    print("Obstype="+self.obstype+"; nobs={:d}".format(self.nobs))
    # subclasses for which type of splitting to perform
    DiagType = self.get_splittype()
    return DiagType(OutDir,clobber)

  def get_splittype(self):
    if "conv" in self.obstype:
      return self._split_conv 
    #elif "aod" in self.obstype:
    elif "rad" in self.obstype:
      return self._split_rad
    else:
      raise ValueError(self.obstype)

##########################################################################
  def _split_conv(self,OutDir,clobber=True):
    # split conventional data
    if "conv_t" in self.obstype:
      self._split_conv_t(OutDir,clobber)
    elif "conv_q" in self.obstype:
      self._split_conv_q(OutDir,clobber)
    elif "conv_ps" in self.obstype:
      self._split_conv_ps(OutDir,clobber)
    elif "conv_uv" in self.obstype:
      self._split_conv_uv(OutDir,clobber)
    else:
      print("Conventional type:"+self.obstype+" not supported")

##########################################################################
  def _split_conv_t(self,OutDir,clobber):
    import numpy as np
    import os
    iqt = self.ncdata['Setup_QC_Mark'][:]
    ind_tv = (iqt == 0); ind_ts = (iqt != 0)
    code = self.ncdata['Observation_Type'][:]
    # radiosondes are treated differently
    ind_sond = np.logical_and(code >= 120, code <= 122)
    # Copy sondes virtual temperature data
    ind = np.logical_and(ind_tv, ind_sond)
    OutFile=OutDir+"/diag_sondes_tv_"+self.validtime.strftime("%Y%m%d%H")+".nc4"
    if os.path.exists(OutFile) and not clobber:
      print("Clobber set to False! Not overwriting "+OutFile) 
    else:
      write_split_file(self.filename,OutFile,len(iqt[ind]), ind)
    # copy sondes sensible temperature data
    ind = np.logical_and(ind_ts, ind_sond)
    OutFile=OutDir+"/diag_sondes_tsen_"+self.validtime.strftime("%Y%m%d%H")+".nc4"
    if os.path.exists(OutFile) and not clobber:
      print("Clobber set to False! Not overwriting "+OutFile) 
    else:
      write_split_file(self.filename,OutFile,len(iqt[ind]), ind)
    # all other temp obstypes have sensible temperature
    obstypes = {
	"aircraft":np.logical_and(code >= 130, code <= 139),
	"rass":(code == 126),
	"sfcship":np.logical_or (code == 180, code == 183),
	"sfc":np.logical_or (code == 181, code == 187),
}
    for ob,index in obstypes.items():
      OutFile=OutDir+"/diag_"+ob+"_tsen_"+self.validtime.strftime("%Y%m%d%H")+".nc4"
      if os.path.exists(OutFile) and not clobber:
        print("Clobber set to False! Not overwriting "+OutFile) 
      else:
        write_split_file(self.filename,OutFile,len(code[index]), index)
##########################################################################
  def _split_conv_q(self,OutDir,clobber):
    import numpy as np
    import os
    code = self.ncdata['Observation_Type'][:]
    obstypes = {
	"aircraft":np.logical_and(code >= 130, code <= 139),
	"sondes":np.logical_and(code >= 120, code <= 122),
	"sfcship":np.logical_or (code == 180, code == 183),
	"sfc":np.logical_or (code == 181, code == 187),
}
    for ob,index in obstypes.items():
      OutFile=OutDir+"/diag_"+ob+"_q_"+self.validtime.strftime("%Y%m%d%H")+".nc4"
      if os.path.exists(OutFile) and not clobber:
        print("Clobber set to False! Not overwriting "+OutFile) 
      else:
        write_split_file(self.filename,OutFile,len(code[index]), index)
##########################################################################
  def _split_conv_ps(self,OutDir,clobber):
    import numpy as np
    import os
    code = self.ncdata['Observation_Type'][:]
    obstypes = {
	"sondes":np.logical_and(code >= 120, code <= 122),
	"sfcship":np.logical_or (code == 180, code == 183),
	"sfc":np.logical_or (code == 181, code == 187),
}
    for ob,index in obstypes.items():
      OutFile=OutDir+"/diag_"+ob+"_ps_"+self.validtime.strftime("%Y%m%d%H")+".nc4"
      if os.path.exists(OutFile) and not clobber:
        print("Clobber set to False! Not overwriting "+OutFile) 
      else:
        write_split_file(self.filename,OutFile,len(code[index]), index)
##########################################################################
  def _split_conv_uv(self,OutDir,clobber):
    import numpy as np
    import os
    code = self.ncdata['Observation_Type'][:]
    obstypes = {
	"aircraft":np.logical_and(code >= 230, code <= 239),
	"sondes":np.logical_and(code >= 220, code <= 222),
	"satwind":np.logical_and(code >= 240, code <= 260),
	"vadwind":(code == 224),
	"windprof":np.logical_and(code >= 227, code <= 229),
	"sfcship":np.logical_or(np.logical_or(code == 280, code == 282),code == 284),
	"sfc":np.logical_or(code == 281, code == 287),
	"scatwind":(code == 290),
}
    for ob,index in obstypes.items():
      OutFile=OutDir+"/diag_"+ob+"_uv_"+self.validtime.strftime("%Y%m%d%H")+".nc4"
      if os.path.exists(OutFile) and not clobber:
        print("Clobber set to False! Not overwriting "+OutFile) 
      else:
        write_split_file(self.filename,OutFile,len(code[index]), index)
##########################################################################
  def _split_rad(self,OutDir,clobber=True):
    print("We do not need to split radiance GSI diag files!")
    print("This is an example placeholder in case there are")
    print("other obs types that need split that aren't conv")
      
##########################################################################
  def genObs(self,OutDir,clobber=True):
    """ genObs - takes GSI ncdiag files and produces 
            IODA compatible observation files
    Required Parameters:
    OutDir  - string path to directory for output files
    
    Optional Parameters:
    clobber - default=True, if false will not overwrite file
    """
    print("Converting "+self.filename)
    print("Obstype="+self.obstype+"; nobs={:d}".format(self.nobs))
    # subclasses for which type of splitting to perform
    DiagType = self.get_obstype()
    return DiagType(OutDir,clobber)

##########################################################################
  def get_obstype(self):
    #conv_vars = ["tv","tsen","uv","ps","q"]
    if self.obstype.split("_")[-1] in conv_vars:
      return self._obs_conv 
    else:
      # we will assume this is a satellite radiance
      return self._obs_rad

##########################################################################
  def _obs_conv(self,OutDir,clobber=True):
    print(" placeholder")
##########################################################################
  def _obs_rad(self,OutDir,clobber=True):
    print(" placeholder")
##########################################################################
  def genGeovals(self,OutDir,clobber=True):
    """ genGeovals - takes GSI ncdiag files and produces 
            UFO compatible GeoVaLs files
    Required Parameters:
    OutDir  - string path to directory for output files
    
    Optional Parameters:
    clobber - default=True, if false will not overwrite file
    """
    print("Converting "+self.filename)
    print("Obstype="+self.obstype+"; nobs={:d}".format(self.nobs))
    # subclasses for which type of splitting to perform
    DiagType = self.get_geovaltype()
    return DiagType(OutDir,clobber)

##########################################################################
  def get_geovaltype(self):
    if self.obstype.split("_")[-1] in conv_vars:
      return self._geovals_conv 
    else:
      # we will assume this is a satellite radiance
      return self._geovals_rad

##########################################################################
  def _geovals_conv(self,OutDir,clobber=True):
    print(" placeholder")
##########################################################################
  def _geovals_rad(self,OutDir,clobber=True):
    print(" placeholder")
##########################################################################
def write_split_file(ncin,ncout,nobs,flag):
  """Copies variable from GSI ncdiag (ncin) file into ncdiag (ncout)
     nobs: number of unique obs to copy,
     flag: which obs to copy from the original file"""
  import netCDF4 as nc
  import numpy as np
  din = nc.Dataset(ncin, 'r')
  print("Writing: "+ncout)
  print("nobs={:d}".format(nobs))
  dout = nc.Dataset(ncout, 'w')
  # Copy attributes
  for aname in din.ncattrs():
    avalue = din.getncattr(aname)
    dout.setncattr(aname, avalue)

  # Copy dimensions
  for dim in din.dimensions.values():
    if (dim.name == "nobs"):
      d_size = nobs
    else:
      d_size = len(dim)
    dout.createDimension(dim.name, d_size)

  # Copy vars, trim them down to the selected obs denoted in
  # arugument 'flag'.
  for var in din.variables.values():
    vname = var.name
    print("  Variable: "+vname)
    vdata = var[...].data
    dims = tuple([len(din.dimensions[d]) for d in var.dimensions])
    vdata = np.frombuffer(vdata,dtype=var.dtype)
    vdata = np.reshape(vdata,dims)
    var_out = dout.createVariable(vname, var.dtype, var.dimensions)
    var_out[...] = vdata[flag,...]
  dout.close()
  din.close()

