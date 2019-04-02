# conventional observations

class Conv
  """ class Conv - conventional observations
              
              Use this class to read in conventional observations
              from GSI netCDF diag files
  
  Functions:

  Attributes:
    filename    - string path to file
    validtime   - datetime object of valid observation time
    nobs        - number of observations
    obsdata     - netCDF dataset object 

"""
  def __init__(self,filename):
    self.filename = filename
    splitfname = self.filename.split('/')[-1].split('_')
    if 'conv' in splitfname:
      i = splitfname.index('conv')
      self.obstype = "_".join(splitfname[i:i+2])
    else:
      raise ValueError("Observation is not a conventional type...")

  def read(self):
    import netCDF4 as nc
    import datetime as dt
    # get valid time
    df = nc.Dataset(self.filename)
    tstr = str(df.getncattr('date_time'))
    self.validtime = dt.datetime.strptime(tstr,"%Y%m%d%H")
    # number of observations
    self.nobs = len(df['Observation_Type'][:])
    self.obsdata = df 
    
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

def grabobsidx(obsdata,platform,var):
  """ grabobsidx(obsdata,platform,var):
  obsdata  - netCDF dataset object
  platform - string of observation type: 'sondes','sfc',etc.
  var      - string of variable type: 'tsen','tv','q', etc.

  returns idx - indices of observations to write out
  """
  """
