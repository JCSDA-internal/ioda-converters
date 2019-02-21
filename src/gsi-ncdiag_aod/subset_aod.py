#!/usr/bin/env python

"""Subsetting conventional files"""
from netCDF4 import Dataset
import numpy as np
import random

def write_to_file(din, fileout, nlocs, flag):
  """Copies variable from GSI ncdiag (din) file into ncdiag (dout)
     nlocs: number of unique locs to copy,
     flag: which obs to copy from the original file"""

  dout = Dataset(fileout, 'w')
  print "    Writing: ", fileout, " nlocs=", nlocs
  # Copy attributes
  for aname in din.ncattrs():
    avalue = din.getncattr(aname)
    dout.setncattr(aname, avalue)

  # Copy dimensions
  for dim in din.dimensions.values():
    if (dim.name == "nlocs"):
      d_size = nlocs
    elif (dim.name == "nrecs"):
      d_size = nlocs
    elif (dim.name == "nobs"):
      d_size = nlocs * din.dimensions['nvars'].size
    else:
      d_size = dim.size
    dout.createDimension(dim.name, d_size)

  # Copy vars, trim them down to the selected obs denoted in
  # arugument 'flag'.
  for var in din.variables.values():
    vname = var.name
    #print "        Variable: ", vname
    vdata = var[...].data
    var_out = dout.createVariable(vname, vdata.dtype, var.dimensions)
    if (var.dimensions[0] == "nlocs"):
      var_out[...] = vdata[flag,...]
    else:
      var_out[...] = vdata[...]
  dout.close()

########################
# MAIN
########################

date='2018041500'
rads = ['v.viirs-m_npp', \
        'sndrd4_g15']
#add modis, goes-r, himawari etc.
rads = ['aod']
date='2018041500'

for name in rads:
  d = Dataset(name+'_obs_'+date+'_f.nc4', 'r')
  nlocs = 100
  flag = random.sample(np.arange(0,d.dimensions['nlocs'].size), nlocs)
  print nlocs, ', locs: ', flag
  flag1obs = np.arange(0,10)
  # create multi obs files (10 "records" or 100 locations
  print name, ' writing out ', len(flag), ' locs'
  write_to_file(d, name+"_obs_"+date+"_m.nc4", len(flag), flag)
  write_to_file(d, name+"_obs_"+date+"_s.nc4", len(flag1obs), flag1obs)
  d.close()
  d = Dataset(name+'_geoval_'+date+'_f.nc4', 'r')
  write_to_file(d, name+"_geoval_"+date+"_m.nc4", len(flag), flag)
  write_to_file(d, name+"_geoval_"+date+"_s.nc4", len(flag1obs), flag1obs)
  d.close()

