#!/usr/bin/env python

"""Subsetting conventional files"""
from netCDF4 import Dataset
import numpy as np
import random

def create_station_id(d):
  station_id_read = d['station_id@MetaData'][:]
  station_id = []
  for i in range(len(station_id_read)):
    station_id.append(station_id_read[i].tostring().decode('ascii').strip())
  return station_id

def create_dimensions(fname):
  d = Dataset(fname, 'a')
  nvars = d.getncattr('nvars')
  nlocs = d.dimensions['nlocs'].size
  d.createDimension("nvars", nvars)
  d.createDimension("nrecs", nvars)
  d.createDimension("nobs",  nlocs * nvars)
  d.close()

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
    vdata = din.variables[vname]
    var_out = dout.createVariable(vname, vdata.dtype, var.dimensions)
    var_out[...] = vdata[flag,...]
  dout.close()

########################
# MAIN
########################

date='2018041500'
files = ['aircraft_tsen', 'satwind_uv', 'scatwind_uv', 'sfc_tsen', 'sfc_uv',   \
         'sfcship_tsen', 'sfcship_uv', 'sondes_tsen', 'sondes_tv', 'sondes_uv', \
         'vadwind_uv', 'windprof_uv', 'rass_tsen']
#files = ['aircraft', 'satwind', 'scatwind', 'sfc', 'sfc_uv', 'sfcship', 'sfcship_uv', \
#         'sondes', 'sondes_tv', 'sondes_uv', 'vadwind', 'rass']
#files = ['aircraft']
for name in files:
  if ('_uv' in name or name == 'rass'):
    try:
      create_dimensions(name+'_obs_'+date+'.nc4')
    except:
      pass
  d = Dataset(name+'_obs_'+date+'.nc4', 'r')
  st = create_station_id(d)
  if (name in ['satwind_uv', 'scatwind_uv']):
  #if (name in ['satwind', 'scatwind']):
    nlocs = 100
    flag = random.sample(np.arange(0,d.dimensions['nlocs'].size), nlocs)
    print nlocs, ', locs: ', flag
  else:
    nst = 10
    stations = random.sample(np.unique(st),nst)
    flag = np.nonzero(np.in1d(st,stations))[0]
    print nst, len(flag), ', stations: ', stations
  flag1obs = np.arange(0,1)
  # create multi obs files (10 "records" or 100 locations
  print name, ' writing out ', len(flag), ' locs'
  write_to_file(d, name+"_obs_"+date+"_m.nc4", len(flag), flag)
  write_to_file(d, name+"_obs_"+date+"_s.nc4", len(flag1obs), flag1obs)
  d.close()
  d = Dataset(name+'_geoval_'+date+'.nc4', 'r')
  write_to_file(d, name+"_geoval_"+date+"_m.nc4", len(flag), flag)
  write_to_file(d, name+"_geoval_"+date+"_s.nc4", len(flag1obs), flag1obs)
  d.close()

