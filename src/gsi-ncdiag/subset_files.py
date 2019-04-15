#!/usr/bin/env python
import argparse
from multiprocessing import Pool
import glob
import numpy as np
import random
import sys
import netCDF4 as nc
import os

def subset(infile,nlocsout,suffix):
  print('Processing:',infile)
  outfile = infile[:-4]+suffix
  ncin = nc.Dataset(infile)
  ncout = nc.Dataset(outfile,'w')
  random.seed(5) # I think this has to be called before each call to random.sample to make it the same random set
  nlocsin = len(ncin.dimensions['nlocs'])
  try:
    nvars = len(ncin.dimensions['nvars'])
  except:
    nvars = 1
  if nlocsout > nlocsin:
    nlocsout = nlocsin
  flag = random.sample(np.arange(0,nlocsin),nlocsout)
  flag = sorted(flag)
  nobsout = nlocsout*nvars
  # copy global attributes
  for aname in ncin.ncattrs():
    avalue = ncin.getncattr(aname)
    ncout.setncattr(aname, avalue)
  # copy dimensions
  for dim in ncin.dimensions.values():
    if dim.name == 'nlocs':
      d_size = nlocsout
    elif dim.name == 'nobs':
      d_size = nobsout
    else:
      d_size = len(dim)
    ncout.createDimension(dim.name,d_size)
  # copy variables
  for var in ncin.variables.values():
    vname = var.name
    vdata = ncin.variables[vname]
    var_out = ncout.createVariable(vname, var.dtype, var.dimensions)
    if (var.dimensions[0] == 'nlocs'):
      var_out[...] = vdata[flag,...]
    else:
      var_out[:] = vdata[:]
  ncin.close();ncout.close()
   
  print('wrote to:',outfile)

######### main script ##############

# parse command line
ap = argparse.ArgumentParser()
ap.add_argument("-m","--medium",action='store_true',help="Subset to 100 obs")
ap.add_argument("-s","--single",action='store_true',help="Output single observation")
ap.add_argument("filedir",help="Path to files to process")
ap.add_argument("-n","--nprocs",help="Number of tasks/processors for multiprocessing")

MyArgs = ap.parse_args()

if MyArgs.nprocs:
  nprocs = int(MyArgs.nprocs)
else:
  nprocs = 1

if MyArgs.medium:
  nobs = 100
  suffix = '_m.nc4'
elif MyArgs.single:
  nobs = 1
  suffix = '_s.nc4'
else:
  print('need either -m or -s, exiting...')
  sys.exit(1)

InDir = MyArgs.filedir

infiles = glob.glob(InDir+'/*.nc4')
obspool = Pool(processes=nprocs)

for infile in infiles:
  # test
  if os.path.getsize(infile) < 10000:
    continue
  if infile[-6:] in ['_m.nc4','_s.nc4']:
    #print('skipping',infile)
    continue
  res = obspool.apply_async(subset,args=(infile,nobs,suffix))
obspool.close()
obspool.join()
