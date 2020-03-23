#!/usr/bin/env python
import argparse
from multiprocessing import Pool
import glob
import numpy as np
import random
import sys
import netCDF4 as nc
import os


def subset(infile, nlocsout, suffix, geofile, diagfile):
    print('Processing:', infile)
    outfile = infile[:-4]+suffix
    ncin = nc.Dataset(infile)
    ncout = nc.Dataset(outfile, 'w')
    # I think this has to be called before each call to random.sample to make it the same random set
    random.seed(5)
    # Get number of records (nrecsin)
    recnum = ncin.variables['record_number@MetaData'][:]
    recnum_uniq = np.unique(recnum)
    nrecsin = len(recnum_uniq)
    nlocsin = len(ncin.dimensions['nlocs'])
    nvars = len(ncin.dimensions['nvars'])
    if nrecsin > 100.:
        nsamples = int(nlocsout/10.)  # just picked 10 randomly
        nsamples = max(1, nsamples)
        npossible = nrecsin
        recs = True
    else:
        nsamples = nlocsout
        npossible = nlocsin
        recs = False
    if nlocsout > nlocsin:
        nsamples = npossible
    flag = random.sample(list(np.arange(0, npossible)), nsamples)
    flag = sorted(flag)
    # get the nlocs where flag is true so consistent for geovals too
    if recs:
        nrecsout = len(flag)
        flag2 = flag
        flag = np.isin(ncin.variables['record_number@MetaData'][:], flag)
        nlocsout = len(ncin.variables['record_number@MetaData'][flag, ...])
    else:
        flag2 = [0]
        nrecsout = 1
        nlocsout = len(flag)
    # process observation file
    # copy global attributes
    for aname in ncin.ncattrs():
        avalue = ncin.getncattr(aname)
        ncout.setncattr(aname, avalue)
    # redo nlocs
    ncout.setncattr("nlocs", np.int32(nlocsout))
    # copy dimensions
    for dim in ncin.dimensions.values():
        if dim.name == 'nlocs':
            d_size = nlocsout
        else:
            d_size = len(dim)
        ncout.createDimension(dim.name, d_size)
    # copy variables
    for var in ncin.variables.values():
        vname = var.name
        vdata = ncin.variables[vname]
        var_out = ncout.createVariable(vname, var.dtype, var.dimensions)
        if (var.dimensions[0] == 'nlocs'):
            var_out[...] = vdata[flag, ...]
        else:
            var_out[:] = vdata[:]
        # variable attributes
        for aname in var.ncattrs():
            avalue = var.getncattr(aname)
            var_out.setncattr_string(aname, avalue)
    ncin.close()
    ncout.close()

    print('wrote obs to:', outfile)
    # now process geoval file if necessary
    if geofile:
        outfile = geofile[:-4]+suffix
        ncin = nc.Dataset(geofile)
        ncout = nc.Dataset(outfile, 'w')
        # attributes
        for aname in ncin.ncattrs():
            avalue = ncin.getncattr(aname)
            ncout.setncattr(aname, avalue)
        # redo nlocs
        ncout.setncattr("nlocs", np.int32(nlocsout))
        # copy dimensions
        for dim in ncin.dimensions.values():
            if dim.name == 'nlocs':
                d_size = nlocsout
            else:
                d_size = len(dim)
            ncout.createDimension(dim.name, d_size)
        # copy variables
        for var in ncin.variables.values():
            vname = var.name
            vdata = ncin.variables[vname]
            var_out = ncout.createVariable(vname, var.dtype, var.dimensions)
            if (var.dimensions[0] == 'nlocs'):
                var_out[...] = vdata[flag, ...]
            else:
                var_out[:] = vdata[:]
        ncin.close()
        ncout.close()

        print('wrote geovals to:', outfile)

    # now process obsdiag file if necessary
    if diagfile:
        outfile = diagfile[:-4]+suffix
        ncin = nc.Dataset(diagfile)
        ncout = nc.Dataset(outfile, 'w')
        # attributes
        for aname in ncin.ncattrs():
            avalue = ncin.getncattr(aname)
            ncout.setncattr(aname, avalue)
        # redo nlocs
        ncout.setncattr("nlocs", np.int32(nlocsout))
        # copy dimensions
        for dim in ncin.dimensions.values():
            if dim.name == 'nlocs':
                d_size = nlocsout
            else:
                d_size = len(dim)
            ncout.createDimension(dim.name, d_size)
        # copy variables
        for var in ncin.variables.values():
            vname = var.name
            vdata = ncin.variables[vname]
            var_out = ncout.createVariable(vname, var.dtype, var.dimensions)
            if (var.dimensions[0] == 'nlocs'):
                var_out[...] = vdata[flag, ...]
            else:
                var_out[:] = vdata[:]
        ncin.close()
        ncout.close()

        print('wrote obsdiag to:', outfile)

# main script ##############


# parse command line
ap = argparse.ArgumentParser()
ap.add_argument("-m", "--medium", action='store_true',
                help="Subset to 5 records or 100 obs")
ap.add_argument("-s", "--single", action='store_true',
                help="Output single observation or record")
ap.add_argument("-g", "--geovals", help="Path to geoval directory")
ap.add_argument("-d", "--obsdiag", help="Path to obsdiag directory")
ap.add_argument("filedir", help="Path to obs files to process")
ap.add_argument("-n", "--nprocs",
                help="Number of tasks/processors for multiprocessing")

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
    if os.path.getsize(infile) < 10000:
        continue
    if infile[-6:] in ['_m.nc4', '_s.nc4']:
        # print('skipping',infile)
        continue
    # get geofile
    geofile = False
    if MyArgs.geovals:
        inob = infile.split('/')[-1]
        ingeo = inob.replace('obs', 'geoval')
        geofile = MyArgs.geovals+'/'+ingeo
    # get diagfile
    diagfile = False
    if MyArgs.obsdiag:
        inob = infile.split('/')[-1]
        indiag = inob.replace('obs', 'obsdiag')
        diagfile = MyArgs.obsdiag+'/'+indiag
    res = obspool.apply_async(subset, args=(infile, nobs, suffix, geofile, diagfile))
obspool.close()
obspool.join()
