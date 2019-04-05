#!/usr/bin/env python
import argparse
import os
import sys
from multiprocessing import Pool, Queue, Process
import glob

import conv

def run_conv(convfile,outdir):
  print("Processing:"+str(convfile))
  Diag = conv.Conv(convfile)
  Diag.read()
  Diag.toIODAobs(outdir)
  return 0

ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("-n","--nprocs",help="Number of tasks/processors for multiprocessing")
ap.add_argument("input_dir",help="Path to concatenated GSI diag files")
ap.add_argument("-o","--obs_dir",help="Path to directory to output observations")
ap.add_argument("-g","--geovals_dir",help="Path to directory to output observations")

MyArgs = ap.parse_args()

if MyArgs.nprocs:
  nprocs = int(MyArgs.nprocs)
else:
  nprocs = 1
  
DiagDir = MyArgs.input_dir

# process obs files
if MyArgs.obs_dir:
  ObsDir=MyArgs.obs_dir
  ### conventional obs first
  # get list of conv diag files
  convfiles = glob.glob(DiagDir+'/*conv*') 
  obspool = Pool(processes=nprocs)
  for convfile in convfiles:
    res = obspool.apply_async(run_conv,args=(convfile,ObsDir))
  obspool.close()
  obspool.join()

# process geovals files
if MyArgs.geovals_dir:
  pass
