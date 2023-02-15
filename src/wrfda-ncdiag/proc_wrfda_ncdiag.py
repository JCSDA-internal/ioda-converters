#!/usr/bin/env python3
import argparse
import os
import sys
from multiprocessing import Pool
import glob
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import wrfda_ncdiag as wrfdad


def run_radiances_obs(radfile, outdir, datesubs):
    print("Processing run_radiances_obs:")
    print("Processing:"+str(radfile))
    Diag = wrfdad.Radiances(radfile)
    Diag.read()
    Diag.toIODAobs(outdir, dateSubDirs=datesubs)
    Diag.close()
    return 0


ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("-n", "--nprocs",
                help="Number of tasks/processors for multiprocessing")
ap.add_argument("input_dir", help="Path to concatenated WRFDA diag files")
ap.add_argument("-o", "--obs_dir",
                help="Path to directory to output observations")
ap.add_argument("-d", "--date_subdirs",
                help="Whether to place output observations in date-specific subdirectories")

MyArgs = ap.parse_args()

if MyArgs.nprocs:
    nprocs = int(MyArgs.nprocs)
else:
    nprocs = 1

if MyArgs.date_subdirs:
    print(MyArgs.date_subdirs)
    print(bool(MyArgs.date_subdirs))

    datesubs = bool(MyArgs.date_subdirs)
else:
    datesubs = False

DiagDir = MyArgs.input_dir

obspool = Pool(processes=nprocs)
# process obs files
if MyArgs.obs_dir:
    ObsDir = MyArgs.obs_dir
    # radiances next
    radfiles = glob.glob(DiagDir+'/diags_*')
    for radfile in radfiles:
        process = False
        for p in wrfdad.rad_platform_sensor_combos:
            if p in radfile:
                process = True
        if process:
            res = obspool.apply_async(run_radiances_obs, args=(radfile, ObsDir, datesubs))

# process all in the same time, because sats with many channels are so slow...
obspool.close()
obspool.join()
