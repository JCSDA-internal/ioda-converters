#!/usr/bin/env python
import argparse
import os
import sys
import glob
import time
from multiprocessing import Pool
from pathlib import Path

prefix_lib_path = Path(__file__).absolute().parent.parent/'lib'
sys.path.append(str(prefix_lib_path/'pyiodaconv'))

import gsi_ncdiag as gsid


def run_conv_obs(convfile, outdir, platforms):
    print("Processing:"+str(convfile))
    startt = time.time()
    Diag = gsid.Conv(convfile)
    Diag.read()
    Diag.toIODAobs(outdir, platforms=platforms)
    Diag.close()
    print("Time (OBS) %s[%s]: %.3g sec" % (covfile, ",".join(platforms), time.time() - startt))
    return 0


def run_radiances_obs(radfile, outdir, obsbias, qcvars, testrefs):
    print("Processing run_radiances_obs:%s" % radfile)
    startt = time.time()
    Diag = gsid.Radiances(radfile)
    Diag.read()
    Diag.toIODAobs(outdir, obsbias, qcvars, testrefs)
    Diag.close()
    print("Time (OBS) %s: %.3g sec" % (radfile, time.time() - startt))
    return 0


def run_aod_obs(aodfile, outdir):
    print("Processing:"+str(aodfile))
    startt = time.time()
    Diag = gsid.AOD(aodfile)
    Diag.read()
    Diag.toIODAobs(outdir)
    print("Time (OBS) %s: %.3g sec" % (aodfile, time.time() - startt))
    return 0


def run_oz_obs(ozfile, outdir):
    print("Processing:"+str(ozfile))
    startt = time.time()
    Diag = gsid.Ozone(ozfile)
    Diag.read()
    Diag.toIODAobs(outdir)
    print("Time (OBS) %s: %.3g sec" % (ozfile, time.time() - startt))
    return 0


def run_conv_geo(convfile, outdir):
    print("Processing:"+str(convfile))
    startt = time.time()
    Diag = gsid.Conv(convfile)
    Diag.read()
    Diag.toGeovals(outdir)
    print("Time (GEO) %s: %.3g sec" % (convfile, time.time() - startt))
    return 0


def run_radiances_geo(radfile, outdir):
    print("Processing run_radiances_geo:%s" % radfile)
    startt = time.time()
    Diag = gsid.Radiances(radfile)
    Diag.read()
    Diag.toGeovals(outdir)
    print("Time (GEO) %s: %.3g sec" % (radfile, time.time() - startt))
    return 0


def run_aod_geo(aodfile, outdir):
    print("Processing:"+str(aodfile))
    startt = time.time()
    Diag = gsid.AOD(aodfile)
    Diag.read()
    Diag.toGeovals(outdir)
    print("Time (GEO) %s: %.3g sec" % (aodfile, time.time() - startt))
    return 0


def run_oz_geo(ozfile, outdir):
    print("Processing:"+str(ozfile))
    startt = time.time()
    Diag = gsid.Ozone(ozfile)
    Diag.read()
    Diag.toGeovals(outdir)
    print("Time (GEO) %s: %.3g sec" % (ozfile, time.time() - startt))
    return 0


def run_radiances_obsdiag(radfile, outdir):
    print("Processing run_radiances_obsdiag: %s" % radfile)
    startt = time.time()
    Diag = gsid.Radiances(radfile)
    Diag.read()
    Diag.toObsdiag(outdir)
    print("Time (DIAG) %s: %.3g sec" % (radfile, time.time() - startt))
    return 0


ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("-n", "--nprocs",
                help="Number of tasks/processors for multiprocessing")
ap.add_argument("input_dir", help="Path to concatenated GSI diag files")
ap.add_argument("-o", "--obs_dir",
                help="Path to directory to output observations")
ap.add_argument("-g", "--geovals_dir",
                help="Path to directory to output observations")
ap.add_argument("-d", "--obsdiag_dir",
                help="Path to directory to output observations")
ap.add_argument("-b", "--add_obsbias", default=False,
                help="Add ObsBias group to output observations")
ap.add_argument("-q", "--add_qcvars", default=False,
                help="Add QC variables to output observations")
ap.add_argument("-r", "--add_testrefs", default=False,
                help="Add TestReference group to output observations")

MyArgs = ap.parse_args()

if MyArgs.nprocs:
    nprocs = int(MyArgs.nprocs)
else:
    nprocs = 1

DiagDir = MyArgs.input_dir

print("Proc GSI Using %d processors." % nprocs)
obspool = Pool(processes=nprocs)
# process obs files
if MyArgs.obs_dir:
    ObsDir = MyArgs.obs_dir
    if not Path(ObsDir).is_dir():
        raise Exception("Obs dir: '%s' does not exist." % ObsDir)
    ObsBias = MyArgs.add_obsbias
    QCVars = MyArgs.add_qcvars
    TestRefs = MyArgs.add_testrefs
    # conventional obs first
    # get list of conv diag files
    convfiles = glob.glob(DiagDir+'/*conv*')
    for convfile in convfiles:
        splitfname = convfile.split('/')[-1].split('_')
        if 'conv' in splitfname:
            i = splitfname.index('conv')
            c = "_".join(splitfname[i:i + 2])
        try:
            for p in gsid.conv_platforms[c]:
                res = obspool.apply_async(run_conv_obs, args=(convfile, ObsDir, [p]))
        except KeyError:
            pass
    # radiances next
    radfiles = glob.glob(DiagDir+'/diag*')
    for radfile in radfiles:
        process = False
        for p in gsid.rad_sensors:
            if p in radfile:
                process = True
        if process:
            res = obspool.apply_async(run_radiances_obs, args=(radfile, ObsDir, ObsBias, QCVars, TestRefs))
    # atmospheric composition observations
    # aod first
    for radfile in radfiles:
        process = False
        for p in gsid.aod_sensors:
            if p in radfile:
                process = True
        if process:
            res = obspool.apply_async(run_aod_obs, args=(radfile, ObsDir))
    # ozone
    for radfile in radfiles:
        process = False
        for p in gsid.oz_sensors:
            if p in radfile:
                process = True
        if process:
            res = obspool.apply_async(run_oz_obs, args=(radfile, ObsDir))

# process geovals files
if MyArgs.geovals_dir:
    GeoDir = MyArgs.geovals_dir
    # conventional obs first
    # get list of conv diag files
    convfiles = glob.glob(DiagDir+'/*conv*')
    for convfile in convfiles:
        res = obspool.apply_async(run_conv_geo, args=(convfile, GeoDir))
    # radiances next
    radfiles = glob.glob(DiagDir+'/diag*')
    for radfile in radfiles:
        process = False
        for p in gsid.rad_sensors:
            if p in radfile:
                process = True
        if process:
            res = obspool.apply_async(run_radiances_geo, args=(radfile, GeoDir))
    # atmospheric composition observations
    # aod first
    for radfile in radfiles:
        process = False
        for p in gsid.aod_sensors:
            if p in radfile:
                process = True
        if process:
            res = obspool.apply_async(run_aod_geo, args=(radfile, GeoDir))
    # ozone
    for radfile in radfiles:
        process = False
        for p in gsid.oz_sensors:
            if p in radfile:
                process = True
        if process:
            res = obspool.apply_async(run_oz_geo, args=(radfile, GeoDir))

# process obsdiag files
if MyArgs.obsdiag_dir:
    ObsdiagDir = MyArgs.obsdiag_dir
    # radiances only
    radfiles = glob.glob(DiagDir+'/diag*')
    for radfile in radfiles:
        process = False
        for p in gsid.rad_sensors:
            if p in radfile:
                process = True
        if process:
            res = obspool.apply_async(run_radiances_obsdiag, args=(radfile, ObsdiagDir))

# process all in the same time, because sats with many channels are so slow...
obspool.close()
obspool.join()
