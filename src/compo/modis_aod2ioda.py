#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, date, timedelta
import os
from pathlib import Path
from pyhdf.SD import SD, SDC

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

obsvars = ["aerosolOpticalDepth"]

# A dictionary of global attributes.  More filled in further down.
AttrData = {
    'converter': os.path.basename(__file__),
    'description': 'AOD at 550nm'
}

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {'aerosolOpticalDepth': ['Location', 'Channel']}
channels = [4]

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

long_missing_value = nc.default_fillvals['i8']


class AOD(object):
    def __init__(self, filenames, obs_time, pltfrm):
        self.filenames = filenames
        self.obs_time = obs_time
        self.pltfrm = pltfrm
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        # there's absolutely no difference in the hdf4 files attributes
        # between Terra and Aqua files. So it is user specified
        AttrData['platform'] = pltfrm
        # sensor would be always MODIS for this converter
        AttrData['sensor'] = 'MODIS'
        AttrData['datetimeReference'] = obs_time
        self._read()

    def _read(self):
        # set up variable names for IODA
        for iodavar in obsvars:
            self.varDict[iodavar]['valKey'] = iodavar, obsValName
            self.varDict[iodavar]['errKey'] = iodavar, obsErrName
            self.varDict[iodavar]['qcKey'] = iodavar, qcName
            self.varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, qcName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, obsValName]['_FillValue'] = -9999.
            self.varAttrs[iodavar, obsErrName]['_FillValue'] = -9999.
            self.varAttrs[iodavar, qcName]['_FillValue'] = -9999
            self.varAttrs[iodavar, obsValName]['units'] = '1'
            self.varAttrs[iodavar, obsErrName]['units'] = '1'

        # All of MODIS AOD data have a singular reference time
        self.varAttrs[('dateTime', metaDataName)]['units'] = 'seconds since 1993-01-01T00:00:00Z'

        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=np.int64)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        # loop through input filenames
        for f in self.filenames:
            hdf = SD(f, SDC.READ)

            #  Get variables
            modis_time = hdf.select('Scan_Start_Time')[:].ravel()
            print(f"length of time var: {len(modis_time)}")
            modis_time = modis_time.astype('float32')
            lats = hdf.select('Latitude')[:].ravel()
            lats = lats.astype('float32')
            lons = hdf.select('Longitude')[:].ravel()
            lons = lons.astype('float32')
            aod = hdf.select('AOD_550_Dark_Target_Deep_Blue_Combined')[:].ravel()
            aod = aod.astype('float64')
            land_sea_flag = hdf.select('Land_sea_Flag')[:].ravel()
            QC_flag = hdf.select('Land_Ocean_Quality_Flag')[:].ravel()
            QC_flag = QC_flag.astype('int32')
            sol_zen = hdf.select('Solar_Zenith')[:].ravel()
            sen_zen = hdf.select('Sensor_Zenith')[:].ravel()
            unc_land = hdf.select('Deep_Blue_Aerosol_Optical_Depth_550_Land_Estimated_Uncertainty')[:].ravel()

            # Remove undefined values
            pos_index = np.where(aod > 0)
            lats = lats[pos_index]
            lons = lons[pos_index]
            aod = aod[pos_index] * 1E-3  # see scale factor
            land_sea_flag = land_sea_flag[pos_index]
            QC_flag = QC_flag[pos_index]
            sol_zen = sol_zen[pos_index]
            sen_zen = sen_zen[pos_index]
            unc_land = unc_land[pos_index] * 1E-3  # see scale factor
            modis_time = modis_time[pos_index]
            obs_time = np.full(len(modis_time), long_missing_value, dtype=np.int64)
            for n, t in enumerate(modis_time):
                obs_time[n] = round(t)

            # uncertainty estimates:
            # From MODIS file (over ocean) and Levy, 2010 (over land)
            # flag = 0 (ocean) 1(land) 2(coastal)

            over_ocean = np.logical_not(land_sea_flag > 0)
            over_land = np.logical_not(land_sea_flag == 0)
            UNC = np.where(over_land, unc_land, np.add(0.05, np.multiply(0.15, aod)))

            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)], np.array(lats, dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)], np.array(lons, dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)], np.array(obs_time, dtype=np.int64))

            for iodavar in obsvars:
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(self.outdata[self.varDict[iodavar]['valKey']], np.array(aod, dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(self.outdata[self.varDict[iodavar]['errKey']], np.array(UNC, dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(self.outdata[self.varDict[iodavar]['qcKey']], np.array(QC_flag, dtype=np.int32))

        DimDict['Location'] = len(self.outdata[('dateTime', metaDataName)])
        DimDict['Channel'] = np.array(channels)


def main():

    # get command line arguments
    # Usage: python blah.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... -p <Terra or Aqua> -t Analysis_time /path/to/obs/2021060823.nc
    # -o /path/to/ioda/20210608.nc
    # where the input obs could be for any desired interval to concatenated together. Analysis time is generally the midpoint of
    # analysis window.
    parser = argparse.ArgumentParser(
        description=(
            'Reads MODIS AOD hdf4 files provided by NASA'
            ' and converts into IODA formatted output files. Multiple'
            ' files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of MODIS AOD hdf4 input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-t', '--time',
        help="Observation time in global attributes (YYYYMMDDHH)",
        type=int, required=True)
    required.add_argument(
        '-p', '--platform',
        help="AQUA or TERRA satellite?",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()

    # setup the IODA writer

    # get the obs time
    obs_time = args.time

    # Read in the AOD data
    aod_class = AOD(args.input, args.time, args.platform)
    # write everything out
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(aod_class.outdata, VarDims, aod_class.varAttrs, AttrData)


if __name__ == '__main__':
    main()
