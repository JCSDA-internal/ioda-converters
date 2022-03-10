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
    ("dateTime", "string")
]


obsvars = {
    'A': "aerosolOpticalDepth",
}

AttrData = {
}


DimDict = {
}

VarDims = {
    'aerosolOpticalDepth': ['Location', 'Channel']
}


class AOD(object):
    def __init__(self, filenames, obs_time):
        self.filenames = filenames
        self.obs_time = obs_time
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        # set up variable names for IODA
        for ncvar, iodavar in obsvars.items():
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = -9999.
            self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = -9999.
            self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = -9999
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = '1'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = '1'
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = '1'

        # loop through input filenames
        first = True
        for f in self.filenames:
            ncd = nc.Dataset(f, 'r')
            ncd_str = str(ncd)
            if 'Terra' in ncd_str:
                AttrData['platform'] = 'Terra'
            elif 'Aqua' in ncd_str:
                AttrData['platform'] = 'Aqua'
            if 'MODIS' in ncd_str:
                AttrData['sensor'] = 'v.modis_terra'

            AttrData['ioda_object_type'] = 'AOD@550nm'

            #  Get variables
            modis_time = ncd.variables['Scan_Start_Time'][:].ravel()

            # convert time to date_string
            lats = ncd.variables['Latitude'][:].ravel()
            lats = lats.astype('float32')
            lons = ncd.variables['Longitude'][:].ravel()
            lons = lons.astype('float32')
            aod = ncd.variables['AOD_550_Dark_Target_Deep_Blue_Combined'][:].ravel()
            land_sea_flag = ncd.variables['Land_sea_Flag'][:].ravel()
            QC_flag = ncd.variables['Land_Ocean_Quality_Flag'][:].ravel()
            QC_flag = QC_flag.astype('int32')
            sol_zen = ncd.variables['Solar_Zenith'][:].ravel()
            sen_zen = ncd.variables['Sensor_Zenith'][:].ravel()
            unc_land = ncd.variables['Deep_Blue_Aerosol_Optical_Depth_550_Land_Estimated_Uncertainty'][:].ravel()

            # Remove undefined values
            pos_index = np.where(aod > 0)
            lats = lats[pos_index]
            lons = lons[pos_index]
            aod = aod[pos_index]
            land_sea_flag = land_sea_flag[pos_index]
            QC_flag = QC_flag[pos_index]
            sol_zen = sol_zen[pos_index]
            sen_zen = sen_zen[pos_index]
            unc_land = unc_land[pos_index]
            modis_time = modis_time[pos_index]
            obs_time = np.empty_like(QC_flag, dtype=object)
            obs_time_2 = [datetime.fromisoformat('1993-01-01') + timedelta(seconds=x) for x in modis_time]
            for t in range(len(obs_time_2)):
                obs_time[t] = obs_time_2[t].strftime('%Y-%m-%dT%H:%M:%SZ')

            # uncertainty estimates:
            # From MODIS file (over ocean) and Levy, 2010 (over land)
            # flag = 0 (ocean) 1(land) 2(coastal)

            over_ocean = np.logical_not(land_sea_flag > 0)
            over_land = np.logical_not(land_sea_flag == 0)
            UNC = np.where(over_land, unc_land, np.add(0.05, np.multiply(0.15, aod)))
            if first:
                self.outdata[('latitude', 'MetaData')] = lats
                self.outdata[('longitude', 'MetaData')] = lons
                self.outdata[('dateTime', 'MetaData')] = obs_time
            else:
                self.outdata[('latitude', 'MetaData')] = np.concatenate((self.outdata[('latitude', 'MetaData')], lats))
                self.outdata[('longitude', 'MetaData')] = np.concatenate((self.outdata[('longitude', 'MetaData')], lons))
                self.outdata[('dateTime', 'MetaData')] = np.concatenate((self.outdata[('dateTime', 'MetaData')], obs_time))

            for ncvar, iodavar in obsvars.items():
                data = aod.astype('float32')
                err = UNC.astype('float32')
                data2d = np.array(data).reshape(len(data), 1)
                err2d = np.array(err).reshape(len(err), 1)
                qc2d = np.array(QC_flag).reshape(len(QC_flag), 1)
                if first:
                    self.outdata[self.varDict[iodavar]['valKey']] = data2d
                    self.outdata[self.varDict[iodavar]['errKey']] = err
                    self.outdata[self.varDict[iodavar]['qcKey']] = QC_flag

                else:
                    self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['valKey']], data))
                    self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['errKey']], err))
                    self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['qcKey']], QC_flag))

            first = False
        self.varAttrs[('dateTime', 'MetaData')]['units'] = ''
        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])
        DimDict['Channel'] = np.array([2], dtype=np.intc)


def main():

    # get command line arguments
    # Usage: python blah.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... -t Analysis_time /path/to/obs/2021060823.nc
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
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()

    # setup the IODA writer

    # get the obs time
    obs_time = args.time

    # Read in the AOD data
    aod_class = AOD(args.input, args.time)
    # write everything out
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(aod_class.outdata, VarDims, aod_class.varAttrs, AttrData)


if __name__ == '__main__':
    main()
