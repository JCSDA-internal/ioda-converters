#!/usr/bin/env python

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

import ioda_conv_ncio as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]


obsvars = {
    'A': "aerosol_optical_depth",
}

AttrData = {
    'converter': os.path.basename(__file__),
}


class AOD(object):
    def __init__(self, filenames, obs_time, writer):
        self.filenames = filenames
        self.obs_time = obs_time
        self.writer = writer
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.units = {}
        self._read()

    def _read(self):
        # set up variable names for IODA
        for ncvar, iodavar in obsvars.items():
            self.varDict[iodavar]['valKey'] = iodavar, self.writer.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, self.writer.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, self.writer.OqcName()

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

            obstime = self.obs_time
            AttrData['date_time'] = self.obs_time
            AttrData['observation_type'] = 'Aod'

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
            QC_flag = QC_flag.astype('int8')
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
            obs_time_2 = [datetime.fromisoformat('1993-01-01') + timedelta(seconds=x) for x in modis_time]
            obs_time = [t.strftime('%Y-%m-%dT%H:%M:%SZ') for t in obs_time_2]

            # uncertainty estimates:
            # From MODIS file (over ocean) and Levy, 2010 (over land)
            # flag = 0 (ocean) 1(land) 2(coastal)

            over_ocean = np.logical_not(land_sea_flag > 0)
            over_land = np.logical_not(land_sea_flag == 0)
            UNC = np.where(over_land, unc_land, np.add(0.05, np.multiply(0.15, aod)))

            if first:
                self.loc_mdata['latitude'] = lats
                self.loc_mdata['longitude'] = lons
                self.loc_mdata['datetime'] = self.writer.FillNcVector(obs_time, "datetime")
            else:
                self.loc_mdata['latitude'] = np.concatenate((self.loc_mdata['latitude'], lats))
                self.loc_mdata['longitude'] = np.concatenate((self.loc_mdata['longitude'], lons))
                self.loc_mdata['datetime'] = np.concatenate((self.loc_mdata['datetime'], self.writer.FillNcVector(obs_time, "datetime")))

            for ncvar, iodavar in obsvars.items():
                data = aod.astype('float32')
                err = UNC
                if first:
                    self.outdata[self.varDict[iodavar]['valKey']] = data
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
        self.writer._nvars = len(obsvars)
        self.writer._nlocs = len(self.loc_mdata['longitude'])


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
    writer = iconv.NcWriter(args.output, locationKeyList)

    # get the obs time
    obs_time = args.time

    # Read in the AOD data
    aod_class = AOD(args.input, args.time, writer)
    # write everything out
    original_stdout = sys.stdout
    writer.BuildNetcdf(aod_class.outdata, aod_class.loc_mdata, aod_class.var_mdata, AttrData)


if __name__ == '__main__':
    main()
