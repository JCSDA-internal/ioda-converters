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
    'A': "Aerosol_optical_depth",
}

AttrData = {
    'converter': os.path.basename(__file__),
}


class AOD(object):
    def __init__(self, filenames, writer):
        self.filenames = filenames
        self.writer = writer
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.units = {}
        self._read()

    def _read(self):
        # set up variable names for IODA
        for iodavar in ['Aerosol_optical_depth']:
            self.varDict[iodavar]['valKey'] = iodavar, self.writer.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, self.writer.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, self.writer.OqcName()

        # Set up modis time

        modis_time = {}

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
                AttrData['sensor'] = 'MODIS'

            #  Get variables
            modis_time = ncd.variables['Scan_Start_Time'][:].ravel()

            # convert time to date_string
            lats = ncd.variables['Latitude'][:].ravel()
            lons = ncd.variables['Longitude'][:].ravel()
            aod = ncd.variables['AOD_550_Dark_Target_Deep_Blue_Combined'][:].ravel()
            land_sea_flag = ncd.variables['Land_sea_Flag'][:].ravel()
            QC_flag = ncd.variables['Land_Ocean_Quality_Flag'][:].ravel()
            sol_zen = ncd.variables['Solar_Zenith'][:].ravel()
            sol_zen = sol_zen
            sen_zen = ncd.variables['Sensor_Zenith'][:].ravel()
            sen_zen = sen_zen
            unc_land = ncd.variables['Deep_Blue_Aerosol_Optical_Depth_550_Land_Estimated_Uncertainty'][:].ravel()
            unc_land = unc_land

            time_1 = timedelta(seconds=1)
            obs_time = np.empty_like(QC_flag, dtype=object)
            obs_time = datetime.fromisoformat('1993-01-01') + modis_time*time_1

            # uncertainty estimates:
            # From MODIS file (over ocean) and Levy, 2010 (over land)
            # flag = 0 (ocean) 1(land) 2(coastal)

            over_ocean = np.logical_not(land_sea_flag > 0)
            over_land = np.logical_not(land_sea_flag == 0)
            UNC = np.where(over_land, unc_land, np.add(0.05, np.multiply(0.15, aod)))
            UNC_2 = UNC[0]

            if first:
                self.loc_mdata['latitude'] = lats
                self.loc_mdata['longitude'] = lons
                self.loc_mdata['datetime'] = self.writer.FillNcVector(obs_time, "datetime")
                self.loc_mdata['latitude'] = np.concatenate((self.loc_mdata['latitude'], lats[0:1]))
                self.loc_mdata['longitude'] = np.concatenate((self.loc_mdata['longitude'], lons[0:1]))
                self.loc_mdata['datetime'] = np.concatenate((self.loc_mdata['datetime'],
                                                            self.writer.FillNcVector(obs_time[0:1], "datetime")))
            else:
                self.loc_mdata['latitude'] = np.concatenate((self.loc_mdata['latitude'], lats))
                self.loc_mdata['longitude'] = np.concatenate((self.loc_mdata['longitude'], lons))
                self.loc_mdata['datetime'] = np.concatenate((self.loc_mdata['datetime'],
                                                            self.writer.FillNcVector(obs_time, "datetime")))

            for ncvar, iodavar in obsvars.items():
                data = aod.astype('float32')
                err = UNC
                err_2 = err[0]
                if first:
                    self.outdata[self.varDict[iodavar]['valKey']] = aod
                    self.outdata[self.varDict[iodavar]['errKey']] = err
                    self.outdata[self.varDict[iodavar]['qcKey']] = QC_flag
                    self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['valKey']], data[0:1]))
                    self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['errKey']], err[0:1]))
                    self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['qcKey']], QC_flag[0:1]))

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
    # Usage: python blah.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... /path/to/obs/2021060823.nc
    # where the input obs could be for any desired interval to concatenated together.
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
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()

    # setup the IODA writer
    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the AOD data
    aod_class = AOD(args.input, writer)
    # write everything out
    original_stdout = sys.stdout
    writer.BuildNetcdf(aod_class.outdata, aod_class.loc_mdata, aod_class.var_mdata, AttrData)


if __name__ == '__main__':
    main()
