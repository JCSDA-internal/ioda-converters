#!/usr/bin/env python3
#
# (C) Copyright 2021 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import sys, os
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
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
    'soil_moisture': 'soilMoistureVolumetric',
}

AttrData = {
    'converter': os.path.basename(__file__),
}


class SMOS_L2NRT(object):

    def __init__(self, filename, mask, writer):
        self.filename = filename
        self.mask = mask
        self.writer = writer
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.units = {}
        self._read()

    def _read(self):

        # set up variable names for IODA
        for iodavar in ['soilMoistureVolumetric']:
            self.varDict[iodavar]['valKey'] = iodavar, self.writer.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, self.writer.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, self.writer.OqcName()
            self.units[iodavar] = 'm3m-3'
        # open input file name
        ncd = nc.Dataset(self.filename, 'r')
        # set and get global attributes
        AttrData["observation_type"] = "surface soil moisture"
        AttrData["satellite"] = "SMOS"
        AttrData["sensor"] = "MIRAS"

        lons = ncd.variables['longitude'][:]
        lats = ncd.variables['latitude'][:]
        vals = ncd.variables['soil_moisture'][:]
        errs = ncd.variables['soil_moisture_uncertainty'][:]
        rfip = ncd.variables['RFI_probability'][:]
        ddys = ncd.variables['days_since_01-01-2000'][:]
        secs = ncd.variables['seconds_since_midnight'][:]
        times = np.empty_like(vals, dtype=object)

        qflg = rfip.astype('int32')

        if self.mask == "maskout":
            mask = vals >= 0.0
            vals = vals[mask]
            lons = lons[mask]
            lats = lats[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            ddys = ddys[mask]
            secs = secs[mask]
            times = times[mask]
        ncd.close()

        for i in range(len(lons)):

            # defined QC flag(Kerr et al., 2016)
            if rfip[i] > 20.0:
                qflg[i] = 1
            else:
                qflg[i] = 0

            base_date = datetime(2000, 1, 1) + timedelta(days=int(ddys[i]))
            dt = base_date + timedelta(seconds=int(secs[i]))
            base_datetime = dt.strftime("%Y-%m-%dT%H:%M:%SZ")
            AttrData['date_time_string'] = base_datetime
            times[i] = base_datetime

        self.loc_mdata['datetime'] = self.writer.FillNcVector(times, "datetime")
        self.loc_mdata['latitude'] = lats
        self.loc_mdata['longitude'] = lons
        for iodavar in ['soilMoistureVolumetric']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg
        self.writer._nvars = len(obsvars)
        self.writer._nlocs = len(self.loc_mdata['datetime'])


def main():

    parser = argparse.ArgumentParser(
        description=('Read SMOS surface soil moisture file(s) and Converter'
                     ' of native NetCDF format for observations of soil'
                     ' moisture from SMOS to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of smos soil moisture input file(s)",
                        type=str, required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-m', '--mask',
        help="maskout missing values: maskout/default, default=none",
        type=str, required=True)

    args = parser.parse_args()

    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the profiles
    ssm = SMOS_L2NRT(args.input, args.mask, writer)

    # write everything out
    writer.BuildNetcdf(ssm.outdata, ssm.loc_mdata, ssm.var_mdata, AttrData, ssm.units)


if __name__ == '__main__':
    main()
