#!/usr/bin/env python3
#
# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import pygrib
import time, os, sys
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
    'snow_cover_fraction': 'snowCover',
}

AttrData = {
    'converter': os.path.basename(__file__),
}


class imsscf(object):

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
        for iodavar in ['snowCover']:
            self.varDict[iodavar]['valKey'] = iodavar, self.writer.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, self.writer.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, self.writer.OqcName()
            self.units[iodavar] = '%'
        # read self.filename to get data
        data = pygrib.open(self.filename)
        lat, lon = data[1].latlons()
        lons = lon[:].ravel()
        lats = lat[:].ravel()
        vals = data.select(name='Snow cover')[0].values[:].ravel()
        # defined errors and qc
        errs = 0.08*vals
        qflg = 0*vals.astype('int32')
        times = np.empty_like(vals, dtype=object)

        if self.mask == "maskout":
            mask = np.logical_not(vals.mask)
            vals = vals[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            lons = lons[mask]
            lats = lats[mask]
            times = times[mask]
        # get global attributes
        start_datetime = data[1].analDate
        base_datetime = start_datetime.isoformat() + "Z"
        # grbs.close()

        # write global attributes out
        self.satellite = "POES/GOES"
        self.sensor = "IMS.Multisensor"
        AttrData["observation_type"] = "Snow Cover Fraction"
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor
        AttrData['date_time_string'] = base_datetime

        for i in range(len(lons)):
            times[i] = base_datetime
        self.loc_mdata['datetime'] = self.writer.FillNcVector(times, "datetime")
        self.loc_mdata['latitude'] = lats
        self.loc_mdata['longitude'] = lons
        for iodavar in ['snowCover']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg
        self.writer._nvars = len(obsvars)
        self.writer._nlocs = len(self.loc_mdata['datetime'])


def main():

    parser = argparse.ArgumentParser(
        description=('Read IMS snow cover fraction file(s) and Converter'
                     ' of native grib2 format for observations of snow'
                     ' cover fraction to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of ims snow cover input file(s)",
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
    scf = imsscf(args.input, args.mask, writer)

    # write the data out
    writer.BuildNetcdf(scf.outdata, scf.loc_mdata, scf.var_mdata, AttrData, scf.units)


if __name__ == '__main__':
    main()
