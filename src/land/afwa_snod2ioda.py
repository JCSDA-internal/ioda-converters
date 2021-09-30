#!/usr/bin/env python3
#
# (C) Copyright 2021 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import pygrib
import time, os, sys
import argparse
import netCDF4 as nc
import numpy as np
import pyproj
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
    'snow_depth': 'snowDepth',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

os.environ["TZ"] = "UTC"


class AFWA(object):

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
        for iodavar in ['snowDepth']:
            self.varDict[iodavar]['valKey'] = iodavar, self.writer.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, self.writer.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, self.writer.OqcName()
            self.units[iodavar] = 'm'
        data = pygrib.open(self.filename)
        lat, lon = data[1].latlons()
        lons = lon[:].ravel()
        lats = lat[:].ravel()
        vals = data[1].values[:].ravel()

        # use stereographic projection to calculate lat/lon as read of
        # lat/lon is not correct for afwa grib1 file
        lat1 = data[1]['latitudeOfFirstGridPointInDegrees']
        lon1 = data[1]['longitudeOfFirstGridPointInDegrees']
        nx = data[1]['Nx']
        ny = data[1]['Ny']
        dx = data[1]['DxInMetres']
        dy = data[1]['DyInMetres']
        # this works for corner assumption to get symmetric data
        dxfac = 1.000376522
        dx = dxfac*dx
        dy = dxfac*dy

        myparams = data[1].projparams
        # reset Lat of True Origin(lat_ts)for Soutern Hemisphere grib file
        if myparams['lat_0'] == -90.0:
            myparams['lat_ts'] = -60.0

        pj = pyproj.Proj(myparams)
        llcrnrx, llcrnry = pj(lon1, lat1)
        x = llcrnrx - dx*np.arange(nx)
        y = llcrnry + dy*np.arange(ny)
        x, y = np.meshgrid(x, y)
        lon, lat = pj(x, y, inverse=True)
        lons = lon[:].ravel()
        lats = lat[:].ravel()
        # setup snowCover and mask_flag
        errs = 0.0*vals.astype('float32')
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
        data.close()
        AttrData['date_time_string'] = base_datetime

        for i in range(len(lons)):
            times[i] = base_datetime
        self.loc_mdata['datetime'] = self.writer.FillNcVector(times, "datetime")
        self.loc_mdata['latitude'] = lats
        self.loc_mdata['longitude'] = lons
        for iodavar in ['snowDepth']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg
        self.writer._nvars = len(obsvars)
        self.writer._nlocs = len(self.loc_mdata['datetime'])


def main():

    parser = argparse.ArgumentParser(
        description=('Read AFWA snow depth file(s) and Converter'
                     ' of native grib format for observations of snow'
                     ' depth to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of afwa snow depth input file(s)",
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
    snod = AFWA(args.input, args.mask, writer)

    writer.BuildNetcdf(snod.outdata, snod.loc_mdata, snod.var_mdata, AttrData, snod.units)


if __name__ == '__main__':
    main()
