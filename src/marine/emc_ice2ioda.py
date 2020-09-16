#!/usr/bin/env python

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import argparse
import netCDF4 as nc
from datetime import datetime, timedelta
import dateutil.parser
import numpy as np
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

from orddicts import DefaultOrderedDict
import ioda_conv_ncio as iconv


class Observation(object):

    def __init__(self, filename, thin, date, writer):
        print(date)
        self.filename = filename
        self.thin = thin
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.writer = writer
        self._read()

    def _read(self):

        ncd = nc.MFDataset(self.filename)
        datein = ncd.variables['dtg_yyyymmdd'][:]
        timein = ncd.variables['dtg_hhmm'][:]
        lons = ncd.variables['longitude'][:]
        lats = ncd.variables['latitude'][:]
        vals = ncd.variables['ice_concentration'][:]
        qc = ncd.variables['quality'][:]
        ncd.close()

        valKey = vName, self.writer.OvalName()
        errKey = vName, self.writer.OerrName()
        qcKey = vName, self.writer.OqcName()

        # apply thinning mask
        if self.thin > 0.0:
            mask_thin = np.random.uniform(size=len(lons)) > self.thin
            datein = datein[mask_thin]
            timein = timein[mask_thin]
            lons = lons[mask_thin]
            lats = lats[mask_thin]
            vals = vals[mask_thin]
            qc = qc[mask_thin]

        for i in range(len(lons)):
            obs_date = datetime.combine(
                datetime.strptime(
                    np.array2string(
                        datein[i]), "%Y%m%d"), datetime.strptime(
                    np.array2string(
                        timein[i]).zfill(4), "%H%M").time())
            locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = 0.1
            self.data[0][locKey][qcKey] = qc[i]


vName = "sea_ice_area_fraction"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

AttrData = {
    'odb_version': 1,
}


def main():

    parser = argparse.ArgumentParser(
        description=('')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="EMC ice fraction obs input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="name of ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-t', '--thin',
        help="percentage of random thinning, from 0.0 to 1.0. Zero indicates"
             " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in
    ice = Observation(args.input, args.thin, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(ice.data)
    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData)


if __name__ == '__main__':
    main()
