#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import argparse
import netCDF4 as nc
from datetime import datetime
import numpy as np

from lib_python.orddicts import DefaultOrderedDict
import lib_python.ioda_conv_engines as iconv


class Observation(object):

    def __init__(self, filename, thin, date):

        self.filename = filename
        self.thin = thin
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read(date)

    def _read(self, date):
        ncd = nc.MFDataset(self.filename)
        datein = ncd.variables['dtg_yyyymmdd'][:]
        timein = ncd.variables['dtg_hhmm'][:]
        lons = ncd.variables['longitude'][:]
        lats = ncd.variables['latitude'][:]
        vals = ncd.variables['ice_concentration'][:]
        qc = ncd.variables['quality'][:]
        ncd.close()

        valKey = vName, iconv.OvalName()
        errKey = vName, iconv.OerrName()
        qcKey = vName, iconv.OqcName()

        # apply thinning mask
        if self.thin > 0.0:
            mask_thin = np.random.uniform(size=len(lons)) > self.thin
            datein = datein[mask_thin]
            timein = timein[mask_thin]
            lons = lons[mask_thin]
            lats = lats[mask_thin]
            vals = vals[mask_thin]
            qc = qc[mask_thin]

        date2 = int(date.strftime("%Y%m%d"))
        for i in range(len(lons)):
            if datein[i] == date2:
                obs_date = datetime.combine(
                    datetime.strptime(
                        np.array2string(
                            datein[i]), "%Y%m%d"), datetime.strptime(
                        np.array2string(
                            timein[i]).zfill(4), "%H%M").time())
                locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
                self.data[locKey][valKey] = vals[i]
                self.data[locKey][errKey] = 0.1
                self.data[locKey][qcKey] = qc[i]


vName = "seaIceFraction"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "string")
]

GlobalAttrs = {
    'odb_version': 1,
}


def main():

    parser = argparse.ArgumentParser(
        description=('')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="EMC sea ice fraction obs input file(s)",
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
    VarDims = {
        vName: ['Location'],
    }
    # Read in
    ice = Observation(args.input, args.thin, fdate)

    # write them out
    ObsVars, Location = iconv.ExtractObsData(ice.data, locationKeyList)
    DimDict = {'Location': Location}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[vName, iconv.OvalName()]['units'] = '1'
    VarAttrs[vName, iconv.OerrName()]['units'] = '1'

    writer.BuildIoda(ObsVars, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
