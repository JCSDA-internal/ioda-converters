#!/usr/bin/env python3

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

IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict


class Observation(object):

    def __init__(self, filename, thin, date):
        self.filename = filename
        self.thin = thin
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        print(" Reading file: ", self.filename)
        ncd = nc.Dataset(self.filename)
        datein = ncd.variables['time'][:] + 0.5
        reftime = dateutil.parser.parse(ncd.variables['time'].units[-20:])
        obs_date = reftime + timedelta(days=float(datein))

        data_in = {}
        input_vars = (
            'stdev_of_cdr_seaice_conc',
            'cdr_seaice_conc',
            'longitude',
            'latitude')

        # apply mask to constrain the ice concentration to within 1.0 (100% coverage)
        data_in['cdr_seaice_conc'] = ncd.variables['cdr_seaice_conc'][:].ravel()
        mask = data_in['cdr_seaice_conc'] <= 1.0
        for v in input_vars:
            data_in[v] = ncd.variables[v][:].ravel()[mask]
        mask = mask[mask]
        vals_FillValue = ncd.variables['cdr_seaice_conc']._FillValue
        vals_units = ncd.variables['cdr_seaice_conc'].units
        errs_FillValue = ncd.variables['cdr_seaice_conc']._FillValue
        errs_units = ncd.variables['cdr_seaice_conc'].units
        # also, sometimes certain input variables have their own mask due to
        # missing values
        for v in input_vars:
            if np.ma.is_masked(data_in[v]):
                mask = np.logical_and(mask, np.logical_not(data_in[v].mask))

        for v in input_vars:
            data_in[v] = data_in[v][mask]

        ncd.close()

        vals = data_in['cdr_seaice_conc']
        errs = data_in['stdev_of_cdr_seaice_conc']
        lons = data_in['longitude']
        lats = data_in['latitude']

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

        for i in range(len(lons)):
            locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[locKey][valKey] = vals[i]
            self.VarAttrs[locKey][valKey]['_FillValue'] = vals_FillValue
            self.VarAttrs[locKey][valKey]['units'] = vals_units
            self.data[locKey][errKey] = errs[i]
            if errs[i] == 0:
                self.data[locKey][errKey] = 0.0001
            self.VarAttrs[locKey][errKey]['_FillValue'] = errs_FillValue
            self.VarAttrs[locKey][errKey]['units'] = errs_units
            self.data[locKey][qcKey] = 1
            self.VarAttrs[locKey][qcKey]['units'] = 'unitless'


vName = "sea_ice_area_fraction"


locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

GlobalAttrs = {
    'odb_version': 1,
}


def main():

    parser = argparse.ArgumentParser(
        description=('Read sea-ice concentration from NSIDC L4 CDR files'
                     '  and convert to IODA format')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="EMC ice fraction obs input file(s)",
        type=str, required=True)
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
        vName: ['nlocs'],
    }
    # Read in
    ice = Observation(args.input, args.thin, fdate)

    # write them out
    ObsVars, nlocs = iconv.ExtractObsData(ice.data, locationKeyList)
    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    writer.BuildIoda(ObsVars, VarDims, ice.VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
