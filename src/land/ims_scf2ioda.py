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
from orddicts import DefaultOrderedDict

vName = {
    'A': "snow_cover_fraction",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

AttrData = {}


class imsscf(object):

    def __init__(self, filename, mask, writer):
        self.filename = filename
        self.mask = mask
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.units_values = {}
        self.writer = writer
        self._read()

    def _read(self):
        data = pygrib.open(self.filename)
        lat, lon = data[1].latlons()
        lons = lon[:].ravel()
        lats = lat[:].ravel()
        vals = data.select(name='Snow cover')[0].values[:].ravel()
        # defined errors and qc
        errs = 0.08*vals

        if self.mask == "maskout":
            mask = np.logical_not(vals.mask)
            vals = vals[mask]
            lons = lons[mask]
            lats = lats[mask]
            errs = errs[mask]
        # get global attributes
        start_datetime = data[1].analDate
        base_datetime = start_datetime.isoformat() + "Z"
        # grbs.close()

        self.units_values[vName['A']] = '%'
        valKey = vName['A'], self.writer.OvalName()
        errKey = vName['A'], self.writer.OerrName()
        qcKey = vName['A'], self.writer.OqcName()

        for i in range(len(lons)):

            locKey = lats[i], lons[i], base_datetime
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = errs[i]
            self.data[0][locKey][qcKey] = 0

            # write global attributes out
            self.satellite = "POES/GOES"
            self.sensor = "IMS.Multisensor"

            AttrData["observation_type"] = "SCF"
            AttrData["satellite"] = self.satellite
            AttrData["sensor"] = self.sensor
            AttrData['date_time_string'] = base_datetime


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

    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(scf.data)

    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData, scf.units_values)


if __name__ == '__main__':
    main()
