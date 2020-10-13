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
import numpy as np
import numpy.matlib
sys.path.append("@SCRIPT_LIB_PATH@")
import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

vName = "sea_water_salinity"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("depth", "float"),
    ("datetime", "string")
]

AttrData = {
    'odb_version': 1,
}

class Profile(object):

    def __init__(self, filename, date, writer):
        self.filename = filename
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.writer = writer
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        dpth = (ncd.variables['ctd_depth'][:])
        lons = np.float32(ncd.variables['longitude'][:])
        lats = np.float32(ncd.variables['latitude'][:])
        hrs = np.float32(np.linspace(1,23,58))
        vals = np.float32(ncd.variables['salinity'][:])
        errs = np.float32(np.matlib.repmat(2.258986,len(lons) , 1)) 
        qcs = ncd.variables['salinity_qc'][:]-1 # to unify the QC
        errs = np.squeeze(errs)
        ncd.close()

        base_date = datetime(2018,4,15) #example SG547_0001_dn_AOML.nc
        for i in range(len(hrs)):
            if qcs[i] != 0:
                continue
            valKey = vName, self.writer.OvalName()
            errKey = vName, self.writer.OerrName()
            qcKey = vName, self.writer.OqcName()
            dt = base_date + timedelta(hours=float(hrs[i]))
            locKey = lats[i], lons[i], dpth[i], dt.strftime(
                "%Y-%m-%dT%H:%M:%SZ")
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = errs[i]
            self.data[0][locKey][qcKey] = qcs[i]


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read insitu T/S profile observation file(s) that have already'
            ' been QCd and thinned for use in Hybrid-GODAS system.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of HGODAS observation input file(s)",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the profiles
    prof = Profile(args.input, fdate, writer)
    print('prof',prof)
    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")
#    print('prof.data',prof.data)
    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(prof.data)
    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData)


if __name__ == '__main__':
    main()
