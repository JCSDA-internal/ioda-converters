#!/usr/bin/env python3

#
# (C) Copyright 2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
import netCDF4 as nc
import numpy as np
import os

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

obsvars = {
    'adt': 'adt',
}

AttrData = {
    'converter': os.path.basename(__file__)
}

DimDict = {
}

VarDims = {
    'adt': ['Location'],
}


class swot_l2adt2ioda(object):
    def __init__(self, filename):
        self.filename = filename
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):
        # Open the input file and read the relevant data
        ncd = nc.Dataset(self.filename, 'r')
        self.lons = ncd.variables['longitude'][:].ravel()
        self.lats = ncd.variables['latitude'][:].ravel()
        self.geoid = ncd.variables['geoid'][:].ravel()
        self.ssha = ncd.variables['ssha_karin'][:].ravel()
        Fillvalue = ncd.variables['ssha_karin']._FillValue
        units = ncd.variables['ssha_karin'].units
        scale_factor = ncd.variables['ssha_karin'].scale_factor
        self.mssh = ncd.variables['mean_sea_surface_cnescls'][:].ravel()
        self.err = ncd.variables['ssh_karin_uncert'][:].ravel()
        err_Fillvalue = ncd.variables['ssh_karin']._FillValue
        err_units = ncd.variables['ssh_karin'].units
        err_scale_factor = ncd.variables['ssh_karin'].scale_factor
        self.qcflag = ncd.variables['ssha_karin_qual'][:].ravel()
        # get the time data, chop off milliseconds, set the units
        time_var = ncd.variables['time']
        time_units = time_var.units[:-2] + "Z"
        s = list(time_units)
        s[24] = "T"
        time_units = "".join(s)
        num_pixels = ncd.dimensions['num_pixels'].size
        self.time = np.zeros(len(time_var)*num_pixels, dtype=np.int64)
        for t in range(len(time_var)):
            for n in range(num_pixels):
                self.time[n + t*num_pixels] = np.round(time_var[t])

        ncd.close()

        # estimate adt from SSH and Geoid height
        adt = np.where(self.ssha == Fillvalue, Fillvalue, self.ssha + self.mssh - self.geoid)

        # set up variable names for IODA
        iodavar = 'absoluteDynamicTopography'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.var_mdata[iodavar, iconv.OvalName()]['units'] = units
        self.var_mdata[iodavar, iconv.OerrName()]['units'] = err_units
        self.var_mdata[iodavar, iconv.OvalName()]['_FillValue'] = Fillvalue
        self.var_mdata[iodavar, iconv.OerrName()]['_FillValue'] = err_Fillvalue
        # self.var_mdata[iodavar, iconv.OvalName()]['scaleFactor'] = scale_factor
        # self.var_mdata[iodavar, iconv.OerrName()]['scaleFactor'] = err_scale_factor

        # map swot adt to ioda data structure
        self.outdata[('dateTime', 'MetaData')] = self.time
        self.var_mdata[('dateTime', 'MetaData')]['units'] = time_units
        self.outdata[('latitude', 'MetaData')] = self.lats.astype('float32')
        self.var_mdata[('latitude', 'MetaData')]['units'] = "degrees_north"
        self.outdata[('longitude', 'MetaData')] = self.lons.astype('float32')
        self.var_mdata[('longitude', 'MetaData')]['units'] = "degrees_east"
        self.outdata[self.varDict[iodavar]['valKey']] = adt
        # The current uncertainity values seem to be wrong so setting error to 1
        self.outdata[self.varDict[iodavar]['errKey']] = np.ones(np.shape(self.err))
        self.outdata[self.varDict[iodavar]['qcKey']] = self.qcflag.astype('int32')

        DimDict['Location'] = len(adt)


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads SWOT L2 SSH data, estimates the ADT '
            'and converts into IODA formatted output files.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of SWOT L2 SSH observation netCDF input file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()

    # Read in the ADT data
    adt = swot_l2adt2ioda(args.input)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(adt.outdata, VarDims, adt.var_mdata, AttrData)


if __name__ == '__main__':
    main()
