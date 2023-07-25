#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
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
import datetime as datim

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "string"),
]

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(1),
}

DimDict = {
}

# DU to mol.m-2 conversion factor
DU2molsqm = 4.4615E-4

class omps_nm(object):
    def __init__(self, filenames, qa_flg, obsVar):
        self.filenames = filenames
        self.qa_flg = qa_flg
        self.obsVar = obsVar
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        varname_str = list(self.obsVar.keys())[0]
        print('Processing variable: %s' % (varname_str), flush=1)
        iodavar = self.obsVar[varname_str]
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'mol m-2'
        self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mol m-2'

        # loop through input filenames
        first = True
        for f in self.filenames:
            ncd = nc.Dataset(f, 'r')
            #get dimensions
            da = ncd.dimensions['DimAlongTrack'].size
            dc = ncd.dimensions['DimCrossTrack'].size
            geo = ncd.groups['GeolocationData']
            sci = ncd.groups['ScienceData']

            #geolocation
            lat = geo.variables['Latitude'][:].ravel()
            lon = geo.variables['Longitude'][:].ravel()
            time = geo.variables['Time'][:]
            dt = np.datetime64('1993-01-01T00:00', 's') - np.datetime64('1970-01-01T00:00', 's')
            time = np.repeat(time + dt.astype('float64'), dc).astype('int64')

            # qa flag, qc value, here we'll use qa_value for the qc_flag
            # other quantities could be used for future filtering in UFO
            qa_value = sci.variables['QualityFlags'][:].ravel()
            flg = qa_value <= self.qa_flg

            # obs value, we prefer to convert DU to mol.m-2
            obs = sci.variables['ColumnAmountO3'][:].ravel() * DU2molsqm

            # for obs error, it is no provided in the
            # product. In the ATBD: https://www.star.nesdis.noaa.gov/jpss/documents/
            # ATBD/D0001-M01-S01-006_JPSS_ATBD_OMPS-TC-Ozone_C.pdf
            # using section 7.1 and figure and tables 7.1-1 and 7.1-2 we can derive
            # a linear relationship between obs value and obs error
            err = (5.84347E-3 * obs + 18.58484) * DU2molsqm

            
def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads OMPS NMTC and NP files from: '
            'https://disc.gsfc.nasa.gov/datasets/OMPS_NPP_NMTO3_L2_2/'
            'https://disc.gsfc.nasa.gov/datasets/OMPS_NPP_NPBUVO3_L2_2/'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of OMPS L2 NM O3 observations input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-q', '--qa_value',
        help="using values described in: "
        " https://snpp-omps.gesdisc.eosdis.nasa.gov/data/SNPP_OMPS_Level2/"
        "OMPS_NPP_NMTO3_L2.2/doc/README.OMPS_NPP_NMTO3_L2.2.pdf",
        type=float, default=128)

    args = parser.parse_args()

    obsVar = {'ozone_total_column': 'ozoneTotal'}
    varDims = {'ozoneTotal': ['Location']}

    # Read in the NO2 data
    var = omps_nm(args.input, args.qa_value, obsVar)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(var.outdata, varDims, var.varAttrs, AttrData)

if __name__ == '__main__':
    main()
