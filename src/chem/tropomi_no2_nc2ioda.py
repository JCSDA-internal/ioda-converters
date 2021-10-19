#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import os
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string"),
]

obsvars = {
    'nitrogendioxide_tropospheric_column': 'nitrogen_dioxide_in_tropospheric_column',
    'nitrogendioxide_total_column': 'nitrogen_dioxide_in_total_column',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}

DimDict = {
}

VarDims = {
    'nitrogen_dioxide_in_tropospheric_column': ['nlocs'],
    'nitrogen_dioxide_in_total_column': ['nlocs'],
}


class tropomi(object):
    def __init__(self, filenames):
        self.filenames = filenames
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        for iodavar in ['nitrogen_dioxide_in_tropospheric_column', 'nitrogen_dioxide_in_total_column']:
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
            # get global attributes
            AttrData['date_time_string'] = ncd.getncattr('time_reference')[0:19]+'Z'
            AttrData['sensor'] = ncd.getncattr('sensor')
            AttrData['platform'] = ncd.getncattr('platform')
            # many variables are time, scanline, ground_pixel
            # but others are just time, scanline
            lats = ncd.groups['PRODUCT'].variables['latitude'][:].ravel()
            lons = ncd.groups['PRODUCT'].variables['longitude'][:].ravel()
            qa_value = ncd.groups['PRODUCT'].variables['qa_value'][:]  # 2D
            times = np.empty_like(qa_value, dtype=object)
            qa_value = qa_value.ravel()
            qc_flag = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS']\
                .variables['processing_quality_flags'][:]
            qc_flag = qc_flag.ravel().astype('int32')
            time1 = ncd.groups['PRODUCT'].variables['time_utc'][:]
            for t in range(len(time1[0])):
                times[0, t, :] = time1[0, t][0:19]+'Z'
            times = times.ravel()
            # need additional variable to use the averaging kernel for DA
            kernel_err = ncd.groups['PRODUCT'].\
                variables['nitrogendioxide_tropospheric_column_precision_kernel'][:].ravel()
            kernel_err_total = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS'].\
                variables['nitrogendioxide_total_column_precision_kernel'][:].ravel()
            trop_layer = ncd.groups['PRODUCT'].variables['tm5_tropopause_layer_index'][:].ravel()
            total_airmass = ncd.groups['PRODUCT'].variables['air_mass_factor_total'][:].ravel()
            trop_airmass = ncd.groups['PRODUCT'].\
                variables['air_mass_factor_troposphere'][:].ravel()
            # grab the averaging kernel
            avg_kernel = ncd.groups['PRODUCT'].variables['averaging_kernel'][:]
            nlevs = len(avg_kernel[0, 0, 0])
            AttrData['averaging_kernel_levels'] = np.int32(nlevs)
            if first:
                # add metadata variables
                self.outdata[('datetime', 'MetaData')] = times
                self.outdata[('latitude', 'MetaData')] = lats
                self.outdata[('longitude', 'MetaData')] = lons
                self.outdata[('quality_assurance_value', 'MetaData')] = qa_value
                self.outdata[('troposphere_layer_index', 'MetaData')] = trop_layer
                self.outdata[('air_mass_factor_total', 'MetaData')] = total_airmass
                self.outdata[('air_mass_factor_troposphere', 'MetaData')] = trop_airmass
                self.outdata[('tropospheric_averaging_kernel_precision', 'MetaData')] = kernel_err
                self.outdata[('averaging_kernel_precision', 'MetaData')] = kernel_err_total
                for k in range(nlevs):
                    varname = ('averaging_kernel_level_'+str(k+1), 'MetaData')
                    self.outdata[varname] = avg_kernel[..., k].ravel()
            else:
                self.outdata[('datetime', 'MetaData')] = np.concatenate((
                    self.outdata[('datetime', 'MetaData')], times))
                self.outdata[('latitude', 'MetaData')] = np.concatenate((
                    self.outdata[('latitude', 'MetaData')], lats))
                self.outdata[('longitude', 'MetaData')] = np.concatenate((
                    self.outdata[('longitude', 'MetaData')], lons))
                self.outdata[('quality_assurance_value', 'MetaData')] = np.concatenate((
                    self.outdata[('quality_assurance_value', 'MetaData')], qa_value))
                self.outdata[('troposphere_layer_index', 'MetaData')] = np.concatenate((
                    self.outdata[('troposphere_layer_index', 'MetaData')], trop_layer))
                self.outdata[('air_mass_factor_total', 'MetaData')] = np.concatenate((
                    self.outdata[('air_mass_factor_total', 'MetaData')], total_airmass))
                self.outdata[('air_mass_factor_troposphere', 'MetaData')] = np.concatenate((
                    self.outdata[('air_mass_factor_troposphere', 'MetaData')], trop_airmass))
                self.outdata[('tropospheric_averaging_kernel_precision', 'MetaData')] = np.concatenate((
                    self.outdata[('tropospheric_averaging_kernel_precision', 'MetaData')], kernel_err))
                self.outdata[('averaging_kernel_precision', 'MetaData')] = np.concatenate((
                    self.outdata[('averaging_kernel_precision', 'MetaData')], kernel_err_total))
                for k in range(nlevs):
                    varname = ('averaging_kernel_level_'+str(k+1), 'MetaData')
                    self.loc_mdata[varname] = np.concatenate((self.loc_mdata[varname],
                                                              avg_kernel[..., k].ravel()))
            for ncvar, iodavar in obsvars.items():
                if ncvar in ['nitrogendioxide_tropospheric_column']:
                    data = ncd.groups['PRODUCT'].variables[ncvar][:].ravel()
                    err = ncd.groups['PRODUCT'].variables[ncvar+'_precision'][:].ravel()
                else:
                    data = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS'].variables[ncvar][:].ravel()
                    err = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS'].variables[ncvar+'_precision'][:].ravel()
                if first:
                    self.outdata[self.varDict[iodavar]['valKey']] = data
                    self.outdata[self.varDict[iodavar]['errKey']] = err
                    self.outdata[self.varDict[iodavar]['qcKey']] = qc_flag
                else:
                    self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['valKey']], data))
                    self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['errKey']], err))
                    self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['qcKey']], qc_flag))
            first = False
        DimDict['nlocs'] = len(self.outdata[('datetime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])

        for k in range(nlevs):
            varname = 'averaging_kernel_level_'+str(k+1)
            vkey = (varname, 'MetaData')
            self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
            self.varAttrs[vkey]['units'] = ''


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads TROPOMI NO2 netCDF files provided by NESDIS'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of TROPOMI L2 NO2 observation netCDF input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()

    # Read in the NO2 data
    no2 = tropomi(args.input)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(no2.outdata, VarDims, no2.varAttrs, AttrData)


if __name__ == '__main__':
    main()
