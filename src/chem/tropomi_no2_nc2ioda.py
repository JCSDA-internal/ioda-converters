#!/usr/bin/env python

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

import ioda_conv_ncio as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

obsvars = {
    'nitrogendioxide_tropospheric_column': 'nitrogen_dioxide_in_tropospheric_column',
}

AttrData = {
    'converter': os.path.basename(__file__),
}


class tropomi(object):
    def __init__(self, filenames, writer):
        self.filenames = filenames
        self.writer = writer
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        iodavar = 'nitrogen_dioxide_in_tropospheric_column'
        self.varDict[iodavar]['valKey'] = iodavar, self.writer.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, self.writer.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, self.writer.OqcName()
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
            trop_layer = ncd.groups['PRODUCT'].variables['tm5_tropopause_layer_index'][:].ravel()
            total_airmass = ncd.groups['PRODUCT'].variables['air_mass_factor_total'][:].ravel()
            trop_airmass = ncd.groups['PRODUCT'].\
                variables['air_mass_factor_troposphere'][:].ravel()
            # grab the averaging kernel
            avg_kernel = ncd.groups['PRODUCT'].variables['averaging_kernel'][:]
            nlevs = len(avg_kernel[0, 0, 0])
            AttrData['averaging_kernel_levels'] = np.int32(nlevs)
            if first:
                self.loc_mdata['datetime'] = self.writer.FillNcVector(times, "datetime")
                self.loc_mdata['latitude'] = lats
                self.loc_mdata['longitude'] = lons
                self.loc_mdata['quality_assurance_value'] = qa_value
                self.loc_mdata['troposphere_layer_index'] = trop_layer
                self.loc_mdata['air_mass_factor_total'] = total_airmass
                self.loc_mdata['air_mass_factor_troposphere'] = trop_airmass
                self.loc_mdata['tropospheric_averaging_kernel_precision'] = kernel_err
                for k in range(nlevs):
                    varname = 'averaging_kernel_level_'+str(k+1)
                    self.loc_mdata[varname] = avg_kernel[..., k].ravel()
            else:
                self.loc_mdata['datetime'] = np.concatenate((self.loc_mdata['datetime'],
                                                            self.writer.FillNcVector(times, "datetime")))
                self.loc_mdata['latitude'] = np.concatenate((self.loc_mdata['latitude'], lats))
                self.loc_mdata['longitude'] = np.concatenate((self.loc_mdata['longitude'], lons))
                self.loc_mdata['quality_assurance_value'] = np.concatenate((
                    self.loc_mdata['quality_assurance_value'], qa_value))
                self.loc_mdata['troposphere_layer_index'] = np.concatenate((
                    self.loc_mdata['troposphere_layer_index'], trop_layer))
                self.loc_mdata['air_mass_factor_total'] = np.concatenate((
                    self.loc_mdata['air_mass_factor_total'], total_airmass))
                self.loc_mdata['air_mass_factor_troposphere'] = np.concatenate((
                    self.loc_mdata['air_mass_factor_troposphere'], trop_airmass))
                self.loc_mdata['tropospheric_averaging_kernel_precision'] = np.concatenate((
                    self.loc_mdata['tropospheric_averaging_kernel_precision'], kernel_err))
                for k in range(nlevs):
                    varname = 'averaging_kernel_level_'+str(k+1)
                    self.loc_mdata[varname] = np.concatenate((self.loc_mdata[varname],
                                                             avg_kernel[..., k].ravel()))
            for ncvar, iodavar in obsvars.items():
                data = ncd.groups['PRODUCT'].variables[ncvar][:].ravel()
                err = ncd.groups['PRODUCT'].variables[ncvar+'_precision'][:].ravel()
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
        self.writer._nvars = len(obsvars)
        self.writer._nlocs = len(self.loc_mdata['datetime'])


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads TROPOMI NO2 netCDF files provided by NESDIS'
            ' and converts into IODA formatted output files. Multiple'
            ' files are able to be concatenated.')
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

    # setup the IODA writer
    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the NO2 data
    no2 = tropomi(args.input, writer)

    # write everything out
    writer.BuildNetcdf(no2.outdata, no2.loc_mdata, no2.var_mdata, AttrData)


if __name__ == '__main__':
    main()
