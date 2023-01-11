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
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
]
meta_keys = [m_item[0] for m_item in locationKeyList]

AttrData = {
    'converter': os.path.basename(__file__)
}

DimDict = {
}

varDims = {
}

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

float_missing_value = nc.default_fillvals['f4']


class tropomi(object):
    def __init__(self, filenames, varname, columnType, qa_flg, thin, obsVar):
        self.filenames = filenames
        self.varname = varname
        self.columnType = columnType
        self.qa_flg = qa_flg
        self.thin = thin
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
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude referenceLevel'
        self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude referenceLevel'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude referenceLevel'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'mol m-2'
        self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mol m-2'
        # loop through input filenames
        first = True
        for f in self.filenames:
            ncd = nc.Dataset(f, 'r')

            # get global attributes
            AttrData['datetimeReference'] = ncd.getncattr('time_reference')[0:19]+'Z'
            AttrData['sensor'] = ncd.getncattr('sensor')
            AttrData['platform'] = ncd.getncattr('platform')

            # many variables are time, scanline, ground_pixel
            # but others are just time, scanline
            lats = ncd.groups['PRODUCT'].variables['latitude'][:].ravel()
            lons = ncd.groups['PRODUCT'].variables['longitude'][:].ravel()
            qa_value = ncd.groups['PRODUCT'].variables['qa_value'][:]  # 2D
            time_offsets = np.empty_like(qa_value, dtype=np.int64)
            qa_value = qa_value.ravel()

            # adding ability to pre filter the data using the qa value
            # and also perform thinning using random uniform draw
            qaf = qa_value > self.qa_flg
            thi = np.random.uniform(size=len(lons)) > self.thin
            flg = np.logical_and(qaf, thi)
            qc_flag = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS']\
                .variables['processing_quality_flags'][:]
            qc_flag = qc_flag.ravel().astype('int32')
            time1 = ncd.groups['PRODUCT'].variables['time_utc'][:]
            for t in range(len(time1[0])):
                this_datetime = datetime.fromisoformat(time1[0, t][0:19])
                time_offset = round((this_datetime - epoch).total_seconds())
                time_offsets[0, t, :] = time_offset
            time_offsets = time_offsets.ravel()

            if self.varname == 'no2':
                trop_layer = ncd.groups['PRODUCT'].variables['tm5_tropopause_layer_index'][:].ravel()
                total_airmass = ncd.groups['PRODUCT'].variables['air_mass_factor_total'][:].ravel()
                trop_airmass = ncd.groups['PRODUCT'].\
                    variables['air_mass_factor_troposphere'][:].ravel()
                # get info to construct the pressure level array
                ps = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['INPUT_DATA'].\
                    variables['surface_pressure'][:]
                # bottom of layer is vertice 0, very top layer is TOA (0hPa)
                ak = ncd.groups['PRODUCT'].variables['tm5_constant_a'][:, :]
                bk = ncd.groups['PRODUCT'].variables['tm5_constant_b'][:, :]
                # grab the averaging kernel
                avg_kernel = ncd.groups['PRODUCT'].variables['averaging_kernel'][:]
            elif self.varname == 'co':
                preslv = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                    groups['DETAILED_RESULTS'].variables['pressure_levels'][:]
                # grab the averaging kernel
                avg_kernel = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                    groups['DETAILED_RESULTS'].variables['column_averaging_kernel'][:]

            nlevs = len(avg_kernel[0, 0, 0])

            # scale the avk using AMF ratio and tropopause level for tropo column
            if self.varname == 'no2':
                nlocf = len(trop_layer[flg])
            elif self.varname == 'co':
                nlocf = len(lats[flg])

            scaleAK = np.ones((nlocf, nlevs), dtype=np.float32)
            if self.columnType == 'tropo':
                # do not loop over nlocs here this makes the execution very slow
                for k in range(nlevs):
                    scaleAK[..., k][np.full((nlocf), k, dtype=int) > trop_layer[flg]] = 0
                    scaleAK[..., k] *= total_airmass[flg] / trop_airmass[flg]

            if first:
                # add metadata variables
                self.outdata[('dateTime', 'MetaData')] = time_offsets[flg]
                self.outdata[('latitude', 'MetaData')] = lats[flg]
                self.outdata[('longitude', 'MetaData')] = lons[flg]
                self.outdata[('qualityFlags', 'QualityMarker')] = qa_value[flg]
                self.outdata[('avgKernelLevel', 'RetrievalData')] = np.full((nlocf, nlevs+1), float_missing_value, dtype=np.float32)
                self.outdata[('referenceLevel', 'RetrievalData')] = np.full((nlocf, nlevs+1), float_missing_value, dtype=np.float32)
                for k in range(nlevs):
                    self.outdata[('avgKernelLevel', 'RetrievalData')][..., k] = avg_kernel[..., k].ravel()[flg] * scaleAK[..., k]
                    vname = ('referenceLevel', 'RetrievalData')
                    if self.varname == 'no2':
                        self.outdata[vname][..., k] = ak[k, 0] + bk[k, 0]*ps[...].ravel()[flg]
                    elif self.varname == 'co':
                        rev_k = nlevs-k-1
                        self.outdata[vname][..., rev_k] = preslv[..., rev_k].ravel()[flg]

                # add top vertice in IODA file, here it is 0hPa but can be different
                # for other obs stream
                if self.varname == 'no2':
                    self.outdata[vname][..., nlevs] = ak[k, 0] + bk[k, 0]*ps[...].ravel()[flg]
                elif self.varname == 'co':
                    self.outdata[vname][..., nlevs] = np.zeros(nlocf, dtype=np.float32)

            else:
                self.outdata[('dateTime', 'MetaData')] = np.concatenate((
                    self.outdata[('dateTime', 'MetaData')], time_offsets[flg]))
                self.outdata[('latitude', 'MetaData')] = np.concatenate((
                    self.outdata[('latitude', 'MetaData')], lats[flg]))
                self.outdata[('longitude', 'MetaData')] = np.concatenate((
                    self.outdata[('longitude', 'MetaData')], lons[flg]))
                self.outdata[('qualityFlags', 'QualityMarker')] = np.concatenate((
                    self.outdata[('qualityFlags', 'QualityMarker')], qa_value[flg]))
                for k in range(nlevs):
                    vname = ('avgKernelLevel', 'RetrievalData')
                    self.outdata[vname][..., k] = np.concatenate((self.outdata[vname][..., k],
                                                  avg_kernel[..., k].ravel()[flg] * scaleAK[..., k]), axis=1)
                    vname = ('referenceLevel', 'RetrievalData')
                    if self.varname == 'no2':
                        self.outdata[vname][..., k] = np.concatenate((self.outdata[vname][..., k],
                                                      ak[k, 0] + bk[k, 0]*ps[...].ravel()[flg]), axis=1)
                    elif self.varname == 'co':
                        rev_k = nlevs-k-1
                        self.outdata[vname][..., rev_k] = np.concatenate((self.outdata[vname][..., rev_k],
                                                          preslv[..., rev_k].ravel()[flg]), axis=1)

                if self.varname == 'no2':
                    self.outdata[vname][..., nlevs] = np.concatenate((self.outdata[vname][..., nlevs],
                                                      ak[nlevs-1, 1] + bk[nlevs-1, 1]*ps[...].ravel()[flg]), axis=1)
                elif self.varname == 'co':
                    self.outdata[vname][..., nlevs] = np.concatenate((self.outdata[vname][..., nlevs],
                                                      np.zeros(nlocf, dtype=np.float32)), axis=1)

            for ncvar, iodavar in self.obsVar.items():

                if ('tropospher' in ncvar and self.varname == "no2") or self.varname == "co":
                    data = ncd.groups['PRODUCT'].variables[ncvar][:].ravel()[flg]
                    err = ncd.groups['PRODUCT'].variables[ncvar+'_precision'][:].ravel()[flg]
                else:
                    data = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS'].variables[ncvar][:].ravel()[flg]
                    err = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS'].variables[ncvar+'_precision'][:].ravel()[flg]

                if first:
                    self.outdata[self.varDict[iodavar]['valKey']] = data
                    self.outdata[self.varDict[iodavar]['errKey']] = err
                    self.outdata[self.varDict[iodavar]['qcKey']] = qc_flag[flg]
                else:
                    self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['valKey']], data))
                    self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['errKey']], err))
                    self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['qcKey']], qc_flag[flg]))

            first = False

        self.varAttrs[('dateTime', 'MetaData')]['units'] = locationKeyList[meta_keys.index('dateTime')][2]

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])
        DimDict['averagingKernelLevels'] = np.int32(nlevs+1)

        # Repeat all Location values on all levels for various MetaData
        self.outdata[('dateTime', 'MetaData')] = np.tile(self.outdata[('dateTime', 'MetaData')], (nlevs+1, 1))
        self.outdata[('latitude', 'MetaData')] = np.tile(self.outdata[('latitude', 'MetaData')], (nlevs+1, 1))
        self.outdata[('longitude', 'MetaData')] = np.tile(self.outdata[('longitude', 'MetaData')], (nlevs+1, 1))
        self.outdata[('qualityFlags', 'QualityMarker')] = np.tile(self.outdata[('qualityFlags', 'QualityMarker')], (nlevs+1, 1))

        for key in list(self.outdata.keys()):
            vname = key[0]
            varDims[vname] = ['Location', 'averagingKernelLevels']


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads TROPOMI NO2/CO netCDF files: official Copernicus product'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of TROPOMI L2 NO2/CO observation netCDF input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-v', '--variable',
        help="name of varibale, available list: [no2, co]",
        type=str, required=True)
    required.add_argument(
        '-c', '--column',
        help="type of column: total or tropo",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-q', '--qa_value',
        help="qa value used to preflag data that goes into file before QC"
        "default at 0.75 (no2) as suggested in the documentation. See:"
        "https://sentinel.esa.int/documents/247904/2474726/"
        "Sentinel-5P-Level-2-Product-User-Manual-Nitrogen-Dioxide.pdf section 8.6"
        "0.5 is suggested for co. See:"
        "https://sentinel.esa.int/documents/247904/2474726/"
        "Sentinel-5P-Level-2-Product-User-Manual-Carbon-Monoxide.pdf section 8.3",
        type=float, default=0.75)
    optional.add_argument(
        '-n', '--thin',
        help="percentage of random thinning from 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    args = parser.parse_args()

    if args.variable == "no2":
        var_longname = 'nitrogendioxide'
    elif args.variable == "co":
        var_longname = 'carbonmonoxide'
        if args.column == "tropo":
            print('CO is only available for total column, reset column to total', flush=1)
            args.column = 'total'

    if args.column == "tropo":
        obsVar = {var_longname+'_tropospheric_column': var_longname+'Column'}
    elif args.column == "total":
        obsVar = {var_longname+'_total_column': var_longname+'Total'}

    # Read in the NO2 or CO data
    var = tropomi(args.input, args.variable, args.column, args.qa_value, args.thin, obsVar)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(var.outdata, varDims, var.varAttrs, AttrData)


if __name__ == '__main__':
    main()
