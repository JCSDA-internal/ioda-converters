#!/usr/bin/env python3

#
# (C) Copyright 2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
import netCDF4 as nc
import numpy as np
import os
from datetime import datetime, timezone

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", iso8601_string),
    ("qualityFlag", "float", ""),
    ("solarZenithAngle", "float", ""),
    ("viewingZenithAngle", "float", ""),
    ("albedo", "float", ""),
]

varsKeyList = [('valKey', iconv.OvalName(), 'float', 'longitude latitude', 'mol m-2'),
               ('errKey', iconv.OerrName(), 'float', 'longitude latitude', 'mol m-2'),
               ('qcKey', iconv.OqcName(), 'integer', 'longitude latitude', None)]

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(1),
}

DimDict = {
}

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()

float_missing_value = iconv.get_default_fill_val(np.float32)
double_missing_value = iconv.get_default_fill_val(np.float64)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)
string_missing_value = iconv.get_default_fill_val(np.str_)

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value,
                'double': double_missing_value}

dtype_dict = {
    'string': np.str_,
    'integer': np.int32,
    'long': np.int64,
    'float': np.float32,
    'double': np.float64,
}


class tropomi(object):
    def __init__(self, filenames, varname, columnType, qa_flg, thin, date_range, obsVar):
        self.filenames = filenames
        self.varname = varname
        self.columnType = columnType
        self.qa_flg = qa_flg
        self.thin = thin
        self.wbeg = np.datetime64(str(datetime.strptime(date_range[0], "%Y%m%d%H%M"))).astype(np.int64)
        self.wend = np.datetime64(str(datetime.strptime(date_range[1], "%Y%m%d%H%M"))).astype(np.int64)
        self.obsVar = obsVar
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.setDicts()
        self._read()

    def setDicts(self):
        meta_keys = [m_item[0] for m_item in locationKeyList]
        # Set units of the MetaData variables and all _FillValues.
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            if locationKeyList[meta_keys.index(key)][2]:
                self.varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
            self.varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
            self.outdata[(key, metaDataName)] = np.empty((0), dtype=dtype_dict[dtypestr])

        var_keys = [v_item[0] for v_item in varsKeyList]
        # set up variable names for IODA
        for varname in self.obsVar.keys():
            iodavar = self.obsVar[varname]
            for key in var_keys:
                varGroupName = varsKeyList[var_keys.index(key)][1]
                dtypestr = varsKeyList[var_keys.index(key)][2]
                coord = varsKeyList[var_keys.index(key)][3]
                self.varDict[iodavar][key] = iodavar, varGroupName
                self.varAttrs[iodavar, varGroupName]['coordinates'] = coord
                self.varAttrs[iodavar, varGroupName]['_FillValue'] = missing_vals[dtypestr]
                if varsKeyList[var_keys.index(key)][4]:
                    self.varAttrs[iodavar, varGroupName]['units'] = varsKeyList[var_keys.index(key)][4]

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        varname_str = list(self.obsVar.keys())[0]
        print('Processing variable: %s' % (varname_str), flush=1)
        iodavar = self.obsVar[varname_str]
        self.outdata[self.varDict[iodavar]['valKey']] = np.empty((0), dtype=np.float32)
        self.outdata[self.varDict[iodavar]['errKey']] = np.empty((0), dtype=np.float32)
        self.outdata[self.varDict[iodavar]['qcKey']] = np.empty((0), dtype=np.int32)

        # loop through input filenames
        first = True
        for f in self.filenames:

            # Open file
            try:
                ncd = nc.Dataset(f, 'r')
            except OSError as e:
                if 'NetCDF: Unknown file format' in str(e):
                    print(f'WARNING: This is not a NetCDF file: {f}')
                    continue
                else:
                    raise e

            # get global attributes
            AttrData['date_time_string'] = ncd.getncattr('time_reference')[0:19]+'Z'
            AttrData['sensor'] = ncd.getncattr('sensor')
            AttrData['platform'] = ncd.getncattr('platform')

            # many variables are time, scanline, ground_pixel
            # but others are just time, scanline
            lats = ncd.groups['PRODUCT'].variables['latitude'][:].ravel()
            nlocs = len(lats)
            lons = ncd.groups['PRODUCT'].variables['longitude'][:].ravel()
            qa_value = ncd.groups['PRODUCT'].variables['qa_value'][:]  # 2D
            times = np.empty_like(qa_value, dtype=np.int64)
            qa_value = qa_value.ravel()
            nlevs = ncd.groups['PRODUCT'].dimensions['layer'].size

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
                tmptime = (datetime.strptime(time1[0, t][0:19]+'Z', "%Y-%m-%dT%H:%M:%SZ")).replace(tzinfo=timezone.utc)
                times[0, t, :] = round((tmptime - epoch).total_seconds())  # time1[0, t][0:19]+'Z'
            times = times.ravel()
            winmsk = ((times >= self.wbeg) & (times <= self.wend))
            flg = np.logical_and(flg, winmsk)

            if self.varname == 'no2':
                # grab the averaging kernel and reshape it
                avg_kernel = ncd.groups['PRODUCT'].variables['averaging_kernel'][:]
                avg_kernel = np.flip(np.reshape(avg_kernel, (nlocs, nlevs)), axis=1)

                if self.columnType == 'troposphere':
                    trop_layer = ncd.groups['PRODUCT'].variables['tm5_tropopause_layer_index'][:].ravel()
                    total_airmass = ncd.groups['PRODUCT'].variables['air_mass_factor_total'][:].ravel()
                    trop_airmass = ncd.groups['PRODUCT'].variables['air_mass_factor_troposphere'][:].ravel()
                    # do not loop over nlocs here this makes the execution very slow
                    for k in range(nlevs):
                        avg_kernel[..., k][np.full((nlocs), k, dtype=int) <= trop_layer] = 0
                        avg_kernel[..., k] *= total_airmass / trop_airmass

                # construct the pressure vertices array
                ps = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['INPUT_DATA'].\
                    variables['surface_pressure'][:]
                # bottom of layer is vertice 0, very top layer is TOA (0hPa)
                ak = ncd.groups['PRODUCT'].variables['tm5_constant_a'][:, :]
                bk = ncd.groups['PRODUCT'].variables['tm5_constant_b'][:, :]
                preslv = np.flip(np.transpose(ak[..., 0][:, np.newaxis] + np.outer(bk[..., 0],
                                 ps[...].ravel())), axis=1)
                top = ak[nlevs-1, 1] + bk[nlevs-1, 1]*ps[...].ravel()

                # albedo
                albedo = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                    groups['INPUT_DATA'].variables['surface_albedo_nitrogendioxide_window'][:].ravel()

            elif self.varname == 'co':
                # grab the averaging kernel and reshape it
                avg_kernel = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                    groups['DETAILED_RESULTS'].variables['column_averaging_kernel'][:]
                avg_kernel = np.reshape(avg_kernel, (nlocs, nlevs))

                # construct the pressure vertices array
                preslv = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                    groups['DETAILED_RESULTS'].variables['pressure_levels'][:]
                preslv = np.reshape(preslv, (nlocs, nlevs))
                top = np.zeros(nlocs, dtype=np.float32)

                # albedo
                albedo1 = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                    groups['DETAILED_RESULTS'].variables['surface_albedo_2325'][:].ravel()
                albedo2 = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                    groups['DETAILED_RESULTS'].variables['surface_albedo_2335'][:].ravel()
                albedo = 0.5 * (albedo1 + albedo2)

            # get angles
            sza = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                groups['GEOLOCATIONS'].variables['solar_zenith_angle'][:].ravel()
            vza = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].\
                groups['GEOLOCATIONS'].variables['viewing_zenith_angle'][:].ravel()

            # assemble presvertices with top vertice
            preslv = np.append(top[:, np.newaxis], preslv, axis=1)

            # scale the avk using AMF ratio and tropopause level for tropo column
            nlocf = len(lats[flg])
            scaleAK = np.ones((nlocf, nlevs), dtype=np.float32)

            self.outdata[('dateTime', metaDataName)] = np.concatenate((
                self.outdata[('dateTime', metaDataName)], times[flg]), dtype=np.int64)
            self.outdata[('latitude', metaDataName)] = np.concatenate((
                self.outdata[('latitude', metaDataName)], lats[flg]), dtype=np.float32)
            self.outdata[('longitude', metaDataName)] = np.concatenate((
                self.outdata[('longitude', metaDataName)], lons[flg]), dtype=np.float32)
            self.outdata[('qualityFlag', metaDataName)] = np.concatenate((
                self.outdata[('qualityFlag', metaDataName)], qa_value[flg]), dtype=np.float32)
            self.outdata[('solarZenithAngle', metaDataName)] = np.concatenate((
                self.outdata[('solarZenithAngle', metaDataName)], sza[flg]), dtype=np.float32)
            self.outdata[('viewingZenithAngle', metaDataName)] = np.concatenate((
                self.outdata[('viewingZenithAngle', metaDataName)], vza[flg]), dtype=np.float32)
            self.outdata[('albedo', metaDataName)] = np.concatenate((
                self.outdata[('albedo', metaDataName)], albedo[flg]), dtype=np.float32)

            if first:
                self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = avg_kernel[flg]
                self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = preslv[flg]
            else:
                self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = np.concatenate((
                    self.outdata[('averagingKernel', 'RetrievalAncillaryData')], avg_kernel[flg]), dtype=np.float32)
                self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = np.concatenate((
                    self.outdata[('pressureVertice', 'RetrievalAncillaryData')], preslv[flg]), dtype=np.float32)

            for ncvar, iodavar in self.obsVar.items():

                if ncvar in ['nitrogendioxide_tropospheric_column',
                             'carbonmonoxide_total_column']:
                    data = ncd.groups['PRODUCT'].variables[ncvar][:].ravel()[flg]
                    err = ncd.groups['PRODUCT'].variables[ncvar+'_precision'][:].ravel()[flg]
                else:
                    data = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS'].variables[ncvar][:].ravel()[flg]
                    err = ncd.groups['PRODUCT'].groups['SUPPORT_DATA'].groups['DETAILED_RESULTS'].variables[ncvar+'_precision'][:].ravel()[flg]

                self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['valKey']], data), dtype=np.float32)
                self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['errKey']], err), dtype=np.float32)
                self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['qcKey']], qc_flag[flg]), dtype=np.int32)

            first = False

        DimDict['Location'] = len(self.outdata[('dateTime', metaDataName)])
        AttrData['Location'] = np.int32(DimDict['Location'])
        DimDict['Layer'] = nlevs
        AttrData['Layer'] = np.int32(DimDict['Layer'])
        DimDict['Vertice'] = nlevs + 1
        AttrData['Vertice'] = np.int32(DimDict['Vertice'])

        varname = 'pressureVertice'
        vkey = (varname, 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'Pa'

        varname = 'averagingKernel'
        vkey = (varname, 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = ''


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
        help="type of column: total or tropophere",
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
    optional.add_argument(
        '--date_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHHmm YYYYMMDDHHmm",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('197001010000', '217001010000'))

    args = parser.parse_args()

    if args.variable == "co":
        var_name = 'carbonmonoxide'
        if args.column == "troposphere":
            print('CO is only available for total column, reset column to total', flush=1)
            args.column = 'total'
    elif args.variable == "no2":
        var_name = 'nitrogendioxide'

    if args.column == "troposphere":

        obsVar = {
            var_name+'_tropospheric_column': var_name+'Column'
        }

        varDims = {
            var_name+'Column': ['Location']
        }

    elif args.column == "total":

        obsVar = {
            var_name+'_total_column': var_name+'Total'
        }

        varDims = {
            var_name+'Total': ['Location']
        }

    varDims['averagingKernel'] = ['Location', 'Layer']
    varDims['pressureVertice'] = ['Location', 'Vertice']

    # Read in the NO2 data
    var = tropomi(args.input, args.variable, args.column, args.qa_value, args.thin, args.date_range, obsVar)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(var.outdata, varDims, var.varAttrs, AttrData)


if __name__ == '__main__':
    main()
