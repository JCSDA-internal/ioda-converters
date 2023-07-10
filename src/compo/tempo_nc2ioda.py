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

# remove this when done
np.set_printoptions(threshold=np.inf)

# constants
hPa2Pa = 1E+2
Na = 6.0221408E+23
cm2m2 = 1E+4
molarmass = {"no2": 46.0055, "hcho": 30.031, "o3": 48.0}


class tempo(object):
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
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'mol m-2'
        self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mol m-2'
        # loop through input filenames
        first = True
        for f in self.filenames:
            ncd = nc.Dataset(f, 'r')

            # conversion factor fron constants
            conv = cm2m2 * molarmass[self.varname] / Na

            # get dimensions
            mirror = ncd.dimensions['mirror_step'].size
            xtrack = ncd.dimensions['xtrack'].size

            # get global attributes
            AttrData['date_time_string'] = ncd.getncattr('time_reference')[0:19]+'Z'
            AttrData['sensor'] = ncd.getncattr('project')
            AttrData['platform'] = ncd.getncattr('platform')

            # coordinates and mask
            lats = ncd.groups['geolocation'].variables['latitude'][:].ravel()
            lons = ncd.groups['geolocation'].variables['longitude'][:].ravel()
            qa_value = ncd.groups['product'].variables['main_data_quality_flag'][:]\
                .ravel()
            qc_flag = ncd.groups['support_data'].variables['ground_pixel_quality_flag'][:]\
                .ravel()
            mask = np.ma.getmask(qa_value)
            lats = np.ma.array(lats, mask=mask)
            lons = np.ma.array(lons, mask=mask)
            qc_flag = np.ma.array(qc_flag, mask=mask)

            # adding ability to pre filter the data using the qa value
            # and also perform thinning using random uniform draw
            qaf = qa_value <= self.qa_flg
            thi = np.random.uniform(size=len(qa_value)) > self.thin
            flg = np.logical_and(qaf, thi)

            # time
            time_ref = np.datetime64(AttrData['date_time_string'])
            dt = ncd.groups['geolocation'].variables['time'][:].ravel()
            time = time_ref + dt.astype('timedelta64[s]')
            time = np.repeat([str(element) + 'Z' for element in time], xtrack)
            time = np.ma.array(time, mask=mask, dtype=object)

            if self.varname == 'no2' or self.varname == 'hcho':

                # pressure levels
                levels = ncd.dimensions['swt_level'].size
                sfp = ncd.groups['support_data'].variables['surface_pressure'][:].ravel()
                sfp = np.ma.array(sfp, mask=mask)
                ak = ncd.groups['support_data'].variables['surface_pressure'].Eta_A
                bk = ncd.groups['support_data'].variables['surface_pressure'].Eta_B
                preslev = hPa2Pa * np.transpose(ak[:, np.newaxis] + np.outer(bk, sfp))
                preslev = np.ma.array(preslev, mask=np.repeat(mask, levels+1))

                # averaging kernel
                # here we assume avk is scattering weights / AMF
                # there is a mismatch between the mask in the scattering weights/box amf
                # so we need to reset the mask and replace with the mask that is used
                t_pause = hPa2Pa * ncd.groups['support_data'].variables['tropopause_pressure'][:]\
                    .ravel()
                tot_amf = ncd.groups['support_data'].variables['amf_total'][:].ravel()
                tot_amf.mask = False
                tot_amf = np.ma.array(tot_amf, mask=mask)
                box_amf = ncd.groups['support_data'].variables['scattering_weights'][:]\
                    .reshape(mirror * xtrack, levels)
                # mask1 = np.ma.getmask(box_amf)
                box_amf.mask = False
                box_amf = np.ma.array(box_amf, mask=np.repeat(mask, levels))
                avg_kernel = box_amf / tot_amf[:, np.newaxis]

                # also using avk to define strat trop separation
                if self.columnType != "total":
                    t_diff = np.array(t_pause[:, np.newaxis] - preslev)[:, :-1]
                    if self.columnType == "strato":
                        avg_kernel[t_diff <= 0] = 0.0
                    if self.columnType == "tropo":
                        avg_kernel[t_diff > 0] = 0.0
                    if first:
                        self.columnType += "sphere"
                # make sure that the avk mask is correctly put
                avg_kernel = np.ma.array(avg_kernel, mask=np.repeat(mask, levels))

                # obs value and error
                col_amf = ncd.groups['support_data'].variables["amf_"+self.columnType][:].ravel()
                obs = ncd.groups['product'].variables["vertical_column_"+self.columnType][:]\
                    .ravel() * conv
                # there is apparently a mismatch in the mask for obs
                obs = np.ma.masked_invalid(obs)

                # FIXME: it seems that the proxy product is incomplete as it doesn't provide values for
                # SCV uncertainty. To have the partial column error calculated like in
                # Boersma et al., 2003 we must have this quantity (i.e. compute SCD/AMF).
                # For we can only do a "wrong" scaling of the VCD error such as VCD/AMF(tropo|strato)
                # the correct line of code below:
                # err = ncd.groups['support_data'].variables["fitted_slant_column_uncertainty"][:].\
                #       ravel() * conv / col_amf
                # teporary error workaround below:
                if self.columnType == "total":
                    col_amf = np.ones((xtrack*mirror))
                err = ncd.groups['product'].variables["vertical_column_total_uncertainty"][:]\
                    .ravel() * conv / col_amf

            # clean data
            neg_obs = obs > 0.0
            nan_obs = obs != np.nan
            cln = np.logical_and(neg_obs, nan_obs)

            # final flag before sending this to ioda engines
            flg = np.logical_and(flg, cln)

            # remove masked Data
            lats = np.ma.compressed(lats)
            lons = np.ma.compressed(lons)
            time = np.ma.compressed(time)
            flg = np.ma.compressed(flg)
            qa_value = np.ma.compressed(qa_value)
            qc_flag = np.ma.compressed(qc_flag)
            obs = np.ma.compressed(obs)
            err = np.ma.compressed(err)
            preslev = np.ma.compress_rowcols(preslev, axis=0)
            avg_kernel = np.ma.compress_rowcols(avg_kernel, axis=0)

            if first:
                self.outdata[('dateTime', 'MetaData')] = time
                self.outdata[('latitude', 'MetaData')] = lats
                self.outdata[('longitude', 'MetaData')] = lons
                self.outdata[('quality_assurance_value', 'MetaData')] = qa_value
                self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = avg_kernel
                self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = preslev
                self.outdata[self.varDict[iodavar]['valKey']] = obs
                self.outdata[self.varDict[iodavar]['errKey']] = err
                self.outdata[self.varDict[iodavar]['qcKey']] = qc_flag
            else:
                self.outdata[('dateTime', 'MetaData')] = np.concatenate((
                    self.outdata[('dateTime', 'MetaData')], time))
                self.outdata[('latitude', 'MetaData')] = np.concatenate((
                    self.outdata[('latitude', 'MetaData')], lats))
                self.outdata[('longitude', 'MetaData')] = np.concatenate((
                    self.outdata[('longitude', 'MetaData')], lons))
                self.outdata[('quality_assurance_value', 'MetaData')] = np.concatenate((
                    self.outdata[('quality_assurance_value', 'MetaData')], qa_value))
                self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = np.concatenate((
                    self.outdata[('averagingKernel', 'RetrievalAncillaryData')], avg_kernel))
                self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = np.concatenate((
                    self.outdata[('pressureVertice', 'RetrievalAncillaryData')], preslev))
                self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['valKey']], obs))
                self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['errKey']], err))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['qcKey']], qc_flag))

            first = False

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])
        AttrData['Location'] = np.int32(DimDict['Location'])
        DimDict['Layer'] = levels
        AttrData['Layer'] = np.int32(DimDict['Layer'])
        DimDict['Vertice'] = levels + 1
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
            'Reads TEMPO NO2 PROXY netCDF files: '
            'from ttps://asdc.larc.nasa.gov/data/TEMPO/NO2-PROXY_L2_V01/'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of TEMPO L2 NO2 observation netCDF input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-v', '--variable',
        help="name of varibale, available list: [no2, hcho, o3]",
        type=str, required=True)
    required.add_argument(
        '-c', '--column',
        help="type of column: total, tropo or strato",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-q', '--qa_value',
        help="qa value used to preflag data that goes into file before QC"
        "0 normal, 1 suspicious, 2 bad",
        type=float, default=0.0)
    optional.add_argument(
        '-n', '--thin',
        help="percentage of random thinning from 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    args = parser.parse_args()

    if args.variable == "hcho":
        var_name = 'formaldehyde'
        if args.column != "total":
            print('hcho is only available for total column, reset column to total', flush=1)
            args.column = 'total'
    elif args.variable == "no2":
        var_name = 'nitrogendioxide'
    elif args.variable == "o3":
        var_name = 'ozone'
        varDims['averagingKernel'] = ['Location', 'Layer']
    if args.column == "tropo" or args.column == "strato":

        obsVar = {
            var_name+'_'+args.column+'spheric_column': var_name+'Column'
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

    varDims['pressureVertice'] = ['Location', 'Vertice']

    # Read in the NO2 data
    var = tempo(args.input, args.variable, args.column, args.qa_value, args.thin, obsVar)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(var.outdata, varDims, var.varAttrs, AttrData)


if __name__ == '__main__':
    main()
