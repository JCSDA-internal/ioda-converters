#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

# Description:
#        This code reads TEMPO Level 2 composition netCDF files and writes
#        selected column retrievals, geolocation, quality information,
#        pressure vertices, and averaging kernels into IODA format.
#        It currently supports NO2 and HCHO inputs; O3 is listed as an
#        option but is not yet implemented.
#
# Usage:
#        python tempo_nc2ioda.py -i tempo_l2_file.nc [tempo_l2_file2.nc ...] \
#             -o tempo_ioda.nc -v no2 -c troposphere
#        -i: one or more TEMPO Level 2 netCDF input files
#        -o: IODA output file path
#        -v: variable name, one of [no2, hcho, o3]
#        -c: column type, one of [total, troposphere]
#        -q: optional maximum QA value to keep before QC, default 0
#        -t: optional random thinning fraction from 0.0 to 1.0, default 0.0

import argparse
import netCDF4 as nc
import numpy as np
import os
import sys

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

            print('FILENAME: ', f)

            # conversion factor from constants
            conv = cm2m2 / Na

            # get dimensions
            mirror = ncd.dimensions['mirror_step'].size
            xtrack = ncd.dimensions['xtrack'].size

            # get global attributes
            AttrData['date_time_string'] = ncd.getncattr('time_reference')[0:19]+'Z'
            AttrData['sensor'] = ncd.getncattr('project')
            AttrData['platform'] = ncd.getncattr('platform')
            AttrData['tempo_l2_version'] = ncd.getncattr('processing_version')
            AttrData['apriori_source'] = ncd.getncattr('apriori_source')
            AttrData['title'] = ncd.getncattr('title')

            # coordinates, mask and RT parameters for BC
            lats = ncd.groups['geolocation'].variables['latitude'][:].ravel()
            lons = ncd.groups['geolocation'].variables['longitude'][:].ravel()
            sza = ncd.groups['geolocation'].variables['solar_zenith_angle'][:].ravel()
            vza = ncd.groups['geolocation'].variables['viewing_zenith_angle'][:].ravel()
            albedo = ncd.groups['support_data'].variables['albedo'][:].ravel()
            qc_flag = ncd.groups['support_data'].variables['ground_pixel_quality_flag'][:]\
                .ravel()
            cld_fra = ncd.groups['support_data'].variables['eff_cloud_fraction'][:]\
                .ravel()
            qa_value = ncd.groups['product'].variables['main_data_quality_flag'][:]\
                .ravel()

            # there are inconsitencies in masking between different variables
            # choose one from one variable and apply it to all the other variables
            mask1 = np.ma.getmaskarray(qa_value)
            mask2 = np.ma.getmaskarray(lats)
            mask = np.logical_or(mask1, mask2)
            lats = np.ma.array(lats, mask=mask)
            lons = np.ma.array(lons, mask=mask)
            qc_flag = np.ma.array(qc_flag, mask=mask)
            cld_fra.mask = False
            cld_fra = np.ma.array(cld_fra, mask=mask)
            qa_value = np.ma.array(qa_value, mask=mask)
            sza.mask = False
            sza = np.ma.array(sza, mask=mask)
            vza.mask = False
            vza = np.ma.array(vza, mask=mask)
            albedo.mask = False
            albedo = np.ma.array(albedo, mask=mask)

            # NO2 and HCHO
            if self.varname == 'no2' or self.varname == 'hcho':

                # pressure levels
                levels = ncd.dimensions['swt_level'].size
                layer_mask = np.repeat(mask[:, np.newaxis], levels, axis=1)
                vertice_mask = np.repeat(mask[:, np.newaxis], levels+1, axis=1)
                sfp = ncd.groups['support_data'].variables['surface_pressure'][:].ravel()
                sfp = np.ma.array(sfp, mask=mask)
                ak = ncd.groups['support_data'].variables['surface_pressure'].Eta_A
                bk = ncd.groups['support_data'].variables['surface_pressure'].Eta_B
                preslev = hPa2Pa * np.transpose(ak[:, np.newaxis] + np.outer(bk, sfp))
                preslev = np.ma.array(preslev, mask=vertice_mask)

                # averaging kernel
                # here we assume avk is scattering weights / AMF
                # there is a mismatch between the mask in the scattering weights/box amf
                # so we need to reset the mask and replace with the mask that is used

                if self.varname == 'no2':
                    err_name = 'vertical_column_'+self.columnType
                    obs_name = 'vertical_column_'+self.columnType
                    col_amf_name = 'amf_'+self.columnType
                    trop_amf_name = 'amf_troposphere'
                    tot_amf_name = 'amf_total'
                    if self.columnType == 'total':
                        group_name = 'support_data'
                    else:
                        group_name = 'product'

                if self.varname == 'hcho':
                    tot_amf_name = 'amf'
                    trop_amf_name = 'amf'
                    col_amf_name = 'amf'
                    obs_name = 'vertical_column'
                    err_name = 'vertical_column'
                    group_name = 'product'

                tot_amf = ncd.groups['support_data'].variables[tot_amf_name][:].ravel()
                tot_amf.mask = False
                tot_amf = np.ma.array(tot_amf, mask=mask)
                trop_amf = ncd.groups['support_data'].variables[trop_amf_name][:].ravel()
                trop_amf.mask = False
                trop_amf = np.ma.array(trop_amf, mask=mask)

                amf_in_ak = ncd.groups['support_data'].variables[col_amf_name][:].ravel()

                # fold amf's native mask + a positive-AMF guard into the global mask
                # (otherwise pixels with fill/zero/negative amf survive and blow up AK = W / amf)
                amf_in_ak_native_mask = np.ma.getmaskarray(amf_in_ak)
                amf_in_ak_bad = amf_in_ak_native_mask | (np.ma.getdata(amf_in_ak) <= 0.0)
                mask = np.logical_or(mask, amf_in_ak_bad)
                layer_mask = np.repeat(mask[:, np.newaxis], levels, axis=1)
                vertice_mask = np.repeat(mask[:, np.newaxis], levels+1, axis=1)

                amf_in_ak = np.ma.array(np.ma.getdata(amf_in_ak), mask=mask)

                # w = box_amf = scattering_weights
                w = ncd.groups['support_data'].variables['scattering_weights'][:]\
                    .reshape(mirror * xtrack, levels)
                w.mask = False
                w = np.ma.array(w, mask=layer_mask)

                avg_kernel = w / amf_in_ak[:, np.newaxis]

                # for no2 use avk to define strat trop separation
                if self.varname == 'no2':
                    t_pause = hPa2Pa * ncd.groups['support_data'].variables['tropopause_pressure'][:]\
                        .ravel()

                    if self.columnType != "total":
                        t_diff = np.array(t_pause[:, np.newaxis] - preslev)[:, :-1]
                        if self.columnType == "troposphere":
                            avg_kernel[t_diff > 0] = 0.0

                    # make sure that the avk mask is correctly put
                    avg_kernel.mask = False
                    avg_kernel = np.ma.array(avg_kernel, mask=layer_mask)

                # from ATBD:
                # total vertical column = stratospheric + tropospheric vertical column
                # Do not use support_data/vertical_column_total as it is influenced by a priori
                if self.columnType == "total":
                    obs = (
                        ncd.groups['product'].variables['vertical_column_troposphere'][:].ravel()
                        + ncd.groups['product'].variables['vertical_column_stratosphere'][:].ravel()
                    ) * conv
                else:
                    obs = ncd.groups['product'].variables[obs_name][:]\
                        .ravel() * conv

                # Reuse amf_in_ak as col_amf: they are the same netCDF variable
                # (col_amf_name), and amf_in_ak already carries the updated mask that
                # includes the positive-AMF guard, which prevents divide-by-zero /
                # inf / nan in the err computation below.
                col_amf = amf_in_ak

                obs.mask = False
                obs = np.ma.array(obs, mask=mask)

                # err = fitted_slant_column_uncertainty / AMF (total, tropospheric, or stratospheric)
                # for tropospheric this is the same is product.vertical_column_troposphere_uncertainty
                err = (
                    ncd.groups['support_data']['fitted_slant_column_uncertainty'][:].ravel()
                    / col_amf
                ) * conv
                err.mask = False
                err = np.ma.array(err, mask=mask)

            # O3
            if self.varname == 'O3':
                print("O3 product converter not ready yet")
                exit()

            # Apply the final location mask to all 1D location-dependent arrays.
            # Some products add bad AMF pixels after the initial geolocation mask is built.
            lats = np.ma.array(np.ma.getdata(lats), mask=mask)
            lons = np.ma.array(np.ma.getdata(lons), mask=mask)
            qc_flag = np.ma.array(np.ma.getdata(qc_flag), mask=mask)
            cld_fra = np.ma.array(np.ma.getdata(cld_fra), mask=mask)
            qa_value = np.ma.array(np.ma.getdata(qa_value), mask=mask)
            sza = np.ma.array(np.ma.getdata(sza), mask=mask)
            vza = np.ma.array(np.ma.getdata(vza), mask=mask)
            albedo = np.ma.array(np.ma.getdata(albedo), mask=mask)
            tot_amf = np.ma.array(np.ma.getdata(tot_amf), mask=mask)
            trop_amf = np.ma.array(np.ma.getdata(trop_amf), mask=mask)
            preslev = np.ma.array(np.ma.getdata(preslev), mask=vertice_mask)

            # time
            time_ref = np.datetime64(AttrData['date_time_string'])
            dt = ncd.groups['geolocation'].variables['time'][:].ravel()
            time = time_ref + dt.astype('timedelta64[s]')
            time = np.repeat([str(element) + 'Z' for element in time], xtrack)
            time = np.ma.array(time, mask=mask, dtype=object)

            # adding ability to pre filter the data using the qa value
            # and also perform thinning using random uniform draw
            qaf = ((qa_value <= self.qa_flg) & (qa_value >= 0))
            thi = np.random.uniform(size=len(qa_value)) > self.thin
            flg = np.logical_and(qaf, thi)

            # remove cloudy data with cf>50%
            cld = cld_fra < 0.5

            flg = np.logical_and(flg, cld)

            # clean data
            neg_obs = obs > 0.0
            nan_obs = np.isfinite(obs) & np.isfinite(err)
            cln = np.logical_and(neg_obs, nan_obs)

            # final flag before sending this to ioda engines
            flg = np.logical_and(flg, cln)

            # print before compression
            print('BEFORE COMPRESSION')
            print('lats: ', np.shape(lats))
            print('lons: ', np.shape(lons))
            print('time: ', np.shape(time))
            print('flg: ', np.shape(flg))
            print('qa_value: ', np.shape(qa_value))
            print('cld_fra: ', np.shape(cld_fra))
            print('sza: ', np.shape(sza))
            print('vza: ', np.shape(vza))
            print('albedo: ', np.shape(albedo))
            print('amf_total: ', np.shape(tot_amf))
            print('amf_tropo: ', np.shape(trop_amf))
            print('qc_flag: ', np.shape(qc_flag))
            print('obs: ', np.shape(obs))
            print('err: ', np.shape(err))
            print('preslev: ', np.shape(preslev))
            print('avg_kernel: ', np.shape(avg_kernel))
            print('w: ', np.shape(w))

            # remove masked Data and make sure types are correct
            lats = np.ma.compressed(lats).astype('float32')
            lons = np.ma.compressed(lons).astype('float32')
            time = np.ma.compressed(time)
            flg = np.ma.compressed(flg)
            qa_value = np.ma.compressed(qa_value).astype('float32')
            cld_fra = np.ma.compressed(cld_fra).astype('float32')
            sza = np.ma.compressed(sza).astype('float32')
            vza = np.ma.compressed(vza).astype('float32')
            albedo = np.ma.compressed(albedo).astype('float32')
            tot_amf = np.ma.compressed(tot_amf).astype('float32')
            trop_amf = np.ma.compressed(trop_amf).astype('float32')
            qc_flag = np.ma.compressed(qc_flag).astype('int32')
            obs = np.ma.compressed(obs).astype('float32')
            err = np.ma.compressed(err).astype('float32')
            preslev = np.ma.compress_rowcols(preslev, axis=0).astype('float32')
            avg_kernel = np.ma.compress_rowcols(avg_kernel, axis=0).astype('float32')
            w = np.ma.compress_rowcols(w, axis=0).astype('float32')

            # flip 2d arrays to have increaing pressure
            if np.shape(lats)[0] > 0:
                preslev = np.flip(preslev, axis=1)
                avg_kernel = np.flip(avg_kernel, axis=1)
                w = np.flip(w, axis=1)

                # print after compression
                print('AFTER COMPRESSION')
                print('lats: ', np.shape(lats))
                print('lons: ', np.shape(lons))
                print('time: ', np.shape(time))
                print('flg: ', np.shape(flg))
                print('qa_value: ', np.shape(qa_value))
                print('cld_fra: ', np.shape(cld_fra))
                print('sza: ', np.shape(sza))
                print('vza: ', np.shape(vza))
                print('albedo: ', np.shape(albedo))
                print('amf_total: ', np.shape(tot_amf))
                print('amf_tropo: ', np.shape(trop_amf))
                print('qc_flag: ', np.shape(qc_flag))
                print('obs: ', np.shape(obs))
                print('err: ', np.shape(err))
                print('preslev: ', np.shape(preslev))
                print('avg_kernel: ', np.shape(avg_kernel))
                print('w: ', np.shape(w))
                print(np.shape(time[flg]))
                if first:
                    self.outdata[('dateTime', 'MetaData')] = time[flg]
                    self.outdata[('latitude', 'MetaData')] = lats[flg]
                    self.outdata[('longitude', 'MetaData')] = lons[flg]
                    self.outdata[('qualityFlags', 'MetaData')] = qa_value[flg]
                    self.outdata[('cloudAmount', 'MetaData')] = cld_fra[flg]
                    self.outdata[('solarZenithAngle', 'MetaData')] = sza[flg]
                    self.outdata[('viewingZenithAngle', 'MetaData')] = vza[flg]
                    self.outdata[('albedo', 'MetaData')] = albedo[flg]
                    self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = avg_kernel[flg]
                    self.outdata[('w', 'RetrievalAncillaryData')] = w[flg]
                    self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = preslev[flg]
                    self.outdata[self.varDict[iodavar]['valKey']] = obs[flg]
                    self.outdata[self.varDict[iodavar]['errKey']] = err[flg]
                    self.outdata[self.varDict[iodavar]['qcKey']] = qc_flag[flg]
                else:
                    self.outdata[('dateTime', 'MetaData')] = np.concatenate((
                        self.outdata[('dateTime', 'MetaData')], time[flg]))
                    self.outdata[('latitude', 'MetaData')] = np.concatenate((
                        self.outdata[('latitude', 'MetaData')], lats[flg]))
                    self.outdata[('longitude', 'MetaData')] = np.concatenate((
                        self.outdata[('longitude', 'MetaData')], lons[flg]))
                    self.outdata[('qualityFlags', 'MetaData')] = np.concatenate((
                        self.outdata[('qualityFlags', 'MetaData')], qa_value[flg]))
                    self.outdata[('cloudAmount', 'MetaData')] = np.concatenate((
                        self.outdata[('cloudAmount', 'MetaData')], cld_fra[flg]))
                    self.outdata[('solarZenithAngle', 'MetaData')] = np.concatenate((
                        self.outdata[('solarZenithAngle', 'MetaData')], sza[flg]))
                    self.outdata[('viewingZenithAngle', 'MetaData')] = np.concatenate((
                        self.outdata[('viewingZenithAngle', 'MetaData')], vza[flg]))
                    self.outdata[('albedo', 'MetaData')] = np.concatenate((
                        self.outdata[('albedo', 'MetaData')], albedo[flg]))
                    self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = np.concatenate((
                        self.outdata[('averagingKernel', 'RetrievalAncillaryData')], avg_kernel[flg]))
                    self.outdata[('w', 'RetrievalAncillaryData')] = np.concatenate((
                        self.outdata[('w', 'RetrievalAncillaryData')], w[flg]))
                    self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = np.concatenate((
                        self.outdata[('pressureVertice', 'RetrievalAncillaryData')], preslev[flg]))
                    self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['valKey']], obs[flg]))
                    self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['errKey']], err[flg]))
                    self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                        (self.outdata[self.varDict[iodavar]['qcKey']], qc_flag[flg]))

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

        varname = 'w'
        vkey = (varname, 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = ''


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads TEMPO Level 2 composition netCDF files and converts them '
            'into IODA formatted output files. Multiple files are able to be '
            'concatenated. NO2 proxy V03 and V04 are supported.')
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
        help="type of column: total, troposphere",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-q', '--qa_value',
        help="qa value used to preflag data that goes into file before QC"
        "0 normal, 1 suspicious, 2 bad",
        type=int, default=0)
    optional.add_argument(
        '-t', '--thin',
        help="percentage of random thinning from 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    args = parser.parse_args()

    if args.variable == "hcho":
        var_name = 'formaldehyde'
        if args.column != "troposphere":
            print('hcho is only available for troposphere column, reset column to troposphere', flush=1)
            args.column = 'troposphere'
    elif args.variable == "no2":
        var_name = 'nitrogendioxide'
    elif args.variable == "o3":
        var_name = 'ozone'

    AttrData['column_type'] = args.column

    if args.column == "troposphere":

        obsVar = {
            var_name+'_'+args.column+'_column': var_name+'Column'
        }

        varDims = {
            var_name+'Column': ['Location']
        }

    elif args.column == "total":

        obsVar = {
            var_name+'_total_column': var_name+'Column'
        }

        varDims = {
            var_name+'Column': ['Location']
        }

    varDims['averagingKernel'] = ['Location', 'Layer']
    varDims['w'] = ['Location', 'Layer']
    varDims['pressureVertice'] = ['Location', 'Vertice']

    # Read in the NO2 data
    var = tempo(args.input, args.variable, args.column, args.qa_value, args.thin, obsVar)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(var.outdata, varDims, var.varAttrs, AttrData)


if __name__ == '__main__':
    main()
