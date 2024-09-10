#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
from datetime import datetime
import netCDF4 as nc
import numpy as np
import os

import glob
import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict


def flag_by_confidence_lvl(qfarr, aerosol_type, conf_lvl='MedHigh',print_diag=False):

    # ADP smoke confidence is defined in (counting from 0) bit 2-3, dust in bit 4-5
    # input array contains decimal representations of the binary values
    # 10 (2 in base10) is medium confidence and 00 (0 in base10) is high confidence
    # 01 (1 in base10) is low confidence and 11 (3 in base10) is missing/bad data

    if conf_lvl == 'med' or conf_lvl == 'medhigh':
        conf_lvl = 'MedHigh'
    if conf_lvl == 'low':
        conf_lvl = 'Low'
    if conf_lvl == 'high':
        conf_lvl = 'High'

    if aerosol_type == 'Smoke':
        rhtrim = np.floor(qfarr/4)  # trim off the 2 least significant bits
    elif aerosol_type == 'Dust':
        rhtrim = np.floor(qfarr/16)  # trim off the 4 least significant bits
    else:
        print(f'ERROR: aerosol_type {aerosol_type} not valid')

    conf = np.mod(rhtrim, 4)  # trim off all except the 2 least significant bits of the remaining value; this gives the value what we want

    if conf_lvl == 'MedHigh':
        inv_conf_mask = np.logical_or(conf == 0, conf == 2)
    elif conf_lvl == 'High':
        inv_conf_mask = conf == 0
    elif conf_lvl == 'Med':
        inv_conf_mask = conf == 2
    elif conf_lvl == 'Low':
        inv_conf_mask = conf == 1
    else:
        print(f'ERROR: No configuration for confidence level {conf_lvl}')

    if print_diag:
       for i,val in enumerate(conf_mask):
          qfarr_i = int(qfarr[i])
          conf_i = int(conf[i])
          # use bitmasks to convert bin() output for negative vals to two's complement
          print(f'{i} Orig dvalue: {qfarr_i} Orig bvalue: {bin(qfarr_i & 0b11111111)}  {aerosol_type} bvalue:'
                 '{bin(conf_i & 0b11)} mask_value: {val}')

    return inv_conf_mask  # true elements here are values we want to keep (invert for an actual mask)


locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

obsvars = ["aerosolOpticalDepth"]
channels = [4]

# A dictionary of global attributes.  More filled in further down.
AttrData = {}
AttrData['ioda_object_type'] = 'AOD'

# A dictionary of variable dimensions.
DimDict = {}
DimDict_smoke = {}
DimDict_dust = {}

# A dictionary of variable names and their dimensions.
VarDims = {'aerosolOpticalDepth': ['Location', 'Channel']}

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

long_missing_value = nc.default_fillvals['i8']


class AOD(object):
    def __init__(self, filenames, method, mask, thin, adp_mask, adp_qc_lvl):
        self.filenames = filenames
        self.mask = mask
        self.method = method
        self.thin = thin
        self.adp_mask = adp_mask
        self.adp_qc_lvl = adp_qc_lvl
        if self.adp_qc_lvl is None:
            self.adp_qc_lvl = 'MedHigh'  # default to including Medium and High Confidence obs based on ADP data
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        if self.adp_mask:
            self.outdata_smoke = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
            self.outdata_dust = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):

        # set up variable names for IODA
        for iodavar in obsvars:
            self.varDict[iodavar]['valKey'] = iodavar, obsValName
            self.varDict[iodavar]['errKey'] = iodavar, obsErrName
            self.varDict[iodavar]['qcKey'] = iodavar, qcName
            self.varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, qcName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, obsValName]['_FillValue'] = -9999.
            self.varAttrs[iodavar, obsErrName]['_FillValue'] = -9999.
            self.varAttrs[iodavar, qcName]['_FillValue'] = -9999
            self.varAttrs[iodavar, obsValName]['units'] = '1'
            self.varAttrs[iodavar, obsErrName]['units'] = '1'

        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=object)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        if self.adp_mask:
            self.outdata_smoke[('latitude', metaDataName)] = np.array([], dtype=np.float32)
            self.outdata_smoke[('longitude', metaDataName)] = np.array([], dtype=np.float32)
            self.outdata_smoke[('dateTime', metaDataName)] = np.array([], dtype=object)
            for iodavar in obsvars:
                self.outdata_smoke[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
                self.outdata_smoke[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
                self.outdata_smoke[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

            self.outdata_dust[('latitude', metaDataName)] = np.array([], dtype=np.float32)
            self.outdata_dust[('longitude', metaDataName)] = np.array([], dtype=np.float32)
            self.outdata_dust[('dateTime', metaDataName)] = np.array([], dtype=object)
            for iodavar in obsvars:
                self.outdata_dust[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
                self.outdata_dust[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
                self.outdata_dust[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        # loop through input filenamess
        for f in self.filenames:
            try:
                ncd = nc.Dataset(f, 'r')
            except FileNotFoundError:
                print(f'AOD data file {f} not found, continuing to next file in list or ending if at end of list.')
                continue
            gatts = {attr: getattr(ncd, attr) for attr in ncd.ncattrs()}
            base_datetime = datetime.strptime(gatts["time_coverage_end"], '%Y-%m-%dT%H:%M:%SZ')
            self.satellite = gatts["satellite_name"]
            self.sensor = gatts["instrument_name"]
            AttrData["platform"] = self.satellite
            AttrData["sensor"] = self.sensor

            if AttrData['sensor'] == 'VIIRS':
                AttrData['sensor'] = "v.viirs-m_npp"
            if AttrData['platform'] == 'NPP':
                AttrData['platform'] = "suomi_npp"

            lons = ncd.variables['Longitude'][:].ravel()
            lats = ncd.variables['Latitude'][:].ravel()
            vals = ncd.variables['AOD550'][:].ravel()
            errs = ncd.variables['Residual'][:].ravel()

            # QCPath is the flag for retrieval path. The valid range is 0-127 in the
            # ATBD: https://www.star.nesdis.noaa.gov/jpss/documents/ATBD/ATBD_EPS_Aerosol_AOD_v3.4.pdf.
            # QCPath's valid range in the input file is not correct, so we define the valid range here.
            qcpath = ncd.variables['QCPath'][:].data.ravel()
            qcpath = np.ma.masked_array(qcpath, np.logical_or(qcpath < 0, qcpath > 127))

            qcall = ncd.variables['QCAll'][:].ravel().astype('int32')
            obs_time = np.full(np.shape(qcall), base_datetime, dtype=object)

            ncd.close()

            # apply ADP mask if indicated by user
            if self.adp_mask:

                # Set path and filename of adp data corresponding to aod data
                # Note: this works for the directory structure that exists for these on hera
                # another approach would be to have the masked output filenames specified as a CLI argument

                # the creation time can differ between the AOD and ADP filenames,
                # break up fname and reconstruct it from pieces after using glob to get corresponding ADP fname
                adp_fn_ctime_to_end = f.replace('aod', 'adp').replace("JRR-AOD", "JRR-ADP").split('_')[-1]
                adp_fn_before_ctime = f.replace('aod', 'adp').replace("JRR-AOD", "JRR-ADP")[:-19]
                adp_fn = glob.glob(adp_fn_before_ctime+'*')
                if len(adp_fn) == 1:
                    adp_fn = adp_fn[0]
                    print(f'Found ADP file: {adp_fn}')
                elif len(adp_fn) > 1:
                    print(f'AOD file: {f}')
                    print(f'ADP files: {adp_fn}')
                    print(f'ERROR: Too many ADP files found for AOD file, skipping granule.')
                    continue

                try:
                    ncd_adp = nc.Dataset(adp_fn, 'r')
                except FileNotFoundError:
                    # this will skip creating output for both the AOD and ADP at the selected time;
                    # however, that should be fine, as when this code runs we are interested only in
                    # the ADP-filtered output rather than the full AOD output.
                    print(f'ADP data file {adp_fn} not found, continuing to next AOD file on list (or ending).')
                    continue

                # Get ADP smoke/dust masks
                # These are 1=True=Smoke/Dust and 0=False=No Smoke/Dust. The 1 values are the ones
                # we want to *keep*, so to mask out all the non-smoke/non-dust values below,
                # invert the mask elements here
                adpmasksmoke = np.logical_not(ncd_adp.variables['Smoke'][:].ravel().astype(bool))
                adpmaskdust = np.logical_not(ncd_adp.variables['Dust'][:].ravel().astype(bool))

                qcflag = ncd_adp.variables['QC_Flag'][:, :].ravel()

                # As above, in the confidence masks, med-high confidence is marked by True mask elements,
                # due to the line conf_mask = np.logical_or(conf == 0, conf == 2) in the get functions,
                # so invert the masks the same as above
                conf_mask_smoke = np.logical_not(flag_by_confidence_lvl(qcflag, 'Smoke', self.adp_qc_lvl))
                conf_mask_dust = np.logical_not(flag_by_confidence_lvl(qcflag, 'Dust', self.adp_qc_lvl))

                ncd_adp.close()

                # apply masks from ADP data including QC masks
                vals_smoke = np.ma.masked_where(np.logical_or(adpmasksmoke, conf_mask_smoke), vals)
                lons_smoke = np.ma.masked_where(np.logical_or(adpmasksmoke, conf_mask_smoke), lons)
                lats_smoke = np.ma.masked_where(np.logical_or(adpmasksmoke, conf_mask_smoke), lats)
                errs_smoke = np.ma.masked_where(np.logical_or(adpmasksmoke, conf_mask_smoke), errs)
                qcpath_smoke = np.ma.masked_where(np.logical_or(adpmasksmoke, conf_mask_smoke), qcpath)
                qcall_smoke = np.ma.masked_where(np.logical_or(adpmasksmoke, conf_mask_smoke), qcall)
                obs_time_smoke = np.ma.masked_where(np.logical_or(adpmasksmoke, conf_mask_smoke), obs_time)

                vals_dust = np.ma.masked_where(np.logical_or(adpmaskdust, conf_mask_dust), vals)
                lons_dust = np.ma.masked_where(np.logical_or(adpmaskdust, conf_mask_dust), lons)
                lats_dust = np.ma.masked_where(np.logical_or(adpmaskdust, conf_mask_dust), lats)
                errs_dust = np.ma.masked_where(np.logical_or(adpmaskdust, conf_mask_dust), errs)
                qcpath_dust = np.ma.masked_where(np.logical_or(adpmaskdust, conf_mask_dust), qcpath)
                qcall_dust = np.ma.masked_where(np.logical_or(adpmaskdust, conf_mask_dust), qcall)
                obs_time_dust = np.ma.masked_where(np.logical_or(adpmaskdust, conf_mask_dust), obs_time)

            # Remove masked elements if 'maskout' is indictated
            if self.mask == 'maskout':
                mask = np.logical_not(vals.mask)
                vals = vals[mask]
                lons = lons[mask]
                lats = lats[mask]
                errs = errs[mask]
                qcpath = qcpath[mask]
                qcall = qcall[mask]
                obs_time = obs_time[mask]

                if self.adp_mask:

                    mask_smoke = np.logical_not(vals_smoke.mask)
                    vals_smoke = vals_smoke[mask_smoke]
                    lons_smoke = lons_smoke[mask_smoke]
                    lats_smoke = lats_smoke[mask_smoke]
                    errs_smoke = errs_smoke[mask_smoke]
                    qcpath_smoke = qcpath_smoke[mask_smoke]
                    qcall_smoke = qcall_smoke[mask_smoke]
                    obs_time_smoke = obs_time_smoke[mask_smoke]

                    mask_dust = np.logical_not(vals_dust.mask)
                    vals_dust = vals_dust[mask_dust]
                    lons_dust = lons_dust[mask_dust]
                    lats_dust = lats_dust[mask_dust]
                    errs_dust = errs_dust[mask_dust]
                    qcpath_dust = qcpath_dust[mask_dust]
                    qcall_dust = qcall_dust[mask_dust]
                    obs_time_dust = obs_time_dust[mask_dust]

            # Apply thinning mask
            if self.thin > 0.0:

                mask_thin = np.random.uniform(size=len(lons)) > self.thin
                vals = vals[mask_thin]
                lons = lons[mask_thin]
                lats = lats[mask_thin]
                errs = errs[mask_thin]
                qcpath = qcpath[mask_thin]
                qcall = qcall[mask_thin]
                obs_time = obs_time[mask_thin]

                if self.adp_mask:

                    # switch smoke and dust to just use mask_thin if moved above maskout block
                    mask_thin_smoke = np.random.uniform(size=len(lons_smoke)) > self.thin
                    vals_smoke = vals_smoke[mask_thin_smoke]
                    lons_smoke = lons_smoke[mask_thin_smoke]
                    lats_smoke = lats_smoke[mask_thin_smoke]
                    errs_smoke = errs_smoke[mask_thin_smoke]
                    qcpath_smoke = qcpath_smoke[mask_thin_smoke]
                    qcall_smoke = qcall_smoke[mask_thin_smoke]
                    obs_time_smoke = obs_time_smoke[mask_thin_smoke]

                    mask_thin_dust = np.random.uniform(size=len(lons_dust)) > self.thin
                    vals_dust = vals_dust[mask_thin_dust]
                    lons_dust = lons_dust[mask_thin_dust]
                    lats_dust = lats_dust[mask_thin_dust]
                    errs_dust = errs_dust[mask_thin_dust]
                    qcpath_dust = qcpath_dust[mask_thin_dust]
                    qcall_dust = qcall_dust[mask_thin_dust]
                    obs_time_dust = obs_time_dust[mask_thin_dust]

            # calculate and store ob errors
            # defined surface type and uncertainty
            if self.method == "nesdis":
                errs = 0.111431 + 0.128699*vals    # over land (dark)
                errs[qcpath % 2 == 1] = 0.00784394 + 0.219923*vals[qcpath % 2 == 1]  # over ocean
                errs[qcpath % 4 == 2] = 0.0550472 + 0.299558*vals[qcpath % 4 == 2]   # over bright land

                # also need to make these changes for smoke and dust filtered arrays if smoke and dust filtering indicated by user
                if self.adp_mask:
                    errs_smoke = 0.111431 + 0.128699*vals_smoke    # over land (dark)
                    errs_smoke[qcpath_smoke % 2 == 1] = 0.00784394 + 0.219923*vals_smoke[qcpath_smoke % 2 == 1]  # over ocean
                    errs_smoke[qcpath_smoke % 4 == 2] = 0.0550472 + 0.299558*vals_smoke[qcpath_smoke % 4 == 2]   # over bright land

                    errs_dust = 0.111431 + 0.128699*vals_dust    # over land (dark)
                    errs_dust[qcpath_dust % 2 == 1] = 0.00784394 + 0.219923*vals_dust[qcpath_dust % 2 == 1]  # over ocean
                    errs_dust[qcpath_dust % 4 == 2] = 0.0550472 + 0.299558*vals_dust[qcpath_dust % 4 == 2]   # over bright land

            #  Write out data
            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)], np.array(lats, dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)], np.array(lons, dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)], np.array(obs_time, dtype=object))

            for iodavar in obsvars:

                self.outdata[self.varDict[iodavar]['valKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['valKey']], np.array(vals, dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['errKey']], np.array(errs, dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['qcKey']], np.array(qcall, dtype=np.int32))

            if self.adp_mask:

                self.outdata_smoke[('latitude', metaDataName)] = np.append(
                    self.outdata_smoke[('latitude', metaDataName)], np.array(lats_smoke, dtype=np.float32))
                self.outdata_smoke[('longitude', metaDataName)] = np.append(
                    self.outdata_smoke[('longitude', metaDataName)], np.array(lons_smoke, dtype=np.float32))
                self.outdata_smoke[('dateTime', metaDataName)] = np.append(
                    self.outdata_smoke[('dateTime', metaDataName)], np.array(obs_time_smoke, dtype=object))

                self.outdata_dust[('latitude', metaDataName)] = np.append(
                    self.outdata_dust[('latitude', metaDataName)], np.array(lats_dust, dtype=np.float32))
                self.outdata_dust[('longitude', metaDataName)] = np.append(
                    self.outdata_dust[('longitude', metaDataName)], np.array(lons_dust, dtype=np.float32))
                self.outdata_dust[('dateTime', metaDataName)] = np.append(
                    self.outdata_dust[('dateTime', metaDataName)], np.array(obs_time_dust, dtype=object))

                for iodavar in obsvars:

                    self.outdata_smoke[self.varDict[iodavar]['valKey']] = np.append(
                        self.outdata_smoke[self.varDict[iodavar]['valKey']], np.array(vals_smoke, dtype=np.float32))
                    self.outdata_smoke[self.varDict[iodavar]['errKey']] = np.append(
                        self.outdata_smoke[self.varDict[iodavar]['errKey']], np.array(errs_smoke, dtype=np.float32))
                    self.outdata_smoke[self.varDict[iodavar]['qcKey']] = np.append(
                        self.outdata_smoke[self.varDict[iodavar]['qcKey']], np.array(qcall_smoke, dtype=np.int32))

                    self.outdata_dust[self.varDict[iodavar]['valKey']] = np.append(
                        self.outdata_dust[self.varDict[iodavar]['valKey']], np.array(vals_dust, dtype=np.float32))
                    self.outdata_dust[self.varDict[iodavar]['errKey']] = np.append(
                        self.outdata_dust[self.varDict[iodavar]['errKey']], np.array(errs_dust, dtype=np.float32))
                    self.outdata_dust[self.varDict[iodavar]['qcKey']] = np.append(
                        self.outdata_dust[self.varDict[iodavar]['qcKey']], np.array(qcall_dust, dtype=np.int32))

        DimDict['Location'] = len(self.outdata[('latitude', metaDataName)])
        DimDict['Channel'] = np.array(channels)

        if self.adp_mask:
            DimDict_smoke['Location'] = len(self.outdata_smoke[('latitude', metaDataName)])
            DimDict_smoke['Channel'] = np.array(channels)
            DimDict_dust['Location'] = len(self.outdata_dust[('latitude', metaDataName)])
            DimDict_dust['Channel'] = np.array(channels)


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=('Read VIIRS aerosol optical depth file(s) and Converter'
                     ' of native NetCDF format for observations of optical'
                     ' depth from VIIRS AOD550 to IODA-V2 netCDF format.')
    )
    parser.add_argument(
        '-i', '--input',
        help="path of viirs aod input file(s)",
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output',
        help="name of base ioda-v2 output file; smoke and dust files will add '_smoke' or '_dust' suffix",
        type=str, required=True)
    parser.add_argument(
        '-m', '--method',
        help="calculation error method: nesdis/default, default=none",
        type=str, required=True)
    parser.add_argument(
        '-k', '--mask',
        help="maskout missing values: maskout/default, default=none",
        type=str, required=True)
    parser.add_argument(
        '-n', '--thin',
        help="percentage of random thinning fro 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)
    parser.add_argument(
        '--adp_mask',
        help="activate ADP-based separation of smoke and dust affected obs from full AOD dataset",
        action='store_true',
        default=False)
    parser.add_argument(
        '--adp_qc_lvl',
        help="set ADP QC confidence level for smoke and dust obs, use 'low','med', 'medhigh',or 'high': default='medhigh'",
        type=str, required=False)

    args = parser.parse_args()

    # setup the IODA writer
    # Read in the AOD data and perform requested processing
    aod = AOD(args.input, args.method, args.mask, args.thin, args.adp_mask, args.adp_qc_lvl)

    # Write everything out. Use try-except because if all obs are filtered out of
    # one of the datasets due to either masking or thinning or a combination, an IndexError will be thrown
    if not args.adp_mask:
        try:
            print('writing full AOD data')
            writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
            writer.BuildIoda(aod.outdata, VarDims, aod.varAttrs, AttrData)
            print(f"AOD ({DimDict['Location']} obs) written to: {args.output}")
        except IndexError:
            print('viirs_aod2ioda.py: No obs to write for non smoke- and dust-filtered AOD at this time')

    else:
        orig_pn = os.path.dirname(args.output)
        orig_fn = os.path.basename(args.output)
        smoke_fn = orig_fn.split('.')[0] + '_smoke' + '.' + '.'.join(orig_fn.split('.')[1:])
        smoke_fullpath = os.path.join(orig_pn, smoke_fn)
        dust_fn = orig_fn.split('.')[0] + '_dust' + '.' + '.'.join(orig_fn.split('.')[1:])
        dust_fullpath = os.path.join(orig_pn, dust_fn)

        try:
            writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
            writer.BuildIoda(aod.outdata, VarDims, aod.varAttrs, AttrData)
            print(f"AOD ({DimDict['Location']} obs) written to: {args.output}")
        except IndexError:
            print('viirs_aod2ioda.py: No obs to write for full AOD at this time')

        try:
            writer = iconv.IodaWriter(smoke_fullpath, locationKeyList, DimDict_smoke)
            writer.BuildIoda(aod.outdata_smoke, VarDims, aod.varAttrs, AttrData)
            print(f"Smoke AOD ({DimDict_smoke['Location']} obs) written to: {smoke_fullpath}")
        except IndexError:
            print('viirs_aod2ioda.py: No obs to write for smoke-filtered AOD at this time')

        try:
            writer = iconv.IodaWriter(dust_fullpath, locationKeyList, DimDict_dust)
            writer.BuildIoda(aod.outdata_dust, VarDims, aod.varAttrs, AttrData)
            print(f"Dust AOD ({DimDict_dust['Location']} obs) written to: {dust_fullpath}")
        except IndexError:
            print('viirs_aod2ioda.py: No obs to write for dust-filtered AOD at this time')


if __name__ == '__main__':
    main()
