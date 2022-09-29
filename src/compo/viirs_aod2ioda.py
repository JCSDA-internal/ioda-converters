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
from datetime import datetime, date, timedelta
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
    ("datetime", "string")
]


obsvars = {
    'A': "aerosol_optical_depth",
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}


DimDict = {
}

VarDims = {
    'aerosol_optical_depth': ['nlocs', 'nchans'],
}


class AOD(object):
    def __init__(self, filenames, method, mask, thin):
        self.filenames = filenames
        self.mask = mask
        self.method = method
        self.thin = thin
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        # set up variable names for IODA
        for ncvar, iodavar in obsvars.items():
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()

            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = '1'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'unitless'
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'

        # loop through input filenamess
        first = True
        for f in self.filenames:
            ncd = nc.Dataset(f)
            gatts = {attr: getattr(ncd, attr) for attr in ncd.ncattrs()}
            base_datetime = gatts["time_coverage_end"]
            self.satellite = gatts["satellite_name"]
            self.sensor = gatts["instrument_name"]
            AttrData["observation_type"] = "AOD"
            AttrData["satellite"] = self.satellite
            AttrData["sensor"] = self.sensor

            if AttrData['sensor'] == 'VIIRS':
                AttrData['sensor'] = "v.viirs-m_npp"
            if AttrData['satellite'] == 'NPP':
                AttrData['satellite'] = "suomi_npp"
            AttrData['date_time_string'] = base_datetime

            lons = ncd.variables['Longitude'][:].ravel()
            lats = ncd.variables['Latitude'][:].ravel()
            vals = ncd.variables['AOD550'][:].ravel()
            errs = ncd.variables['Residual'][:].ravel()
            qcpath = ncd.variables['QCPath'][:].ravel()
            qcall = ncd.variables['QCAll'][:].ravel().astype('int32')
            obs_time = np.empty_like(qcall, dtype=object)
            for t in range(len(obs_time)):
                obs_time[t] = base_datetime
            if self.mask == "maskout":
                mask = np.logical_not(vals.mask)
                vals = vals[mask]
                lons = lons[mask]
                lats = lats[mask]
                errs = errs[mask]
                qcpath = qcpath[mask]
                qcall = qcall[mask]
                obs_time = obs_time[mask]

            ncd.close()

            # apply thinning mask
            if self.thin > 0.0:
                mask_thin = np.random.uniform(size=len(lons)) > self.thin
                lons = lons[mask_thin]
                lats = lats[mask_thin]
                vals = vals[mask_thin]
                errs = errs[mask_thin]
                qcpath = qcpath[mask_thin]
                qcall = qcall[mask_thin]
                obs_time = obs_time[mask_thin]

            # defined surface type and uncertainty
            if self.method == "nesdis":
                errs = 0.00784394 + 0.219923*vals
                errs[qcpath == 1] = 0.0550472 + 0.299558*vals[qcpath == 1]
                errs[qcpath != 1] = 0.111431 + 0.128699*vals[qcpath != 1]

            #  Write out data

            #  Values

            if first:
                self.outdata[self.varDict[iodavar]['valKey']] = vals
                self.outdata[self.varDict[iodavar]['errKey']] = errs
                self.outdata[self.varDict[iodavar]['qcKey']] = qcall
            else:
                self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['valKey']], vals))
                self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['errKey']], errs))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['qcKey']], qcall))

            #  Add Meta data

            if first:
                self.outdata[('latitude', 'MetaData')] = lats
                self.outdata[('longitude', 'MetaData')] = lons
                self.outdata[('datetime', 'MetaData')] = obs_time
            else:
                self.outdata[('latitude', 'MetaData')] = np.concatenate((self.outdata[('latitude', 'MetaData')], lats))
                self.outdata[('longitude', 'MetaData')] = np.concatenate((self.outdata[('longitude', 'MetaData')], lons))
                self.outdata[('datetime', 'MetaData')] = np.concatenate((self.outdata[('datetime', 'MetaData')], obs_time))

            first = False

        DimDict['nlocs'] = len(self.outdata[('latitude', 'MetaData')])
        DimDict['nchans'] = np.array([4])


def main():

    # get command line arguments
    # Usage: python blah.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... -t Analysis_time /path/to/obs/2021060823.nc
    # -o /path/to/ioda/20210608.nc
    # where the input obs could be for any desired interval to concatenated together. Analysis time is generally the midpoint of
    # analysis window.
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
        help="name of ioda-v2 output file",
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

    args = parser.parse_args()

    # setup the IODA writer

    # Read in the AOD data
    aod = AOD(args.input, args.method, args.mask, args.thin)

    # write everything out

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(aod.outdata, VarDims, aod.varAttrs, AttrData)


if __name__ == '__main__':
    main()
