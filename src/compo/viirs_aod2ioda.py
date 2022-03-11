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
    ("dateTime", "integer")
]

obsvars = ["aerosolOpticalDepth"]

# A dictionary of global attributes.  More filled in further down.
AttrData = {}
AttrData['ioda_object_type'] = 'AOD at 550nm'

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {'aerosolOpticalDepth': ['Location']}

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

long_missing_value = nc.default_fillvals['i8']


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
        self.outdata[('latitude', metaDataName)] = []
        self.outdata[('longitude', metaDataName)] = []
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=object)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = []
            self.outdata[self.varDict[iodavar]['errKey']] = []
            self.outdata[self.varDict[iodavar]['qcKey']] = []

        # loop through input filenamess
        for f in self.filenames:
            ncd = nc.Dataset(f, 'r')
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
            sfctyp = 0*qcall
            uncertainty = 0.0*errs
            uncertainty1 = 0.0*errs
            uncertainty2 = 0.0*errs

            if self.method == "nesdis":
                # Case of water high quality
                uncertainty = 0.00784394 + 0.219923*vals
                # case of bright land high quality
                uncertainty1 = 0.0550472 + 0.299558*vals
                # case of dark land high quality
                uncertainty2 = 0.111431 + 0.128699*vals

            for i in range(len(lons)):

                # convert byte to integer
                sfctyp[i] = int.from_bytes(qcpath[i], byteorder='big')
                if self.method == "nesdis":
                    if sfctyp[i] == 1:   # case of bright land high quality
                        uncertainty[i] = uncertainty1[i]
                    else:   # case of dark land high quality
                        uncertainty[i] = uncertainty2[i]
                    errs[i] = uncertainty[i]

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

        DimDict['Location'] = len(self.outdata[('latitude', metaDataName)])


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
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-m', '--method',
        help="calculation error method: nesdis/default, default=none",
        type=str, required=True)
    optional.add_argument(
        '-k', '--mask',
        help="maskout missing values: maskout/default, default=none",
        type=str, required=True)
    optional.add_argument(
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
