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

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

obsvars = ["reflectance"]
channels = [1,2,3,4,5,6,7,8,9,10,11]
# A dictionary of global attributes.  More filled in further down.
AttrData = {}
AttrData['ioda_object_type'] = 'TOA reflectance (factor)'

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {'toaReflectance': ['Location', 'Channel']}

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

long_missing_value = nc.default_fillvals['i8']


class TOA_R(object):
    def __init__(self, filenames, method, mask, thin):
        self.filenames = filenames
        self.mask = mask
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
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=object)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        # loop through input filenamess
        for obs, geo in self.filenames:
            geo_ncd = nc.Dataset(geo, 'r')
            obs_ncd = nc.Dataset(obs, 'r')
            gatts = {attr: getattr(ncd, attr) for attr in ncd.ncattrs()}
            base_datetime = datetime.strptime(gatts["time_coverage_end"], '%Y-%m-%dT%H:%M:%S.000Z')
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
        DimDict['Channel'] = np.array(channels)


def main():

    # get command line arguments
    # Usage: python blah.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... -t Analysis_time /path/to/obs/2021060823.nc
    # -o /path/to/ioda/20210608.nc
    # where the input obs could be for any desired interval to concatenated together. Analysis time is generally the midpoint of
    # analysis window.
    parser = argparse.ArgumentParser(
        description=('Read NASA VIIRS M-band Level 1b file(s) and Converter'
                     ' of native NetCDF format for observations of TOA reflectance'
                     ' from VIIRS to IODA-V2 netCDF format.')
    )
    parser.add_argument(
        '-i', '--obsinfo',
        help="path of viirs l1b 6-min observation input file(s)",
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-g', '--geoinfo',
        help="path of viirs l1b 6-min geolocation input file(s)",
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output',
        help="name of ioda-v2 output file",
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

    zipped_list = zip(sorted(args.obsinfo), sorted(args.geoinfo))

    # setup the IODA writer

    # Read in the reflectance data
    toa_r = TOA_R(zipped_list, args.method, args.mask, args.thin)

    # write everything out

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(toa_r.outdata, VarDims, toa_r.varAttrs, AttrData)


if __name__ == '__main__':
    main()
