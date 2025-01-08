#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
from datetime import datetime, timezone
import netCDF4 as nc
import numpy as np
import os

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["TZ"] = "UTC"

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", iso8601_string),
]

obsvars = ["aerosolOpticalDepth"]
channels = [4]
# A dictionary of global attributes.  More filled in further down.
AttrData = {}
AttrData['ioda_object_type'] = 'AOD'

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {'aerosolOpticalDepth': ['Location', 'Channel']}

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()

varsKeyList = [('valKey', iconv.OvalName(), 'float', 'longitude latitude', '1'),
               ('errKey', iconv.OerrName(), 'float', 'longitude latitude', '1'),
               ('qcKey', iconv.OqcName(), 'integer', 'longitude latitude', None)]

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


class AOD(object):
    def __init__(self, in_dict):
        self.filenames = in_dict['input']
        self.mask_missing = in_dict['mask_missing']
        self.error_method = in_dict['error_method']
        self.thin = in_dict['thin']
        self.provider = in_dict['provider']
        self.retrieval_method = in_dict['retrieval_method']
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.setDicts()
        print(self.varDict)
        print(self.varAttrs)
        self.read()

        DimDict['Location'] = len(self.outdata[('latitude', metaDataName)])
        DimDict['Channel'] = np.array(channels)

    def setDicts(self):
        meta_keys = [m_item[0] for m_item in locationKeyList]
        # Set units of the MetaData variables and all _FillValues.
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            if locationKeyList[meta_keys.index(key)][2]:
                self.varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
            self.varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]

        var_keys = [v_item[0] for v_item in varsKeyList]
        # set up variable names for IODA
        for iodavar in obsvars:
            for key in var_keys:
                varGroupName = varsKeyList[var_keys.index(key)][1]
                dtypestr = varsKeyList[var_keys.index(key)][2]
                coord = varsKeyList[var_keys.index(key)][3]
                self.varDict[iodavar][key] = iodavar, varGroupName
                self.varAttrs[iodavar, varGroupName]['coordinates'] = coord
                self.varAttrs[iodavar, varGroupName]['_FillValue'] = missing_vals[dtypestr]
                if varsKeyList[var_keys.index(key)][4]:
                    self.varAttrs[iodavar, varGroupName]['units'] = varsKeyList[var_keys.index(key)][4]

    def _read_nasa_dt(self):
        print(f'Testing')

    def _read_nasa_db(self):
        print(f'Testing')

    def _read_noaa(self):
        min_time = -int_missing_value
        max_time = int_missing_value
        # loop through input filenamess
        for f in self.filenames:
            ncd = nc.Dataset(f, 'r')
            gatts = {attr: getattr(ncd, attr) for attr in ncd.ncattrs()}

            # Special time consideration. Get min/max of all times being converted for output attribute data.
            this_starttime = datetime.strptime(gatts["time_coverage_start"], '%Y-%m-%dT%H:%M:%SZ')
            this_starttime = this_starttime.replace(tzinfo=timezone.utc)
            s_time = round((this_starttime - epoch).total_seconds())
            this_endtime = datetime.strptime(gatts["time_coverage_end"], '%Y-%m-%dT%H:%M:%SZ')
            this_endtime = this_endtime.replace(tzinfo=timezone.utc)
            e_time = round((this_endtime - epoch).total_seconds())
            min_time = min(s_time, min_time)
            max_time = max(e_time, max_time)

            self.satellite = gatts["satellite_name"]
            self.sensor = gatts["instrument_name"]
            AttrData["platform"] = self.satellite
            AttrData["sensor"] = self.sensor

            if AttrData['sensor'] == 'VIIRS':
                AttrData['sensor'] = "v.viirs-m_npp"
            if AttrData['platform'] == 'NPP':
                AttrData['platform'] = "suomi_npp"

            self.lons = ncd.variables['Longitude'][:].ravel()
            self.lats = ncd.variables['Latitude'][:].ravel()
            self.vals = ncd.variables['AOD550'][:].ravel()
            self.errs = ncd.variables['Residual'][:].ravel()

            # QCPath is the flag for retrieval path. The valid range is 0-127 in the
            # ATBD: https://www.star.nesdis.noaa.gov/jpss/documents/ATBD/ATBD_EPS_Aerosol_AOD_v3.4.pdf.
            # QCPath's valid range in the input file is not correct, so we define the valid range here.
            qcpath = ncd.variables['QCPath'][:].data.ravel()
            qcpath = np.ma.masked_array(qcpath, np.logical_or(qcpath < 0, qcpath > 127))

            self.qcall = ncd.variables['QCAll'][:].ravel().astype('int32')
            self.obs_time = np.full(np.shape(self.lons), round(0.5*(s_time+e_time)), dtype=np.int64)

            if self.mask_missing:
                mask = np.logical_not(self.vals.mask)
                self.vals = self.vals[mask]
                self.lons = self.lons[mask]
                self.lats = self.lats[mask]
                self.errs = self.errs[mask]
                qcpath = qcpath[mask]
                self.qcall = self.qcall[mask]
                self.obs_time = self.obs_time[mask]

            ncd.close()

            # apply thinning mask
            if self.thin > 0.0:
                mask_thin = np.random.uniform(size=len(lons)) > self.thin
                self.lons = self.lons[mask_thin]
                self.lats = self.lats[mask_thin]
                self.vals = self.vals[mask_thin]
                self.errs = self.errs[mask_thin]
                qcpath = qcpath[mask_thin]
                self.qcall = self.qcall[mask_thin]
                self.obs_time = self.obs_time[mask_thin]

            # defined surface type and uncertainty
            if self.error_method == "calval":
                self.errs = 0.111431 + 0.128699 * self.vals    # over land (dark)
                self.errs[qcpath % 2 == 1] = 0.00784394 + 0.219923 * self.vals[qcpath % 2 == 1]  # over ocean
                self.errs[qcpath % 4 == 2] = 0.0550472 + 0.299558 * self.vals[qcpath % 4 == 2]   # over bright land

            self._append_outdata()

        AttrData['datetimeRange'] = np.array([datetime.fromtimestamp(min_time).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                              datetime.fromtimestamp(max_time).strftime("%Y-%m-%dT%H:%M:%SZ")], dtype=object)
        print(f"Processed data for datetimeRange: {AttrData['datetimeRange']}")


    def _append_outdata(self):
        #  Write out data
        self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)], np.array(self.lats, dtype=np.float32))
        self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)], np.array(self.lons, dtype=np.float32))
        self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)], np.array(self.obs_time, dtype=np.int64))

        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.append(
                self.outdata[self.varDict[iodavar]['valKey']], np.array(self.vals, dtype=np.float32))
            self.outdata[self.varDict[iodavar]['errKey']] = np.append(
                self.outdata[self.varDict[iodavar]['errKey']], np.array(self.errs, dtype=np.float32))
            self.outdata[self.varDict[iodavar]['qcKey']] = np.append(
                self.outdata[self.varDict[iodavar]['qcKey']], np.array(self.qcall, dtype=np.int32))

    def read(self):
        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=np.int64)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        if self.provider == 'nasa':
            if self.retrieval_method == 'DarkTarget':
                self._read_nasa_dt()
            if self.retrieval_method == 'DeepBlue':
                self._read_nasa_db()
        elif self.provider == 'noaa':
            self._read_noaa()



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
        '--error_method',
        help="calculation error method: calval/default, default=none",
        type=str, required=True)
    parser.add_argument(
        '--mask_missing',
        help="maskout missing values, default=False",
        action='store_true', default=False)
    parser.add_argument(
        '--provider',
        help="data source, noaa/nasa",
        type=str, required=True)
    parser.add_argument(
        '--retrieval_method',
        help="specify the retrieval method when provider is nasa, DarkTarget/DeepBlue",
        type=str, default=None)
    parser.add_argument(
        '-n', '--thin',
        help="percentage of random thinning fro 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    args = parser.parse_args()

    args_in_dict = {'input': args.input,
            'error_method': args.error_method,
            'mask_missing': args.mask_missing,
            'provider': args.provider,
            'retrieval_method': args.retrieval_method,
            'thin': args.thin,
            }

    # setup the IODA writer

    # Read in the AOD data
    aod = AOD(args_in_dict)

    # write everything out

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(aod.outdata, VarDims, aod.varAttrs, AttrData)


if __name__ == '__main__':
    main()
