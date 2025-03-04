#!/usr/bin/env python3

#
# (C) Copyright 2025 UCAR
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
    ("sensorCentralFrequency", "float", "Hz"),
    ("sensorCentralWavelength", "float", "micron"),
]

obsvars = ["aerosolOpticalDepth"]
# A dictionary of global attributes.  More filled in further down.
AttrData = {}
AttrData['ioda_object_type'] = 'AOD'

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {
    "aerosolOpticalDepth": ['Location', 'Channel'],
    "sensorCentralFrequency": ['Channel'],
    "sensorCentralWavelength": ['Channel'],
}

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

speed_light = 2.99792458E8


class AOD(object):
    def __init__(self, in_dict):
        self.filenames = in_dict['input']
        self.error_method = in_dict['error_method']
        self.thin = in_dict['thin']
        self.provider = in_dict['provider']
        self.retrieval_method = in_dict['retrieval_method']
        self.wbeg = np.datetime64(str(datetime.strptime(in_dict['date_range'][0], "%Y%m%d%H"))).astype(np.int64)
        self.wend = np.datetime64(str(datetime.strptime(in_dict['date_range'][1], "%Y%m%d%H"))).astype(np.int64)
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.setDicts()
        self.read()

    def setDicts(self):
        meta_keys = [m_item[0] for m_item in locationKeyList]
        # Set units of the MetaData variables and all _FillValues.
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

    def get_platform_sensor_names(self):
        satellite = self.glb_attrs["platform"]
        sensor = self.glb_attrs["instrument"]
        AttrData["platform"] = "oci_pace"
        AttrData["sensor"] = "v.oci_pace"

    def get_s_e_time(self):
        timeformat = '%Y-%m-%dT%H:%M:%S.%fZ'
        this_starttime = datetime.strptime(self.glb_attrs["time_coverage_start"], timeformat)
        this_starttime = this_starttime.replace(tzinfo=timezone.utc)
        self.s_time = round((this_starttime - epoch).total_seconds())

        this_endtime = datetime.strptime(self.glb_attrs["time_coverage_end"], timeformat)
        this_endtime = this_endtime.replace(tzinfo=timezone.utc)
        self.e_time = round((this_endtime - epoch).total_seconds())

    def get_uaa_data(self):
        self.wavelength = np.array([0.354, 0.388, 0.48, 0.55, 0.67, 0.87, 1.24, 1.64, 2.2])
        self.frequency = speed_light * 1.0E6 / self.wavelength
        self.channels = np.arange(self.wavelength.size) + 1

        # PACE UAA retrieval
        self.lons = self.ncd.groups['geolocation_data'].variables['longitude'][:].ravel()
        self.lats = self.ncd.groups['geolocation_data'].variables['latitude'][:].ravel()
        vals = self.ncd.groups['geophysical_data'].variables['Aerosol_Optical_Depth'][:]
        self.vals = vals.reshape(-1, vals.shape[2])
        qcfs = self.ncd.groups['geophysical_data'].variables['Quality_flag_Aerosol_Optical_Depth'][:].ravel()
        self.qcfs = np.repeat(qcfs[:, np.newaxis], self.channels.size, axis=1)

        # Temporarily use expected error (EE) of Dark Target ATBD (March 2024)
        # https://darktarget.gsfc.nasa.gov/sites/default/files/users/user9/ATBD_DarkTarget_April3.pdf
        AttrData['errorMethod'] = 'Expected Error (EE)'
        land_pts = self.ncd.groups['geophysical_data'].variables['Land_Sea_Flag'][:].ravel() == 1
        self.errs = np.zeros_like(self.vals)
        for n in range(self.channels.size):
            self.errs[:, n] = np.where(land_pts, np.add(0.05, np.multiply(0.2, self.vals[:, n])),
                                       np.add(0.05, np.multiply(0.15, self.vals[:, n])))

        if self.error_method == "pue":
            raise Exception("Pixel-level Uncertainty Estimates (PUE) is not ready for PACE UAA")

        # Keep valid data points only
        valid_pts = np.any(~self.vals.mask, axis=1)
        self.lons = self.lons[valid_pts]
        self.lats = self.lats[valid_pts]
        self.vals = self.vals[valid_pts, :]
        self.errs = self.errs[valid_pts, :]
        self.qcfs = self.qcfs[valid_pts, :]

    def read(self):
        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=np.int64)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        # Define get_data function based on retrieval method
        if self.retrieval_method == 'UAA':
            get_paceaod_data = self.get_uaa_data
        AttrData['retrievalMethod'] = self.retrieval_method

        min_time = -int_missing_value
        max_time = int_missing_value

        # loop through input filenamess
        for n, f in enumerate(self.filenames):
            self.ncd = nc.Dataset(f, 'r')
            self.glb_attrs = {attr: getattr(self.ncd, attr) for attr in self.ncd.ncattrs()}

            # Special time consideration. Get min/max of all times being converted for output attribute data.
            # Get the coverage start and end time
            self.get_s_e_time()
            min_time = min(self.s_time, min_time)
            max_time = max(self.e_time, max_time)

            # Get the platform and sensor name
            self.get_platform_sensor_names()

            # Get PACE OCI data
            get_paceaod_data()

            # assign the observation time based on time coverage
            self.obs_time = np.full(np.shape(self.lons), round(0.5*(self.s_time + self.e_time)), dtype=np.int64)
            winmsk = ((self.obs_time >= self.wbeg) & (self.obs_time <= self.wend))

            # apply thinning mask
            if self.thin > 0.0:
                mask_thin = np.random.uniform(size=len(self.lons)) > self.thin
                self.lons = self.lons[mask_thin]
                self.lats = self.lats[mask_thin]
                self.vals = self.vals[mask_thin]
                self.errs = self.errs[mask_thin]
                self.qcfs = self.qcfs[mask_thin]
                self.obs_time = self.obs_time[mask_thin]

            #  Write out data
            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)], np.array(self.lats[winmsk], dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)], np.array(self.lons[winmsk], dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)], np.array(self.obs_time[winmsk], dtype=np.int64))

            for iodavar in obsvars:
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['valKey']], np.array(self.vals[winmsk, :], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['errKey']], np.array(self.errs[winmsk, :], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['qcKey']], np.array(self.qcfs[winmsk, :], dtype=np.int32))

            self.ncd.close()

        self.outdata[('sensorCentralWavelength', metaDataName)] = self.wavelength.astype(np.float32)
        self.outdata[('sensorCentralFrequency', metaDataName)] = self.frequency.astype(np.float32)
        AttrData['datetimeRange'] = np.array([datetime.fromtimestamp(min_time).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                              datetime.fromtimestamp(max_time).strftime("%Y-%m-%dT%H:%M:%SZ")], dtype=object)
        print(f"Processed data for datetimeRange: {AttrData['datetimeRange']}")

        DimDict['Location'] = len(self.outdata[('latitude', metaDataName)])
        DimDict['Channel'] = np.array(self.channels)


def main():

    # get command line arguments
    # Usage: python pace_aod2ioda.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... -o /path/to/ioda/20210608.nc
    # --retieval_method [UAA] --error_method [pue]
    # where the input obs could be for any desired interval to concatenated together.
    parser = argparse.ArgumentParser(
        description=('Read PACE OCI aerosol optical depth file(s) and Converter'
                     ' of native NetCDF format for observations of optical'
                     ' depth from PACE OCI AOD to IODA-V2 netCDF format.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of pace aod input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="name of ioda-v2 output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--retrieval_method',
        help="specify the retrieval method when provider is nasa, UAA",
        type=str, default=None)
    optional.add_argument(
        '--error_method',
        help="calculation error method: pue/default, Expected Error for NASA product",
        type=str, default=None)
    optional.add_argument(
        '-n', '--thin',
        help="percentage of random thinning fro 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)
    optional.add_argument(
        '--date_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHH YYYYMMDDHH",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    args = parser.parse_args()

    args_in_dict = {'input': args.input,
                    'error_method': args.error_method,
                    'provider': args.provider,
                    'retrieval_method': args.retrieval_method,
                    'thin': args.thin,
                    'date_range': args.date_range,
                    }

    # setup the IODA writer

    # Read in the AOD data
    aod = AOD(args_in_dict)

    # write everything out

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(aod.outdata, VarDims, aod.varAttrs, AttrData)


if __name__ == '__main__':
    main()
