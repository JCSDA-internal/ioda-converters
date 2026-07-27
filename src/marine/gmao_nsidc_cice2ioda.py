#!/usr/bin/env python3

# (C) Copyright 2019-2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import os
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import netCDF4 as nc
import numpy as np
from datetime import datetime

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

# IODA variable and metadata definitions
locationKey = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")
]

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

dtypes = {'integer': np.int32, 'long': np.int64, 'float': np.float32}

class NSIDCobs:
    def __init__(self, filename, gridname):
        self.filename = filename
        self.gridname = gridname
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        ncgrid = nc.Dataset(self.gridname)

        # Read and mask data
        # note: netcdf applies scale factor and offset automatically with [:] access
        cice_2d = ncd.variables['F17_ICECON'][:,:,:].data[0, :, :]
        lon_2d = ncgrid.variables['longitude'][:,:].data
        lat_2d = ncgrid.variables['latitude'][:,:].data

        land_mask = cice_2d == 1200
        pole_hole_mask = cice_2d == 1100
        mask = land_mask | pole_hole_mask

        cice_2d = np.ma.masked_where(mask, cice_2d)
        lon_2d = np.ma.masked_where(mask, lon_2d)
        lat_2d = np.ma.masked_where(mask, lat_2d)

        # Flatten and fill masked values
        cice_1d = cice_2d.filled(float_missing_value).astype(np.float32)
        lon_1d = lon_2d.filled(float_missing_value).astype(np.float32)
        lat_1d = lat_2d.filled(float_missing_value).astype(np.float32)

        nobs = cice_1d.size
        # Convert days since epoch to seconds
        seconds_since_epoch = int(ncd.variables['time'][:].data[0] * 86400)
        dateTime = np.full(nobs, seconds_since_epoch, dtype=np.int64)
        obs_error = np.full(nobs, 0.1, dtype=np.float32)

        self.data = {
            'nobs': nobs,
            'latitude': lat_1d,
            'longitude': lon_1d,
            'dateTime': dateTime,
            'seaIceFraction': cice_1d,
            'seaIceFraction_error': obs_error,
            'seaIceFraction_qc': np.zeros(nobs, dtype=np.int32)
        }
        ncd.close()

class IODA:
    def __init__(self, filename, obs):
        self.filename = filename
        self.obs = obs
        self.write_ioda()

    def write_ioda(self):
        nobs = self.obs.data['nobs']
        data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        data[('latitude', metaDataName)] = self.obs.data['latitude']
        data[('longitude', metaDataName)] = self.obs.data['longitude']
        data[('dateTime', metaDataName)] = self.obs.data['dateTime']
        data[('seaIceFraction', obsValName)] = self.obs.data['seaIceFraction']
        data[('seaIceFraction', obsErrName)] = self.obs.data['seaIceFraction_error']
        data[('seaIceFraction', qcName)] = self.obs.data['seaIceFraction_qc']

        varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        varAttrs[('latitude', metaDataName)]['units'] = 'degrees_north'
        varAttrs[('longitude', metaDataName)]['units'] = 'degrees_east'
        varAttrs[('dateTime', metaDataName)]['units'] = 'seconds since 1970-01-01T00:00:00Z'
        varAttrs[('seaIceFraction', obsValName)]['units'] = '1'
        varAttrs[('seaIceFraction', obsErrName)]['units'] = '1'
        varAttrs[('seaIceFraction', qcName)]['units'] = 'unitless'
        varAttrs[('latitude', metaDataName)]['_FillValue'] = float_missing_value
        varAttrs[('longitude', metaDataName)]['_FillValue'] = float_missing_value
        varAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value
        varAttrs[('seaIceFraction', obsValName)]['_FillValue'] = float_missing_value
        varAttrs[('seaIceFraction', obsErrName)]['_FillValue'] = float_missing_value
        varAttrs[('seaIceFraction', qcName)]['_FillValue'] = int_missing_value

        DimDict = {'Location': nobs}
        varDims = {'seaIceFraction': ['Location']}

        writer = iconv.IodaWriter(self.filename, locationKey, DimDict)
        writer.BuildIoda(data, varDims, varAttrs, {})

def main():
    parser = ArgumentParser(
        description='Convert NSIDC sea-ice concentration data to IODA format.',
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-i', '--input', type=str, required=True, help='Input NSIDC obs file')
    parser.add_argument('-g', '--gridfile', type=str, required=True, help='NSIDC grid file')
    parser.add_argument('-o', '--output', type=str, default='nsidc_ioda_output.nc', help='Output IODA file')
    args = parser.parse_args()

    obs = NSIDCobs(args.input, args.gridfile)
    IODA(args.output, obs)

if __name__ == '__main__':
    main()
