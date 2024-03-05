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
    ("dateTime", "long"),
    ("solarZenithAngle", "float"),
    ("solarAzimuthAngle", "float"),
    ("sensorZenithAngle", "float"),
    ("sensorAzimuthAngle", "float"),
]

obsvars = ["albedo"]
channels = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

# VIIRS M-band 11 reflective channels centeral wavelength
wavelength = [0.412, 0.445, 0.488, 0.555, 0.672, 0.746,
              0.865, 1.24, 1.378, 1.61, 2.25]
speed_light = 2.99792458E8
frequency = speed_light*1.0E6/np.array(wavelength, dtype=np.float32)

# A dictionary of global attributes.  More filled in further down.
AttrData = {}
AttrData['ioda_object_type'] = 'TOA reflectance factor'

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {'albedo': ['Location', 'Channel'],
           'sensorCentralFrequency': ['Channel'],
           'sensorCentralWavelength': ['Channel'],
           'sensorChannelNumber': ['Channel'],
           }

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)


class viirs_l1b_rf(object):
    def __init__(self, filenames, thin):
        self.filenames = filenames
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
            self.varAttrs[iodavar, obsValName]['_FillValue'] = float_missing_value
            self.varAttrs[iodavar, obsErrName]['_FillValue'] = float_missing_value
            self.varAttrs[iodavar, qcName]['_FillValue'] = int_missing_value
            self.varAttrs[iodavar, obsValName]['units'] = '1'
            self.varAttrs[iodavar, obsErrName]['units'] = '1'
            self.varAttrs[iodavar, obsValName]['long_name'] = 'TOA reflectance factor'
            self.varAttrs[iodavar, obsErrName]['long_name'] = 'TOA reflectance factor'
            self.varAttrs[iodavar, qcName]['long_name'] = 'TOA reflectance factor'

        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=object)
        self.outdata[('solarZenithAngle', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('solarAzimuthAngle', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('sensorZenithAngle', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('sensorAzimuthAngle', metaDataName)] = np.array([], dtype=np.float32)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        # loop through input filenamess
        for obs, geo in self.filenames:
            geo_ncd = nc.Dataset(geo, 'r')
            obs_ncd = nc.Dataset(obs, 'r')
            gatts = {attr: getattr(obs_ncd, attr) for attr in obs_ncd.ncattrs()}
            base_datetime = datetime.strptime(gatts["time_coverage_end"], '%Y-%m-%dT%H:%M:%S.000Z')
            self.satellite = gatts["platform"]
            self.sensor = gatts["instrument"]
            self.satellite_instrument = gatts["SatelliteInstrument"]

            if self.satellite_instrument == 'JP1VIIRS':
                AttrData['sensor'] = "v.viirs-m_n20"
            if self.satellite == 'NPP':
                AttrData['platform'] = "suomi_npp"

            lons = geo_ncd.groups['geolocation_data'].variables['longitude'][:].data.ravel()
            lats = geo_ncd.groups['geolocation_data'].variables['latitude'][:].data.ravel()
            solar_za = geo_ncd.groups['geolocation_data'].variables['solar_zenith'][:].data.ravel()
            solar_aa = geo_ncd.groups['geolocation_data'].variables['solar_azimuth'][:].data.ravel()
            sensor_za = geo_ncd.groups['geolocation_data'].variables['sensor_zenith'][:].data.ravel()
            sensor_aa = geo_ncd.groups['geolocation_data'].variables['sensor_azimuth'][:].data.ravel()
        
            nlocs = lons.size

            obsgrp = obs_ncd.groups['observation_data']

            vals = np.zeros((nlocs, len(channels)), dtype=np.float32)
            errs = np.zeros_like(vals)
            qcfs = np.zeros(np.shape(vals), dtype=np.int32)

            ichan = 0
            for chan in channels:
                obsname = 'M%2.2i' % (chan)
                qcfname = '%s_quality_flags' % (obsname)
                errname = '%s_uncert_index' % (obsname)

                obs = obsgrp.variables[obsname]
                obs_mask = obs[:].mask.ravel()
                qcf = obsgrp.variables[qcfname]
                err = obsgrp.variables[errname]
                err.set_auto_scale(False)

                vals[:, ichan] = obs[:].data.ravel()
                qcfs[:, ichan] = qcf[:].data.ravel()
                errs[:, ichan] = 1. + err.scale_factor * err[:].data.ravel() ** 2

                # Apply _FillValue to masked points
                vals[obs_mask, ichan] = float_missing_value

                ichan += 1

            # Apply base_datetime to whole obs_time array,
            # may need to derive from scan_start_time, scan_end_time
            obs_time = np.full(np.shape(lons), base_datetime, dtype=object)

            geo_ncd.close()
            obs_ncd.close()

            # apply thinning mask
            if self.thin > 0.0:
                mask_thin = np.random.uniform(size=len(lons)) > self.thin
                lons = lons[mask_thin]
                lats = lats[mask_thin]
                vals = vals[mask_thin, :]
                errs = errs[mask_thin, :]
                qcfs = qcfs[mask_thin, :]
                obs_time = obs_time[mask_thin]

            #  Write out data
            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)], np.array(lats, dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)], np.array(lons, dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)], np.array(obs_time, dtype=object))

            self.outdata[('solarZenithAngle', metaDataName)] = np.append(self.outdata[('solarZenithAngle', metaDataName)],
                                                                         np.array(solar_za, dtype=np.float32))
            self.outdata[('solarAzimuthAngle', metaDataName)] = np.append(self.outdata[('solarAzimuthAngle', metaDataName)],
                                                                          np.array(solar_aa, dtype=np.float32))
            self.outdata[('sensorZenithAngle', metaDataName)] = np.append(self.outdata[('sensorZenithAngle', metaDataName)],
                                                                          np.array(sensor_za, dtype=np.float32))
            self.outdata[('sensorAzimuthAngle', metaDataName)] = np.append(self.outdata[('sensorAzimuthAngle', metaDataName)],
                                                                           np.array(sensor_aa, dtype=np.float32))

            for iodavar in obsvars:
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['valKey']], np.array(vals, dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['errKey']], np.array(errs, dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['qcKey']], np.array(qcfs, dtype=np.int32))

        # Write other MetaData
        output_chidx = np.array(channels, dtype=np.int32) - 1
        self.outdata[('sensorCentralFrequency', metaDataName)] = np.array(frequency[output_chidx], dtype=np.float32)
        self.varAttrs[('sensorCentralFrequency', metaDataName)]['units'] = 'Hz'
        self.outdata[('sensorCentralWavelength', metaDataName)] = np.array(frequency[output_chidx], dtype=np.float32)
        self.varAttrs[('sensorCentralWavelength', metaDataName)]['units'] = 'micron'
        self.outdata[('sensorChannelNumber', metaDataName)] = np.array(channels, dtype=np.int32)

        for tmpvar in ['solarZenithAngle', 'solarAzimuthAngle', 'sensorZenithAngle', 'sensorAzimuthAngle']:
            self.varAttrs[(tmpvar, metaDataName)]['_FillValue'] = float_missing_value
            self.varAttrs[(tmpvar, metaDataName)]['units'] = 'degrees'

        DimDict['Location'] = len(self.outdata[('latitude', metaDataName)])
        AttrData['Location'] = np.int32(DimDict['Location'])
        DimDict['Channel'] = len(channels)
        AttrData['Channel'] = np.int32(DimDict['Channel'])


def main():

    # get command line arguments
    # Usage: python blah.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... -t Analysis_time /path/to/obs/2021060823.nc
    # -o /path/to/ioda/20210608.nc
    # where the input obs could be for any desired interval to concatenated together. Analysis time is generally the midpoint of
    # analysis window.
    parser = argparse.ArgumentParser(
        description=('Read NASA VIIRS M-band Level 1b file(s) and Converter'
                     ' of native NetCDF format for observations of TOA reflectance'
                     ' from VIIRS to IODA NetCDF format.')
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
        '-n', '--thin',
        help="percentage of random thinning fro 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    args = parser.parse_args()

    zipped_list = zip(sorted(args.obsinfo), sorted(args.geoinfo))

    # setup the IODA writer

    # Read in the reflectance factor data
    toa_rf = viirs_l1b_rf(zipped_list, args.thin)

    # write everything out (albedo)
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(toa_rf.outdata, VarDims, toa_rf.varAttrs, AttrData)


if __name__ == '__main__':
    main()
