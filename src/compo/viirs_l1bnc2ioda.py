#!/usr/bin/env python3

#
# (C) Copyright 2024 UCAR
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
from pyiodaconv.def_jedi_utils import compute_scan_angle
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["TZ"] = "UTC"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
    ("satelliteAscendingFlag", "integer"),
    ("solarZenithAngle", "float"),
    ("solarAzimuthAngle", "float"),
    ("sensorZenithAngle", "float"),
    ("sensorAzimuthAngle", "float"),
    ("sensorViewAngle", "float"),
    ("sensorScanPosition", "integer"),
    ("surfaceQualifier", "integer"),
]

obsvars = ["albedo"]
channels = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

# VIIRS M-band 11 reflective channels central wavelength. Do not change list below.
wavelength = [0.412, 0.445, 0.488, 0.555, 0.672, 0.746,
              0.865, 1.24, 1.378, 1.61, 2.25]
orbit_height = 829000.       # in meters
speed_light = 2.99792458E8
frequency = speed_light*1.0E6/np.array(wavelength)

# A dictionary of global attributes.  More filled in further down.
AttrData = {
    'converter': os.path.basename(__file__),
    'description': 'VIIRS L1b M-band visible albedo',
    'source': 'NASA, ladsweb.modaps.eosdis.nasa.gov',
    'sourceFiles': ''
}

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {'albedo': ['Location', 'Channel'],
           'satelliteAscendingFlag': ['Location'],
           'sensorCentralFrequency': ['Channel'],
           'sensorCentralWavelength': ['Channel'],
           'sensorChannelNumber': ['Channel'],
           'sensorScanPosition': ['Location'],
           'sensorViewAngle': ['Location'],
           }

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

double_missing_value = iconv.get_default_fill_val(np.float64)
float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)


class viirs_l1b_rf(object):
    def __init__(self, filenames, date_range, thinning_ratio):
        self.filenames = filenames
        self.wbeg = np.datetime64(str(datetime.strptime(date_range[0], "%Y%m%d%H"))).astype(np.int64)
        self.wend = np.datetime64(str(datetime.strptime(date_range[1], "%Y%m%d%H"))).astype(np.int64)
        self.thinning_ratio = thinning_ratio
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
            self.varAttrs[iodavar, obsValName]['long_name'] = 'Albedo at top-of-atmosphere'

        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=np.int64)
        self.outdata[('satelliteAscendingFlag', metaDataName)] = np.array([], dtype=np.int32)
        self.outdata[('solarZenithAngle', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('solarAzimuthAngle', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('sensorZenithAngle', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('sensorAzimuthAngle', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('sensorScanPosition', metaDataName)] = np.array([], dtype=np.int32)
        self.outdata[('sensorViewAngle', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('surfaceQualifier', metaDataName)] = np.array([], dtype=np.int32)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        min_time = -int_missing_value
        max_time = int_missing_value
        # loop through input filenames
        for obs, geo in self.filenames:
            geo_ncd = nc.Dataset(geo, 'r')
            obs_ncd = nc.Dataset(obs, 'r')
            gatts = {attr: getattr(obs_ncd, attr) for attr in obs_ncd.ncattrs()}
            if gatts['DayNightFlag'] != "Day":
                continue
            self.satellite = gatts["platform"]
            self.sensor = gatts["instrument"]
            self.satellite_instrument = gatts["SatelliteInstrument"]
            self.ascend_or_descend = gatts["startDirection"]
            # Special time consideration. Get min/max of all times being converted for output attribute data.
            this_starttime = datetime.strptime(gatts["time_coverage_start"], '%Y-%m-%dT%H:%M:%S.000Z')
            this_starttime = this_starttime.replace(tzinfo=timezone.utc)
            s_time = round((this_starttime - epoch).total_seconds())
            this_endtime = datetime.strptime(gatts["time_coverage_end"], '%Y-%m-%dT%H:%M:%S.000Z')
            this_endtime = this_endtime.replace(tzinfo=timezone.utc)
            e_time = round((this_endtime - epoch).total_seconds())
            min_time = min(s_time, min_time)
            max_time = max(e_time, max_time)

            AttrData['sourceFiles'] += str(obs.split('/')[-1]) + ", "
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
            sensor_va = compute_scan_angle(np.full_like(sensor_za, orbit_height), sensor_za)
            landwat_mask = geo_ncd.groups['geolocation_data'].variables['land_water_mask'][:].data.ravel()

            nlocs = lons.size

            obsgrp = obs_ncd.groups['observation_data']

            vals = np.zeros((nlocs, len(channels)), dtype=np.float32)
            errs = np.zeros_like(vals)
            qcfs = np.zeros(np.shape(vals), dtype=np.int32)
            orbit_ad = np.zeros_like(qcfs)
            if self.ascend_or_descend == "Ascending":
                orbit_ad[:] = 1
            self.varAttrs['satelliteAscendingFlag', metaDataName]['description'] = '0=descending, 1=ascending'

            # Calculate cosine of solar zenith angle
            cos_za = np.cos(solar_za * np.pi / 180.)

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

                # Setup observation error:
                # 1. Convert Uncertainty Index to uncertainty in percentage, which is
                #    described in section 2.5 in the L1B User Guide v3.0 available on
                #    https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/science-domain/viirs-L0-L1/
                # 2. Multiplying obs value to get absolute error
                # 3. Apply the cosine of solar zenith angle to obs error instead of obs and hofx
                errs[:, ichan] = (1. + err.scale_factor * err[:].data.ravel() ** 2) * 0.01 * vals[:, ichan] * cos_za

                # Apply _FillValue to masked points
                vals[obs_mask, ichan] = float_missing_value
                qcfs[obs_mask, ichan] = int_missing_value
                errs[obs_mask, ichan] = float_missing_value

                ichan += 1

            # Apply middle of start and end time of scan to all of this specific scan.
            obs_time = np.full(np.shape(lons), round(0.5*(s_time+e_time)), dtype=np.int64)

            geo_ncd.close()
            obs_ncd.close()

            # apply thinning mask
            if self.thinning_ratio > 0.0:
                mask_thin = np.random.uniform(size=len(lons)) > self.thinning_ratio
                lons = lons[mask_thin]
                lats = lats[mask_thin]
                vals = vals[mask_thin, :]
                errs = errs[mask_thin, :]
                qcfs = qcfs[mask_thin, :]
                obs_time = obs_time[mask_thin]
                orbit_ad = orbit_ad[mask_thin]
                solar_za = solar_za[mask_thin]
                solar_aa = solar_aa[mask_thin]
                sensor_za = sensor_za[mask_thin]
                sensor_aa = sensor_aa[mask_thin]
                sensor_va = sensor_va[mask_thin]
                landwat_mask = landwat_mask[mask_thin]

            winmsk = ((obs_time >= self.wbeg) & (obs_time <= self.wend))

            #  Append the data to prepare for output
            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)], np.array(lats[winmsk], dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)], np.array(lons[winmsk], dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)], np.array(obs_time[winmsk], dtype=np.int64))

            self.outdata[('satelliteAscendingFlag', metaDataName)] = np.append(self.outdata[('satelliteAscendingFlag', metaDataName)],
                                                                               np.array(orbit_ad[winmsk], dtype=np.int32))
            self.outdata[('solarZenithAngle', metaDataName)] = np.append(self.outdata[('solarZenithAngle', metaDataName)],
                                                                         np.array(solar_za[winmsk], dtype=np.float32))
            self.outdata[('solarAzimuthAngle', metaDataName)] = np.append(self.outdata[('solarAzimuthAngle', metaDataName)],
                                                                          np.array(solar_aa[winmsk], dtype=np.float32))
            self.outdata[('sensorZenithAngle', metaDataName)] = np.append(self.outdata[('sensorZenithAngle', metaDataName)],
                                                                          np.array(sensor_za[winmsk], dtype=np.float32))
            self.outdata[('sensorAzimuthAngle', metaDataName)] = np.append(self.outdata[('sensorAzimuthAngle', metaDataName)],
                                                                           np.array(sensor_aa[winmsk], dtype=np.float32))
            self.outdata[('sensorViewAngle', metaDataName)] = np.append(self.outdata[('sensorViewAngle', metaDataName)],
                                                                        np.array(sensor_va[winmsk], dtype=np.float32))
            self.outdata[('surfaceQualifier', metaDataName)] = np.append(self.outdata[('surfaceQualifier', metaDataName)],
                                                                         np.array(landwat_mask[winmsk], dtype=np.int32))

            for iodavar in obsvars:
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['valKey']], np.array(vals[winmsk, :], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['errKey']], np.array(errs[winmsk, :], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['qcKey']], np.array(qcfs[winmsk, :], dtype=np.int32))

        # Write other MetaData
        self.varAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
        output_chidx = np.array(channels, dtype=np.int32) - 1
        self.outdata[('sensorCentralFrequency', metaDataName)] = np.array(frequency[output_chidx], dtype=np.float32)
        self.varAttrs[('sensorCentralFrequency', metaDataName)]['units'] = 'Hz'
        self.varAttrs[('sensorCentralFrequency', metaDataName)]['_FillValue'] = float_missing_value
        self.outdata[('sensorCentralWavelength', metaDataName)] = np.array(wavelength, dtype=np.float32)[output_chidx]
        self.varAttrs[('sensorCentralWavelength', metaDataName)]['units'] = 'micron'
        self.outdata[('sensorChannelNumber', metaDataName)] = np.array(channels, dtype=np.int32)
        self.outdata[('sensorScanPosition', metaDataName)] = np.ones_like(self.outdata[('latitude', metaDataName)], dtype=np.int32)

        for tmpvar in ['solarZenithAngle', 'solarAzimuthAngle', 'sensorZenithAngle', 'sensorAzimuthAngle', 'sensorViewAngle']:
            self.varAttrs[(tmpvar, metaDataName)]['_FillValue'] = float_missing_value
            self.varAttrs[(tmpvar, metaDataName)]['units'] = 'degrees'

        DimDict['Location'] = len(self.outdata[('latitude', metaDataName)])
        DimDict['Channel'] = self.outdata[('sensorChannelNumber', metaDataName)]
        AttrData['sourceFiles'] = AttrData['sourceFiles']
        AttrData['datetimeRange'] = np.array([datetime.fromtimestamp(min_time).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                              datetime.fromtimestamp(max_time).strftime("%Y-%m-%dT%H:%M:%SZ")], dtype=object)
        print(f"Processed data for datetimeRange: {AttrData['datetimeRange']}")


def main():

    # get command line arguments
    # Usage: python viirs_l1bnc2ioda.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ...
    #                                   -g /path/to/geo/2021060801.nc /path/to/geo/2021060802.nc ...
    #                                   -o /path/to/ioda/20210608.nc
    # where the input obs could be for any desired interval to concatenated together.
    parser = argparse.ArgumentParser(
        description=('Read NASA VIIRS M-band Level 1b file(s) from:'
                     'Obs data: https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5201/VJ102MOD/'
                     'Geo data: https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5201/VJ103MOD/'
                     'and converter of native NetCDF format for observations of TOA reflectance during'
                     ' daytime from VIIRS to IODA NetCDF format.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--obsinfo',
        help="path of viirs l1b 6-min observation (VJ102MOD) input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-g', '--geoinfo',
        help="path of viirs l1b 6-min geolocation (VJ103MOD) input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="name of ioda-v2 output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--thinning_ratio',
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

    zipped_list = zip(sorted(args.obsinfo), sorted(args.geoinfo))

    # Read in the reflectance factor data
    toa_rf = viirs_l1b_rf(zipped_list, args.date_range, args.thinning_ratio)

    # write everything out (albedo)
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(toa_rf.outdata, VarDims, toa_rf.varAttrs, AttrData)


if __name__ == '__main__':
    main()
