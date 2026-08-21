#!/usr/bin/env python3

#
# (C) Copyright 2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
from datetime import datetime, timedelta, timezone
import os, sys

from pyhdf.HDF import HDF
from pyhdf.VS import VS
from pyhdf.SD import SD, SDC
import numpy as np
import netCDF4 as nc

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["TZ"] = "UTC"

# globals
AttrData = {
    'converter': os.path.basename(__file__),
    "platformCommonName": "CALIPSO",
    "platformLongDescription": "CALIPSO L2 Lidar Data",
}

metaKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", iso8601_string),
    ("pressure", "float", "Pa"),
    ("height", "float", "m"),
    ("atmosphereLayerThicknessZ", "float", "m"),
    ("cloudAerosolDiscriminationHigher", "integer", ""),
    ("cloudAerosolDiscriminationLower", "integer", ""),
    ("sequenceNumber", "integer", ""),
]

DimDict = {
}

# Locations are flattened one-per-(profile,layer) in layer-major order:
# location = layer*n_profiles + profile (0-indexed). "height" is each layer's
# midpoint altitude; combined with atmosphereLayerThicknessZ (top = height +
# thickness/2, bottom = height - thickness/2) that's enough to recover the
# layer's interface bounds without a second stored height field. Channel is
# the CRTM/wavelength channel (532nm=1, 1064nm=2), matching other CRTM
# operators' convention -- it is no longer used for the vertical layer axis.
VarDims = {
    'extinctionCoefficient': ['Location', 'Channel'],
    'pressure': ['Location'],
    'height': ['Location'],
    'atmosphereLayerThicknessZ': ['Location'],
    'cloudAerosolDiscriminationHigher': ['Location'],
    'cloudAerosolDiscriminationLower': ['Location'],
}

obsvars = ["extinctionCoefficient"]
channels = [1, 2]
wavelength = np.array([0.532, 1.064])
speed_light = 2.99792458E8
frequency = speed_light * 1.0E6 / wavelength

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

varsKeyList = [('valKey', obsValName, 'float', 'longitude latitude height', "km-1"),
               ('errKey', obsErrName, 'float', 'longitude latitude height', "km-1"),
               ('qcKey', qcName, 'integer', 'longitude latitude height', None)]

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


class caliop_l2ext(object):
    def __init__(self, filenames, date_range):
        self.filenames = filenames
        wbeg = datetime.strptime(date_range[0], "%Y%m%d%H%M").replace(tzinfo=timezone.utc) - epoch
        wend = datetime.strptime(date_range[1], "%Y%m%d%H%M").replace(tzinfo=timezone.utc) - epoch
        self.wbeg = wbeg.total_seconds()
        self.wend = wend.total_seconds()
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.setDicts()
        self._read()

    def setDicts(self):
        meta_keys = [m_item[0] for m_item in metaKeyList]
        # Set units of the MetaData variables and all _FillValues.
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for key in meta_keys:
            dtypestr = metaKeyList[meta_keys.index(key)][1]
            if metaKeyList[meta_keys.index(key)][2]:
                self.varAttrs[(key, metaDataName)]['units'] = metaKeyList[meta_keys.index(key)][2]
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

    def caliop_time2dt(self, time):
        """
        Convert CALIOP Profile_UTC_Time to datetime.datetime object

        Args:
            time: list or array of Profile_UTC_Time from CALIOP file (float number: yymmdd.ffffffff)
        """
        dtarr = [datetime.strptime(f"{str(t).split('.')[0]:>06}", '%y%m%d') for t in time]
        delta = [timedelta(frac) for frac in np.mod(time, 1)]
        outarr = [(dt + dl).replace(tzinfo=timezone.utc) for dt, dl in zip(dtarr, delta)]
        return outarr

    def _read(self):
        # default missing value in CALIPSO file
        caliop_missing_value = -9999.
        caliop_ref_time = datetime(1993, 1, 1, 0, 0, 0)
        nchan = len(channels)
        output_chidx = np.array(channels, dtype=np.int32) - 1

        # Get the lidar data altitude (per-layer midpoint, shared by every profile)
        tmpfile = self.filenames[0]
        tmphdf = HDF(tmpfile)
        vs = tmphdf.vstart()
        metaid = vs.find('metadata')
        vd = vs.attach(metaid)
        vd.setfields('Lidar_Data_Altitudes')
        height = np.array(vd.read()[0][0]) * 1000.
        nlev = height.size
        vd.detach()
        vs.end()

        # Calculate the thickness of LiDAR profile
        thickness = np.empty_like(height)
        thickness[1:-1] = 0.5 * (height[:-2] - height[2:])
        thickness[0] = height[0] - height[1]
        thickness[-1] = height[-2] - height[-1]

        # Adjust thickness near 20.2 km because it should be around 180m above and 60m below 20.2km
        tmpidx = np.argmin(np.abs(height-20200))
        oldthick = thickness[tmpidx]
        if height[tmpidx] > 20200:
            thickness[tmpidx] = thickness[tmpidx - 1]
            thickness[tmpidx + 1] = thickness[tmpidx + 1] + np.abs(thickness[tmpidx] - oldthick)
        else:
            thickness[tmpidx] = thickness[tmpidx + 1]
            thickness[tmpidx - 1] = thickness[tmpidx - 1] + np.abs(thickness[tmpidx] - oldthick)

        # Accumulate per-profile / per-(profile,layer) data across all input files
        # first -- the profile x layer -> Location flatten below needs the total
        # profile count across every file, not just one file at a time.
        lats_list, lons_list, time_list, seq_list = [], [], [], []
        pres_list, cad1_list, cad2_list = [], [], []
        obs_list, err_list, qcf_list = [], [], []

        prev_nloc = 0
        for f in self.filenames:
            sd = SD(f, SDC.READ)

            pres = sd.select('Pressure').get() * 1e2  # hPa to Pa
            lats = sd.select('Latitude').get()[:, 1]
            lons = sd.select('Longitude').get()[:, 1]
            proftime = sd.select('Profile_UTC_Time').get()[:, 1]
            obs_time = np.array([(pt - epoch).total_seconds() for pt in self.caliop_time2dt(proftime)],
                                dtype=np.int64)

            nloc = lats.size
            profidx = np.arange(prev_nloc, prev_nloc + nloc)

            winmsk = ((obs_time >= self.wbeg) & (obs_time <= self.wend))
            if not any(winmsk):
                print(f"No obs in date range, skip {f}")
                continue

            prev_nloc += nloc

            obs = np.zeros((nloc, nlev, nchan))
            err = np.zeros_like(obs)
            qcf = np.zeros_like(obs)
            for i, chidx in enumerate(output_chidx):
                wavelength_str = str(int(wavelength[chidx] * 1e3))
                obsvarname = f"Extinction_Coefficient_{wavelength_str}"
                errvarname = f"Extinction_Coefficient_Uncertainty_{wavelength_str}"
                qcfvarname = f"Extinction_QC_Flag_{wavelength_str}"
                # Level 2 QC flag stores 30m level 1 QC flag below 8.3 km in the rightmost dimension
                # Based on Young et al. (2018): qc flag value 0, 1, 2, 16, and 18 should be used.
                tmpqcf = sd.select(qcfvarname).get()
                tmpqcf = np.where((tmpqcf[:, :, 0] == tmpqcf[:, :, 1]), tmpqcf[:, :, 0],
                                  np.maximum(tmpqcf[:, :, 0], tmpqcf[:, :, 1]))
                tmpqcf = np.where(np.isin(tmpqcf, [0, 1, 2, 16, 18]), 0, 1)

                obs[:, :, i] = sd.select(obsvarname).get()
                err[:, :, i] = sd.select(errvarname).get()
                qcf[:, :, i] = tmpqcf

            # Similar to QC_Flag, it stores higher and lower 30 meter layers' CAD score
            tmpcad = sd.select("CAD_Score").get()
            cad1 = tmpcad[:, :, 0]
            cad2 = tmpcad[:, :, 1]

            obs = np.where((obs == caliop_missing_value), float_missing_value, obs)
            err = np.where((err == caliop_missing_value), float_missing_value, err)
            pres = np.where(pres < 0, float_missing_value, pres)

            lats_list.append(np.array(lats[winmsk], dtype=np.float32))
            lons_list.append(np.array(lons[winmsk], dtype=np.float32))
            time_list.append(np.array(obs_time[winmsk], dtype=np.int64))
            seq_list.append(np.array(profidx[winmsk], dtype=np.int32))
            pres_list.append(np.array(pres[winmsk, :], dtype=np.float32))
            cad1_list.append(np.array(cad1[winmsk, :], dtype=np.int32))
            cad2_list.append(np.array(cad2[winmsk, :], dtype=np.int32))
            obs_list.append(np.array(obs[winmsk, :, :], dtype=np.float32))
            err_list.append(np.array(err[winmsk, :, :], dtype=np.float32))
            qcf_list.append(np.array(qcf[winmsk, :, :], dtype=np.int32))

            sd.end()

        lats_all = np.concatenate(lats_list)
        lons_all = np.concatenate(lons_list)
        time_all = np.concatenate(time_list)
        seq_all = np.concatenate(seq_list)
        seq_all = seq_all - seq_all.min()
        pres_all = np.concatenate(pres_list, axis=0)   # (n_profiles, nlev)
        cad1_all = np.concatenate(cad1_list, axis=0)   # (n_profiles, nlev)
        cad2_all = np.concatenate(cad2_list, axis=0)   # (n_profiles, nlev)
        obs_all = np.concatenate(obs_list, axis=0)     # (n_profiles, nlev, nchan)
        err_all = np.concatenate(err_list, axis=0)
        qcf_all = np.concatenate(qcf_list, axis=0)

        n_profiles = lats_all.size

        # Flatten (profile, layer) -> Location in layer-major order:
        # location = layer*n_profiles + profile (0-indexed), so locations
        # 0..n_profiles-1 are exactly the n_profiles distinct profiles at
        # layer 1. This lets the Fortran operator read the GeoVaLs/atmosphere
        # data for only those n_profiles locations, running CRTM once per
        # profile rather than once per flattened (profile,layer) row.
        def tile_per_profile(arr):
            # (n_profiles,) -> (nlev*n_profiles,): repeat the whole per-profile
            # array once per layer
            return np.tile(arr, nlev)

        def flatten_profile_layer(arr):
            # (n_profiles, nlev, ...) -> (nlev*n_profiles, ...)
            moved = np.moveaxis(arr, 0, 1)
            return moved.reshape((nlev * n_profiles,) + moved.shape[2:])

        def repeat_per_layer(arr):
            # (nlev,) -> (nlev*n_profiles,): repeat each layer's value once
            # per profile
            return np.repeat(arr, n_profiles)

        self.outdata[('latitude', metaDataName)] = tile_per_profile(lats_all)
        self.outdata[('longitude', metaDataName)] = tile_per_profile(lons_all)
        self.outdata[('dateTime', metaDataName)] = tile_per_profile(time_all)
        self.outdata[('sequenceNumber', metaDataName)] = tile_per_profile(seq_all)
        self.outdata[('pressure', metaDataName)] = flatten_profile_layer(pres_all)
        self.outdata[('cloudAerosolDiscriminationHigher', metaDataName)] = flatten_profile_layer(cad1_all)
        self.outdata[('cloudAerosolDiscriminationLower', metaDataName)] = flatten_profile_layer(cad2_all)
        self.outdata[('height', metaDataName)] = repeat_per_layer(np.array(height, dtype=np.float32))
        self.outdata[('atmosphereLayerThicknessZ', metaDataName)] = repeat_per_layer(np.array(thickness, dtype=np.float32))

        iodavar = "extinctionCoefficient"
        self.outdata[self.varDict[iodavar]['valKey']] = flatten_profile_layer(obs_all)
        self.outdata[self.varDict[iodavar]['errKey']] = flatten_profile_layer(err_all)
        self.outdata[self.varDict[iodavar]['qcKey']] = flatten_profile_layer(qcf_all).astype(np.int32)

        DimDict['Location'] = nlev * n_profiles
        DimDict['Channel'] = np.array(channels, dtype=np.int32)

        min_time = min(time_all)
        max_time = max(time_all)
        AttrData['datetimeRange'] = np.array([datetime.fromtimestamp(min_time, tz=timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                              datetime.fromtimestamp(max_time, tz=timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")], dtype=object)
        print(f"Processed data for datetimeRange: {AttrData['datetimeRange']}")


def main():
    parser = argparse.ArgumentParser(
        description=(
            'Reads the CALIOP Level 2 aerosol profile data '
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of CALIOP APro (HDF4) input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help='path to output ioda file',
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--date_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHHmm YYYYMMDDHHmm",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('197001010000', '217001010000'))
    optional.add_argument(
        '--compression',
        help="gzip compression level for the output file, 0-9; 0 disables "
        "compression (default: %(default)s)",
        type=int, default=1, choices=range(0, 10), metavar='0-9')

    args = parser.parse_args()

    # Read CALIPSO extinction profile data
    caliop_l2 = caliop_l2ext(args.input, args.date_range)

    # write everything out
    writer = iconv.IodaWriter(args.output, metaKeyList, DimDict, complevel=args.compression)
    writer.BuildIoda(caliop_l2.outdata, VarDims, caliop_l2.varAttrs, AttrData)


if __name__ == "__main__":
    main()
