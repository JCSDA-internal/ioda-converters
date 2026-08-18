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
    ("heightVertice", "float", "m"),
    ("atmosphereLayerThicknessZ", "float", "m"),
    ("cloudAerosolDiscriminationHigher", "integer", ""),
    ("cloudAerosolDiscriminationLower", "integer", ""),
    ("sequenceNumber", "integer", ""),
]

DimDict = {
}

VarDims = {
    'extinctionCoefficient_532nm': ['Location', 'Channel'],
    'extinctionCoefficient_1064nm': ['Location', 'Channel'],
    'pressure': ['Location', 'Channel'],
    'height': ['Channel'],
    'heightVertice': ['Vertice'],
    'atmosphereLayerThicknessZ': ['Channel'],
    'cloudAerosolDiscriminationHigher': ['Location', 'Channel'],
    'cloudAerosolDiscriminationLower': ['Location', 'Channel'],
}

obsvars = ["extinctionCoefficient_532nm", "extinctionCoefficient_1064nm"]
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

        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=np.int64)
        self.outdata[('pressure', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('sequenceNumber', metaDataName)] = np.array([], dtype=np.int32)
        self.outdata[('cloudAerosolDiscriminationHigher', metaDataName)] = np.array([], dtype=np.int32)
        self.outdata[('cloudAerosolDiscriminationLower', metaDataName)] = np.array([], dtype=np.int32)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        # Get the lidar data altitude
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

        # Height at interface
        iheight = np.zeros(nlev+1)
        iheight[-1] = height[-1] - 0.5 * thickness[-1]
        for k in reversed(range(nlev)):
            iheight[k] = iheight[k+1] + thickness[k]

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
            cad1 = np.zeros_like(pres)
            cad2 = np.zeros_like(pres)
            tmpcad = sd.select("CAD_Score").get()
            cad1 = tmpcad[:, :, 0]
            cad2 = tmpcad[:, :, 1]

            obs = np.where((obs == caliop_missing_value), float_missing_value, obs)
            err = np.where((err == caliop_missing_value), float_missing_value, err)
            pres = np.where(pres < 0, float_missing_value, pres)

            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)],
                                                                 np.array(lats[winmsk], dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)],
                                                                  np.array(lons[winmsk], dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)],
                                                                 np.array(obs_time[winmsk], dtype=np.int64))
            self.outdata[('pressure', metaDataName)] = np.append(self.outdata[('pressure', metaDataName)],
                                                                 np.array(pres[winmsk], dtype=np.float32))
            self.outdata[('sequenceNumber', metaDataName)] = np.append(self.outdata[('sequenceNumber', metaDataName)],
                                                                       np.array(profidx[winmsk], dtype=np.int32))
            self.outdata[('cloudAerosolDiscriminationHigher', metaDataName)] = np.append(
                self.outdata[('cloudAerosolDiscriminationHigher', metaDataName)], np.array(cad1[winmsk], dtype=np.int32))
            self.outdata[('cloudAerosolDiscriminationLower', metaDataName)] = np.append(
                self.outdata[('cloudAerosolDiscriminationLower', metaDataName)], np.array(cad2[winmsk], dtype=np.int32))

            for i, iodavar in enumerate(obsvars):
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['valKey']], np.array(obs[winmsk, :, i], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['errKey']], np.array(err[winmsk, :, i], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['qcKey']], np.array(qcf[winmsk, :, i], dtype=np.int32))

            sd.end()

        self.outdata[('height', metaDataName)] = np.array(height, dtype=np.float32)
        self.outdata[('heightVertice', metaDataName)] = np.array(iheight, dtype=np.float32)
        self.outdata[('atmosphereLayerThicknessZ', metaDataName)] = np.array(thickness, dtype=np.float32)

        tmpseq = self.outdata[('sequenceNumber', metaDataName)]
        self.outdata[('sequenceNumber', metaDataName)] = tmpseq - min(tmpseq)

        DimDict['Location'] = len(self.outdata[('dateTime', metaDataName)])
        DimDict['Channel'] = np.arange(nlev) + 1
        DimDict['Vertice'] = nlev + 1

        min_time = min(self.outdata[('dateTime', metaDataName)])
        max_time = max(self.outdata[('dateTime', metaDataName)])
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

    args = parser.parse_args()

    # Read CALIPSO extinction profile data
    caliop_l2 = caliop_l2ext(args.input, args.date_range)

    # write everything out
    writer = iconv.IodaWriter(args.output, metaKeyList, DimDict)
    writer.BuildIoda(caliop_l2.outdata, VarDims, caliop_l2.varAttrs, AttrData)


if __name__ == "__main__":
    main()
