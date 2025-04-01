#!/usr/bin/env python3

"""
Python code to ingest HDF4 CALIPSO L2 APro data
"""

import argparse
from datetime import datetime, timedelta
import os, sys

from pyhdf.HDF import *
from pyhdf.VS import *
from pyhdf.SD import SD, SDC
import numpy as np
import netCDF4 as nc

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import compute_scan_angle
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

# globals
CALIPSO_WMO_sat_ID = 787

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
    ("sensorCentralWavelength", "float", "micron"),
    #("sequenceNumber", "integer", None),
    ("height", "float", "m"),
    ("cloudAerosolDiscrimination", "integer", ""),
]

DimDict = {
}

VarDims = {
    'extinctionCoefficient': ['Location', 'Layer', 'Channel'],
    'pressure': ['Location', 'Layer'],
    'height': ['Layer'],
    'sequenceNumber': ['Location'],
    'sensorCentralWavelength': ['Channel'],
    'cloudAerosolDiscrimination': ['Location', 'Layer', 'Channel'],
}

obsvars = ["extinctionCoefficient"]
channels = [1, 2]
wavelength = [0.532, 1.064]

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

varsKeyList = [('valKey', obsValName, 'float', 'longitude latitude', 'km-1'),
               ('errKey', obsErrName, 'float', 'longitude latitude', 'km-1'),
               ('qcKey', qcName, 'integer', 'longitude latitude', None)]


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


class calipso_l2ext(object):
    def __init__(self, filenames, date_range):
        self.filenames = filenames
        self.wbeg = np.datetime64(datetime.strptime(date_range[0], "%Y%m%d%H"))
        self.wend = np.datetime64(datetime.strptime(date_range[1], "%Y%m%d%H"))
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
        self.outdata[('cloudAerosolDiscrimination', metaDataName)] = np.array([], dtype=np.int32)
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
        alt = np.array(vd.read()[0][0]) * 1000.
        vd.detach()
        vs.end()

        for f in self.filenames:
            sd = SD(f, SDC.READ)

            pres = sd.select('Pressure').get() * 1e2 # hPa to Pa
            nloc = pres.shape[0]
            nlev = pres.shape[1] 
            #pres = pres.ravel()
            lats = sd.select('Latitude').get()[:,1]
            lons = sd.select('Longitude').get()[:,1]
            profidx = np.arange(nloc)
            proftime = sd.select('Profile_Time').get()[:,1]
            obs_time = (proftime + caliop_ref_time.timestamp()).astype('datetime64[s]')
            winmsk = ((obs_time >= self.wbeg) & (obs_time <= self.wend))

            obs = np.zeros((nloc, nlev, nchan))
            err = np.zeros_like(obs)
            qcf = np.zeros_like(obs)
            cad = np.zeros_like(obs)
            for i, chidx in enumerate(output_chidx):
                wavelength_str = str(int(wavelength[chidx] * 1e3))
                obsvarname = f"Extinction_Coefficient_{wavelength_str}"
                errvarname = f"Extinction_Coefficient_Uncertainty_{wavelength_str}"
                qcfvarname = f"Extinction_QC_Flag_{wavelength_str}"
                # Level 2 QC flag stores 30m level 1 QC flag below 8.3 km in the rightmost dimension
                tmpqcf = sd.select(qcfvarname).get()
                tmpqcf = np.where(tmpqcf[:, :, 0]==tmpqcf[:, :, 1], tmpqcf[:, :, 0], 
                                  np.maximum(tmpqcf[:, :, 0], tmpqcf[:, :, 1]))

                obs[:, :, i] = sd.select(obsvarname).get()
                err[:, :, i] = sd.select(errvarname).get()
                qcf[:, :, i] = tmpqcf

            cadvarname = f"CAD_Score"
            cad = sd.select(cadvarname).get()

            obs = np.where(obs==caliop_missing_value, float_missing_value, obs)
            err = np.where(err==caliop_missing_value, float_missing_value, err)
            pres = np.where(pres < 0, float_missing_value, pres)
                
            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)],
                                                                 np.array(lats[winmsk], dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)],
                                                                  np.array(lons[winmsk], dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)],
                                                                 np.array(obs_time[winmsk], dtype=np.int64))
            self.outdata[('pressure', metaDataName)] = np.append(self.outdata[('pressure', metaDataName)],
                                                                 np.array(pres[winmsk, :], dtype=np.float32))
            self.outdata[('cloudAerosolDiscrimination', metaDataName)] = np.append(
                    self.outdata[('cloudAerosolDiscrimination', metaDataName)],  np.array(cad[winmsk, :, :], dtype=np.int32))

            for iodavar in obsvars:
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(
                        self.outdata[self.varDict[iodavar]['valKey']], np.array(obs[winmsk, :, :], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(
                        self.outdata[self.varDict[iodavar]['errKey']], np.array(err[winmsk, :, :], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(
                        self.outdata[self.varDict[iodavar]['qcKey']],  np.array(qcf[winmsk, :, :], dtype=np.int32))

            sd.end()

        self.outdata[('sensorCentralWavelength', metaDataName)] = np.array(wavelength, dtype=np.float32)[output_chidx]
        self.outdata[('sensorCentralFrequency', metaDataName)] = np.array(wavelength, dtype=np.float32)[output_chidx]
        self.outdata[('height', metaDataName)] = np.array(alt, dtype=np.float32)
        DimDict['Location'] = len(self.outdata[('dateTime', metaDataName)])
        DimDict['Channel'] = nchan
        DimDict['Layer'] = nlev

def main():
    parser = argparse.ArgumentParser(
        description=(
            'Reads the satellite data '
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of satellite observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help='path to output ioda file',
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--date_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHH YYYYMMDDHH",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    args = parser.parse_args()

    # Read CALIPSO extinction profile data
    calipsol2 = calipso_l2ext(args.input, args.date_range)

    # write everything out
    writer = iconv.IodaWriter(args.output, metaKeyList, DimDict)
    writer.BuildIoda(calipsol2.outdata, VarDims, calipsol2.varAttrs, AttrData)

if __name__ == "__main__":
    main()

