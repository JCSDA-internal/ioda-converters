#!/usr/bin/env python3

#
# (C) Copyright 2020-2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import os
from pyhdf.SD import SD, SDC

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", iso8601_string),
    ("surfaceQualifier", "integer", ""),
]

obsvars = ["aerosolOpticalDepth"]

# A dictionary of global attributes.  More filled in further down.
AttrData = {
    'converter': os.path.basename(__file__),
    'description': 'AOD at 550nm'
}

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {
    'aerosolOpticalDepth': ['Location', 'Channel'],
    'surfaceQualifier': ['Location'],
}
channels = [4]

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

# QC mapping array for MODIS collection (Dark Target and Deep Blue)
# https://darktarget.gsfc.nasa.gov/products/viirs-modis/level-2-product-contents
# AOD_550_Dark_Target_Deep_Blue_Combined_QA_Flag:
#   0 = no retrieval, 1 = marginal, 2 = good, 3 = very good/best
qcmapping = {0: 3, 1: 2, 2: 1, 3: 0}
nasa_flip_qc = np.array([qcmapping[k] for k in sorted(qcmapping)])


class AOD(object):
    def __init__(self, filenames, date_range, pltfrm):
        self.filenames = filenames
        self.wbeg = np.datetime64(datetime.strptime(date_range[0], "%Y%m%d%H"))
        self.wend = np.datetime64(datetime.strptime(date_range[1], "%Y%m%d%H"))
        self.pltfrm = pltfrm
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        # there's absolutely no difference in the hdf4 files attributes
        # between Terra and Aqua files. So it is user specified
        AttrData['platform'] = pltfrm
        # sensor would be always MODIS for this converter
        AttrData['sensor'] = 'MODIS'
        self.setDicts()
        self._read()

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

    def _read(self):

        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=np.int64)
        self.outdata[('surfaceQualifier', metaDataName)] = np.array([], dtype=np.int32)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        # loop through input filenames
        for f in self.filenames:
            hdf = SD(f, SDC.READ)

            # All of MODIS AOD data have a singular reference time - good practice to get from attribute
            modis_time_key = 'Scan_Start_Time'
            try:
                modis_time_attribute = hdf.select(modis_time_key).attributes().get('units')
                if modis_time_attribute is None:
                    print("'units' attribute is not present in {modis_time_key}.")
                    modis_ref_time = datetime(1993, 1, 1, 0, 0, 0)
                else:
                    # Extract the date and time part
                    datetime_str = modis_time_attribute.split('since ')[1].rsplit(' ', 1)[0]

                    # Convert to a datetime object
                    modis_ref_time = datetime.strptime(datetime_str, "%Y-%m-%d %H:%M:%S.%f")
            except Exception as e:
                # Catch and print any errors
                print(f"An error occurred: {e}")

            #  Get variables
            modis_time = hdf.select(modis_time_key)[:].ravel()
            modis_time = modis_time.astype('float32')
            lats = hdf.select('Latitude')[:].ravel()
            lats = lats.astype('float32')
            lons = hdf.select('Longitude')[:].ravel()
            lons = lons.astype('float32')
            aod = hdf.select('AOD_550_Dark_Target_Deep_Blue_Combined')[:].ravel()
            aod = aod.astype('float64')
            land_sea_flag = hdf.select('Land_sea_Flag')[:].ravel()
            sol_zen = hdf.select('Solar_Zenith')[:].ravel()
            sen_zen = hdf.select('Sensor_Zenith')[:].ravel()
            unc_land = hdf.select('Deep_Blue_Aerosol_Optical_Depth_550_Land_Estimated_Uncertainty')[:].ravel()

            # Special treatment for qc flags
            QC_flag = hdf.select('AOD_550_Dark_Target_Deep_Blue_Combined_QA_Flag')[:].ravel()
            QC_flag = QC_flag.astype('int32')
            valid_QC = (QC_flag >= 0) & (QC_flag <= 3)
            # Flip QC flags for PreQC (0->3, 3->0)
            QC_flag[valid_QC] = nasa_flip_qc[QC_flag[valid_QC]]
            QC_flag = np.where(~valid_QC, missing_vals['integer'], QC_flag)

            # Remove undefined values
            pos_index = np.where(aod > 0)
            lats = lats[pos_index]
            lons = lons[pos_index]
            aod = aod[pos_index] * 1E-3  # see scale factor
            land_sea_flag = land_sea_flag[pos_index]
            QC_flag = QC_flag[pos_index]
            sol_zen = sol_zen[pos_index]
            sen_zen = sen_zen[pos_index]
            unc_land = unc_land[pos_index] * 1E-3  # see scale factor
            modis_time = modis_time[pos_index]
            obs_time = (modis_time + modis_ref_time.timestamp()).astype('datetime64[s]')
            winmsk = ((obs_time >= self.wbeg) & (obs_time <= self.wend))

            # uncertainty estimates:
            # From MODIS file (over ocean) and Levy, 2010 (over land)
            # https://acp.copernicus.org/articles/10/10399/2010/acp-10-10399-2010.pdf
            # flag = 0 (ocean) 1(land) 2(coastal)
            over_ocean = np.logical_not(land_sea_flag > 0)
            over_land = np.logical_not(land_sea_flag == 0)
            UNC = np.where(over_land, unc_land, np.add(0.05, np.multiply(0.15, aod)))

            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)],
                                                                 np.array(lats[winmsk], dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)],
                                                                  np.array(lons[winmsk], dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)],
                                                                 np.array(obs_time[winmsk], dtype=np.int64))
            self.outdata[('surfaceQualifier', metaDataName)] = np.append(self.outdata[('surfaceQualifier', metaDataName)],
                                                                         np.array(land_sea_flag[winmsk], dtype=np.int32))

            for iodavar in obsvars:
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(self.outdata[self.varDict[iodavar]['valKey']],
                                                                          np.array(aod[winmsk], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(self.outdata[self.varDict[iodavar]['errKey']],
                                                                          np.array(UNC[winmsk], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(self.outdata[self.varDict[iodavar]['qcKey']],
                                                                         np.array(QC_flag[winmsk], dtype=np.int32))

        DimDict['Location'] = len(self.outdata[('dateTime', metaDataName)])
        DimDict['Channel'] = np.array(channels)


def main():

    # get command line arguments
    # Usage: python modis_aod2ioda.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... -p <Terra or Aqua>
    # -o /path/to/ioda/2021060806.nc --date_range YYYYMMDDHH YYYYMMDDHH
    # where the input obs could be for any desired interval to concatenated together.
    # Use date_range to process data for the length of assimilation window.
    parser = argparse.ArgumentParser(
        description=(
            'Reads MODIS AOD hdf4 files provided by NASA'
            ' and converts into IODA formatted output files. Multiple'
            ' files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of MODIS AOD hdf4 input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    required.add_argument(
        '--platform',
        help="AQUA or TERRA satellite",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--date_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHH YYYYMMDDHH",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    args = parser.parse_args()

    # setup the IODA writer
    # Read in the AOD data
    aod_class = AOD(args.input, args.date_range, args.platform)

    # write everything out
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(aod_class.outdata, VarDims, aod_class.varAttrs, AttrData)


if __name__ == '__main__':
    main()
