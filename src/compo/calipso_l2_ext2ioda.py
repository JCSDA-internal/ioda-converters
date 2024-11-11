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

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import compute_scan_angle
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

# globals
CALIPSO_WMO_sat_ID = 787

AttrsData = {
    'converter': os.path.basename(__file__),
    "platformCommonName": "CALIPSO",
    "platformLongDescription": "CALIPSO L2 Lidar Data",
}

metaKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", iso8601_string),
    #("lidarDataAltitude", "float", "km"),
    ("pressure", "float", "hPa"),
    ("sensorCentralWavelength", "float", "micron"),
]

DimDict = {
}

VarDims = {'extinctionCoefficient':['Location', 'Level', 'Channel']}
channels = [1, 2]
wavelength = [0.532, 1.064]

metaDataName = iconv.MetaDataName()
varsKeyList = [('valKey', iconv.OvalName(), 'float', 'longitude latitude', 'km-1'),
               ('errKey', iconv.OerrName(), 'float', 'longitude latitude', 'km-1'),
               ('qcKey', iconv.OqcName(), 'integer', 'longitude latitude', None)]


float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'

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
        self.pltfrm = pltfrm
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
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
        for iodavar in varDims.keys():
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
        default_missing_value = -9999.
        n_channel = len(channels)
        output_chidx = np.array(channels, dtype=np.int32) - 1

        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=np.int64)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        for f in self.filenames:
            hdf = SD(f, SDC.READ)

            nlevs = hdf.select('Pressure').get().shape[1]

            lats = f.select('Latitude').get()[:,1]
            lons = f.select('Longitude').get()[:,1]
            pres = f.select('Pressure').get()
            proftime = f.select('Profile_Time').get()[:,1]

            obs = np.array([])
            err = np.array([])
            qcf = np.array([])
            for chidx in output_chidx:
                wavelength_str = str(int(wavelength[chidx] * 1000))
                obsvarname = f"Extinction_Coefficient_{wavelength_str}"
                errvarname = f"Extinction_Coefficient_Uncertainty_{wavelength_str}"
                qcfvarname = f"Extinction_QC_Flag_{wavelength_str}"

                obs = np.append(obs, f.select(obsvarname).get())
                err = np.append(err, f.select(errvarname).get())
                qcf = np.append(qcf, f.select(qcfvarname).get())
                
            print(obs.shape)
            sys.exit()

            self.outdata[('latitude', metaDataName)] = np.append(self.outdata[('latitude', metaDataName)],
                                                                 np.array(lats, dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(self.outdata[('longitude', metaDataName)],
                                                                  np.array(lons, dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(self.outdata[('dateTime', metaDataName)],
                                                                 np.array(proftime, dtype=np.int64))

            for iodavar in VarDims.keys():
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(self.outdata[self.varDict[iodavar]['valKey']],
                                                                          np.array(obs, dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(self.outdata[self.varDict[iodavar]['errKey']],
                                                                          np.array(err, dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(self.outdata[self.varDict[iodavar]['qcKey']],
                                                                         np.array(qcf, dtype=np.int32))
            hdf.end()

        self.outdata[('sensorCentralWavelength', metaDataName)] = np.array(wavelength, dtype=np.float32)[output_chidx]
        DimDict['Location'] = len(self.outdata[('dateTime', metaDataName)])
        # DimDict['Level'] = 

def get_data_from_files(afile):

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    hdf=HDF(afile)
    vs=hdf.vstart()
    meta_dict=get_hdf_meta_dict(vs)
    vs.end()
    hdf.close()

    f = SD(afile,SDC.READ)
    obs_data = get_data(f, obs_data, meta_dict)
    f.end()

    return obs_data

def get_hdf_meta_dict(vs):
    # Reference: https://forum.earthdata.nasa.gov/viewtopic.php?f=7&t=2452#confirm_external_link-modal
    hdfmeta = vs.attach('metadata')
    hdfmeta_fields=hdfmeta.fieldinfo()
    hdfmeta_values=hdfmeta.read(hdfmeta._nrecs)[0]
    hdfmeta.detach()
    meta_dict={}
    fld_idx=0
    for fld, data in zip(hdfmeta_fields,hdfmeta_values):
        meta_dict[fld[fld_idx]]=data

    return meta_dict

def get_data(f, obs_data, meta_dict):

    #nchans = 2
    nlevs = f.select('Pressure').get().shape[1]
    
    obs_data[('latitude', 'MetaData')] = np.array(f.select('Latitude').get()[:,1], dtype='float32')
    obs_data[('longitude', 'MetaData')] = np.array(f.select('Longitude').get()[:,1], dtype='float32')
    obs_data[('level', 'MetaData')] = np.array(np.arange(nlevs)+1, dtype='int32')
  
    nlocs = len(obs_data[('latitude', 'MetaData')])
    obs_data[('satelliteId', 'MetaData')] = np.full((nlocs), CALIPSO_WMO_sat_ID, dtype='int32')
    
    obs_data[('Lidar_Data_Altitudes', 'MetaData')] = np.array(meta_dict['Lidar_Data_Altitudes'],dtype='float32')
    obs_data[('profileTime', 'MetaData')] = np.array(f.select('Profile_Time').get()[:,1], dtype='float32')
    obs_data[('Pressure', 'MetaData')] = np.array(f.select('Pressure').get(), dtype='float32')
    obs_data[('Temperature', 'MetaData')] = np.array(f.select('Temperature').get(), dtype='float32')

    obs_data[('ExtinctionCoeff_532', "ObsValue")] = np.array(f.select("Extinction_Coefficient_532").get(),dtype='float32')
    obs_data[('ExtinctionCoeff_1064',"ObsValue")] = np.array(f.select("Extinction_Coefficient_1064").get(),dtype='float32')
    obs_data[('ExtinctionCoeff_532', "ObsError")] = np.array(f.select("Extinction_Coefficient_Uncertainty_532").get(),dtype='float32')
    obs_data[('ExtinctionCoeff_1064',"ObsError")] = np.array(f.select("Extinction_Coefficient_Uncertainty_1064").get(),dtype='float32')

    obs_data[('Extinction_QC_Flag_532', "ObsValue")] = np.array(f.select("Extinction_QC_Flag_532").get(),dtype='int16')
    obs_data[('Extinction_QC_Flag_1064',"ObsValue")] = np.array(f.select("Extinction_QC_Flag_1064").get(),dtype='int16')

    # For PreQC, the value of -9999. and -333. of Extinction and Backscatter can be rejected.

    #obs_data[(k, "PreQC")] = np.full((nlocs, nchans), 0, dtype='int32')
    #quality_word = np.vstack(np.stack(f['calQualityFlag'], axis=2))
    #obs_data[('ascending_flag', 'MetaData')] = np.array(get_normalized_bit(quality_word[:, 0], bit_index=6), dtype='int32')
    #obs_key = (k, "ObsValue")
    #obs_data = set_missing_value(nchans, chk_geolocation, quality_word, obs_key, obs_data)

    return obs_data


def set_missing_value(nchans, chk_geolocation, quality_word, obs_key, obs_data):
    # use quality word to determine where to set for missing values
    for jchan in np.arange(nchans):
        i_land = get_normalized_bit(quality_word[:, jchan], bit_index=1)
        i_intrusion = get_normalized_bit(quality_word[:, jchan], bit_index=2)
        i_maneuver = get_normalized_bit(quality_word[:, jchan], bit_index=3)
        i_cold_cal = get_normalized_bit(quality_word[:, jchan], bit_index=4)
        i_hot_cal = get_normalized_bit(quality_word[:, jchan], bit_index=5)
        i_asc = get_normalized_bit(quality_word[:, jchan], bit_index=6)
        i_day = get_normalized_bit(quality_word[:, jchan], bit_index=7)
        i_forward = get_normalized_bit(quality_word[:, jchan], bit_index=8)
        chk_ob = (i_cold_cal + i_hot_cal + i_intrusion + i_maneuver + chk_geolocation) > 0
        obs_data[obs_key][:, jchan][chk_ob] = float_missing_value

    return obs_data


def get_normalized_bit(value, bit_index):
    return (value >> bit_index) & 1


def assign_values(data):
    if data.dtype == float:
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
        return np.array(data, dtype=ioda_int_type)


def concat_obs_dict(obs_data, append_obs_data):
    # For now we are assuming that the obs_data dictionary has the "golden" list
    # of variables. If one is missing from append_obs_data, the obs_data variable
    # will be extended using fill values.
    #
    # Use the first key in the append_obs_data dictionary to determine how
    # long to make the fill value vector.
    append_keys = list(append_obs_data.keys())


def get_string_dtg(f):

    # for TROPICS data times are per scan line
    # current IODA needs replication by beam position
    nbeam_pos = len(f['spots'])
    year = f['Year']
    month = f['Month']
    day = f['Day']
    hour = f['Hour']
    minute = f['Minute']
    dtg = []
    for i, yyyy in enumerate(year):
        cdtg = ("%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (yyyy, month[i], day[i], hour[i], minute[i]))
        # need to add replication by nbeam_pos
        for _ in range(nbeam_pos):
            dtg.append(cdtg)

    return dtg


def init_obs_loc():
    obs = {
        ( 'ExtinctionCoeff_532', "ObsValue"): [],
        ( 'ExtinctionCoeff_1064', "ObsValue"): [],
        ( 'ExtinctionCoeff_532', "ObsError"): [],
        ( 'ExtinctionCoeff_1064', "ObsError"): [],
        ( 'Extinction_QC_Flag_532', "ObsValue"): [],
        ( 'Extinction_QC_Flag_1064', "ObsValue"): [],
        ('latitude', 'MetaData'): [],
        ('longitude', 'MetaData'): [],
        ('level', 'MetaData'): [],
        ('Lidar_Data_Altitudes', 'MetaData'): [],
        ('profileTime', 'MetaData'): [],
        ('satelliteId', 'MetaData'): [],
    }

    return obs


def concat_obs_dict(obs_data, append_obs_data):
    # For now we are assuming that the obs_data dictionary has the "golden" list
    # of variables. If one is missing from append_obs_data, a warning will be issued.
    append_keys = list(append_obs_data.keys())
    for gv_key in obs_data.keys():
        if gv_key in append_keys:
            obs_data[gv_key] = np.append(obs_data[gv_key], append_obs_data[gv_key], axis=0)
        else:
            print("WARNING: ", gv_key, " is missing from append_obs_data dictionary")


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
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(calipsol2.outdata, VarDims, calipsol2.varAttrs, AttrData)

if __name__ == "__main__":
    main()

