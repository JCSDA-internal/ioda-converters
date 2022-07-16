#!/usr/bin/env python3

"""
Python code to ingest HDF4 CALIPSO L2 APro data
"""

import argparse
from datetime import datetime, timedelta
import glob
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
import os.path
from os import getcwd
import sys

from pyhdf.HDF import *
from pyhdf.VS import *
from pyhdf.SD import SD, SDC
import numpy as np

float_missing_value = -9999.
int_missing_value = 32768

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

# globals
CALIPSO_WMO_sat_ID = 787

GlobalAttrs = {
    "platformCommonName": "CALIPSO",
    "platformLongDescription": "CALIPSO L2 Lidar Data",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("level", "int32")
]

obsvars = {
}

AttrData = {
}

DimDict = {
}

VarDims = {
}

def main(args):

    output_filename = args.output
    #dtg = datetime.strptime(args.date, '%Y%m%d%H')

    input_files = [(i) for i in args.input]
    # initialize
    obs_data = {}
    meta_data = {}
    # read / process files in parallel
#   with ProcessPoolExecutor(max_workers=args.threads) as executor:
#       for file_obs_data in executor.map(get_data_from_files, input_files):
#           if not file_obs_data:
#               print("INFO: non-nominal file skipping")
#               continue
#           if obs_data:
#               concat_obs_dict(obs_data, file_obs_data)
#           else:
#               obs_data = file_obs_data

    for afile in input_files:
        file_obs_data = get_data_from_files(afile)
        if not file_obs_data:
            print("INFO: non-nominal file skipping")
            continue
        if obs_data:
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    ### Can be used when 3d IODA variables available
    #lid_wav=np.array([532,1064],dtype='float32')
    #speed_light = 2.99792458E8
    #frequency = speed_light*1.0E9/lid_wav

    nlocs_int32 = np.array(len(obs_data[('latitude', 'MetaData')]), dtype='float32')  # this is float32 in old convention
    nlocs = nlocs_int32.item()
    #nchans = len(obs_data[('channelNumber', 'MetaData')])

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    #GlobalAttrs['date_time_string'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    #date_time_int32 = np.array(int(dtg.strftime("%Y%m%d%H")), dtype='int32')
    #GlobalAttrs['date_time'] = date_time_int32.item()
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {
        'ExtinctionCoeff_532': ['nlocs', 'nlevs'],
        'Extinction_QC_Flag_532': ['nlocs', 'nlevs'],
        'ExtinctionCoeff_1064': ['nlocs', 'nlevs'],
        'Extinction_QC_Flag_1064': ['nlocs', 'nlevs'],
        'Pressure': ['nlocs', 'nlevs'],
        'Temperature': ['nlocs', 'nlevs'],
        'level':['nlevs'],
        'Lidar_Data_Altitudes':['nlevs'],
        'profileTime':['nlocs'],
    }

    DimDict = {
        'nlocs': nlocs,
        'nlevs': len(obs_data[('level','MetaData')]),
    }
    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    VarAttrs[('ExtinctionCoeff_532',   'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('ExtinctionCoeff_1064',  'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('Extinction_QC_Flag_532',   'ObsValue')]['_FillValue'] = int_missing_value
    VarAttrs[('Extinction_QC_Flag_1064',  'ObsValue')]['_FillValue'] = int_missing_value
    VarAttrs[('ExtinctionCoeff_532',   'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('ExtinctionCoeff_1064',  'ObsError')]['_FillValue'] = float_missing_value
    #VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value

    VarAttrs[('ExtinctionCoeff_532',   'ObsValue')]['units'] = 'km-1'
    VarAttrs[('ExtinctionCoeff_1064',  'ObsValue')]['units'] = 'km-1'
    VarAttrs[('ExtinctionCoeff_532',   'ObsError')]['units'] = 'km-1'
    VarAttrs[('ExtinctionCoeff_1064',  'ObsError')]['units'] = 'km-1'
    #VarAttrs[(k, 'PreQC')]['units'] = 'unitless'

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


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


if __name__ == "__main__":

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
    #required.add_argument(
    #    '-d', '--date',
    #    metavar="YYYYMMDDHH",
    #    help="base date for the center of the window",
    #    type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-o', '--output',
        help='path to output ioda file',
        type=str, default=os.path.join(os.getcwd(), 'output.nc4'))

    args = parser.parse_args()

    main(args)
