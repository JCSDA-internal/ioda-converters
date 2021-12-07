#!/usr/bin/python

"""
Python code to ingest netCDF4 or HDF5 ATMS data
"""

import argparse
from datetime import datetime, timedelta
import glob
from multiprocessing import Pool
from pathlib import Path
import os.path
from os import getcwd
import sys

import h5py
import numpy as np

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
#import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

from IPython import embed as shell

# globals
ATMS_WMO_sat_ID = 77

GlobalAttrs = {
 ("MetaData", "platformCommonName") :  "ATMS",
 ("MetaData", "platformLongDescription") :  "ATMS Brightness Temperature Data",
 ("MetaData", "sensorCentralFrequency") : 
                 [23.8, 31.4, 50.3, 51.76, 52.8, 53.596, 54.40, 54.94, 55.50,
                  57.2903, 57.2903, 57.2903, 57.2903, 57.2903, 57.2903,
                  88.20, 165.5, 183.31, 183.31, 183.31, 183.31, 183.31],
}
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]


def main(input_files, output_dir, threads):

    pool_inputs = [( i ) for i in input_files]

    # read / process files in parallel
    pool = Pool(args.threads)
    #obs = pool.map(get_data_from_files, pool_inputs)
    obs = get_data_from_files( input_files )

    # concatenate the data from the files
    obs_data, loc_data = obs[0]

    nlocs = len(loc_data['lat'])
    nchans = 22

    # pass parameters to the IODA writer
    VarDims = {
        'brightnessTemperature': ['nlocs', 'nchans'],
    }

    DimDict = {'nlocs': nlocs, 'nchans': nchans}
    output_filename = os.path.join( output_dir, 'atms_ioda.v2.nc4' )
#   writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('brightnessTemperature', 'ObsValue')]['units'] = 'K'
    VarAttrs[('brightnessTemperature', 'ObsError')]['units'] = 'K'
    VarAttrs[('brightnessTemperature', 'PreQC')]['units']    = 'unitless'

    missing_value = -1.0000e+100
    int_missing_value = -2147483647
    VarAttrs[('brightnessTemperature', 'ObsValue')]['_FillValue'] = missing_value
    VarAttrs[('brightnessTemperature', 'ObsError')]['_FillValue'] = missing_value
    VarAttrs[('brightnessTemperature', 'PreQC')]['_FillValue'] = int_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_data_from_files( zfiles ):

    # allocate space for output depending on which variables are to be saved
    obs_data, loc_data = init_obs_loc()

    for afile in zfiles:
        f = h5py.File(afile, 'r')

        path, file = os.path.split(afile)
        if afile[-3:] == 'nc4':
            g = h5py.File(afile, 'r')
        else:
            geo_file = file.replace('SATMS', 'GATMO')
            ftype, sat, date, time, end_time, orbit, _, _, _ = file.split('_')
            geo_search = '_'.join(['GATMO', sat, date, time, end_time, orbit, '*.h5'])
            geo_search = os.path.join( path, geo_search )

            gfile = glob.glob( geo_search )
            if ( len(gfile) > 0 ):
                g = h5py.File(gfile[0], 'r')
            else:
                print ("could not find geofile matching: %s" % filename)
                print ("tried searching geofile: %s" % geo_search)
                sys.exit()

        obs_data, loc_data = get_data( f, g, obs_data, loc_data )
        f.close()
        g.close()
    return (obs_data, loc_data)

def get_data( f, g, obs_data, loc_data ):

#   NOAA CLASS h5 SDR-GEO keys
#   'BeamLatitude', 'BeamLongitude', 'Height', 'Latitude', 'Longitude', 'MidTime', 'PadByte1', 
#   'QF1_ATMSSDRGEO', 'SCAttitude', 'SCPosition', 'SCVelocity', 'SatelliteAzimuthAngle', 
#   'SatelliteRange', 'SatelliteZenithAngle', 'SolarAzimuthAngle', 'SolarZenithAngle', 'StartTime' 

#   NOAA CLASS h5 SDR-Data keys
#   'BeamTime', 'BrightnessTemperature', 'BrightnessTemperatureFactors', 'GainCalibration', 
#   'InstrumentMode', 'NEdTCold', 'NEdTWarm', 'PadByte1', 'QF10_GRAN_HEALTHSTATUS', 
#   'QF11_GRAN_QUADRATICCORRECTION', 'QF12_SCAN_KAVPRTCONVERR', 'QF13_SCAN_WGPRTCONVERR', 
#   'QF14_SCAN_SHELFPRTCONVERR', 'QF15_SCAN_KAVPRTTEMPLIMIT', 'QF16_SCAN_WGPRTTEMPLIMIT', 
#   'QF17_SCAN_KAVPRTTEMPCONSISTENCY', 'QF18_SCAN_WGPRTTEMPCONSISTENCY', 'QF19_SCAN_ATMSSDR', 
#   'QF1_GRAN_HEALTHSTATUS', 'QF20_ATMSSDR', 'QF21_ATMSSDR', 'QF22_ATMSSDR', 
#   'QF2_GRAN_HEALTHSTATUS', 'QF3_GRAN_HEALTHSTATUS', 'QF4_GRAN_HEALTHSTATUS', 
#   'QF5_GRAN_HEALTHSTATUS', 'QF6_GRAN_HEALTHSTATUS', 'QF7_GRAN_HEALTHSTATUS', 
#   'QF8_GRAN_HEALTHSTATUS', 'QF9_GRAN_HEALTHSTATUS' 

#   NASA GES DISC keys
#   'antenna', 'antenna_len', 'antenna_temp', 'antenna_temp_qc', 'asc_flag', 'asc_node_local_solar_time', 
#   'asc_node_lon', 'asc_node_tai93', 'atrack', 'attitude', 'attitude_lbl', 'attitude_lbl_len', 
#   'aux_cal_blackbody_qualflag', 'aux_cal_qualflag', 'aux_cal_space_qualflag', 'aux_cold_temp', 
#   'aux_gain', 'aux_geo_qualflag', 'aux_nonlin', 'aux_offset', 'aux_warm_temp', 'band', 'band_geoloc_chan', 
#   'band_land_frac', 'band_lat', 'band_lat_bnds', 'band_lbl', 'band_lbl_len', 'band_lon', 'band_lon_bnds', 
#   'band_surf_alt', 'bandwidth', 'beam_width', 'center_freq', 'chan_band', 'chan_band_len', 'channel', 
#   'cold_nedt', 'fov_poly', 'if_offset_1', 'if_offset_2', 'instrument_state', 'land_frac', 'lat', 
#   'lat_bnds', 'lat_geoid', 'local_solar_time', 'lon', 'lon_bnds', 'lon_geoid', 'mean_anom_wrt_equat', 
#   'moon_ang', 'obs_id', 'obs_id_len', 'obs_time_tai93', 'obs_time_utc', 'polarization', 'polarization_len', 
#   'sat_alt', 'sat_att', 'sat_azi', 'sat_pos', 'sat_range', 'sat_sol_azi', 'sat_sol_zen', 'sat_vel', 'sat_zen', 
#   'scan_mid_time', 'sol_azi', 'sol_zen', 'solar_beta_angle', 'spacextrack', 'spatial', 'spatial_lbl', 
#   'spatial_lbl_len', 'subsat_lat', 'subsat_lon', 'sun_glint_dist', 'sun_glint_lat', 'sun_glint_lon', 
#   'surf_alt', 'surf_alt_sdev', 'utc_tuple', 'utc_tuple_lbl', 'utc_tuple_lbl_len', 'view_ang', 'warm_nedt', 'xtrack'

    # dimension ( 180, 96 )

    #shell()
    #sys.exit()
    try:
        nscans                          =  np.shape(g['lat'])[0]
        nbeam_pos                       =  np.shape(g['lat'])[1]
        loc_data['latitude']            =  g['lat'][:,:].flatten()
        loc_data['longitude']           =  g['lon'][:,:].flatten()
        loc_data['channelNumber']       =  g['channel'][:]
        loc_data['satelliteId']         =  ATMS_WMO_sat_ID
        loc_data['fieldOfViewNumber']   =  np.tile( np.arange(nbeam_pos)+1, (nscans,1) ).flatten()
        loc_data['solarZenithAngle']    =  g['sol_zen'][:,:].flatten()
        loc_data['solarAzimuthAngle']   =  g['sol_azi'][:,:].flatten()
        loc_data['sensorZenithAngle']   =  g['sat_zen'][:,:].flatten()
        loc_data['sensorAzimuthAngle']  =  g['sat_azi'][:,:].flatten()
        obs_time_utc = loc_data['datetime']  = g['obs_time_utc'][:,:].flatten()
        nchans = len( loc_data['channelNumber'] )
        nlocs  = len( loc_data['latitude'] )
#       dtg = ( "%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (year, month, day, hour, minute) )
#       loc_data['datetime'] = datetime.strptime( dtg ,"%Y-%m-%dT%H:%M:%SZ")


    except:
        loc_data['latitude'].append(  g['All_Data']['ATMS-SDR-GEO_All']['Latitude'][:,:].flatten()  )
        loc_data['longitude'].append(  g['All_Data']['ATMS-SDR-GEO_All']['Longitude'][:,:].flatten() )

    # dimension ( 180, 96, 22 )
    try:
        obs_data[('brightnessTemperature', "ObsValue")]    = np.vstack(g['antenna_temp'])
        obs_data[('brightnessTemperature', "ObsError")]    = np.full((nlocs,nchans), 5.0, dtype='float32')
        obs_data[('brightnessTemperature', "PreQC")]       = np.full((nlocs,nchans), 0, dtype='int32')
    except:
        scaled_data = np.vstack( f['All_Data']['ATMS-SDR_All']['BrightnessTemperature'] )
        scale_fac = f['All_Data']['ATMS-SDR_All']['BrightnessTemperatureFactors'][:].flatten()

        obs_data[('brightnessTemperature', "ObsValue")]    = (scaled_data * scale_fac[0]) + scale_fac[1]
        obs_data[('brightnessTemperature', "ObsError")]    = np.full((nlocs,nchans), 5.0, dtype='float32')
        obs_data[('brightnessTemperature', "PreQC")]       = np.full((nlocs,nchans), 0, dtype='int32')

    return obs_data, loc_data

def init_obs_loc():
    obs = { 
             ('brightnessTemperature', "ObsValue")  : [], 
             ('brightnessTemperature', "ObsError")  : [], 
             ('brightnessTemperature', "PreQC")     : [], 
}

    loc = {
            'satelliteId'  :  [],
            'channelNumber'  :  [],
            'latitude'  :  [],
            'longitude'  :  [],
            'datetime'  :  [],
            'fieldOfViewNumber'  :  [],
            'solarZenithAngle'  :  [],
            'solarAzimuthAngle'  :  [],
            'sensorZenithAngle'  :  [],
            'sensorAzimuthAngle'  :  [],
}
    
    return obs, loc

if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description=(
            'Reads the satellite data '
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated' )
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of satellite observation input file(s)",
        type=str, nargs='+', required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-o', '--output-dir',
        help='output directory path',
        type=str, default=os.getcwd())

    args = parser.parse_args()

    # create output directory path if necessary
    if not os.path.exists(args.output_dir):
        os.mkdir( args.output_dir )

    main(args.input, args.output_dir, args.threads)
