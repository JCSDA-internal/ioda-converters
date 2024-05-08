#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import argparse
from datetime import datetime
import dateutil.parser
from concurrent.futures import ProcessPoolExecutor
import numpy as np
import os
from itertools import repeat
import netCDF4 as nc

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.def_jedi_utils import epoch, iso8601_string, ioda_int_type, ioda_float_type, concat_obs_dict
from pyiodaconv.orddicts import DefaultOrderedDict

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

locationKeyList = [
    ('elevationAngleGNSS', 'float'),
    ('latitude', 'float'),
    ('longitude', 'float'),
    ('xECEFPosition', 'float'),
    ('yECFPosition', 'float'),
    ('zECFPosition', 'float'),
    ('xECFPositionGNSS', 'float'),
    ('yECFPositionGNSS', 'float'),
    ('zECFPositionGNSS', 'float'),
    ("dateTime", "long")
]


def main(args):
    RO_files = args.input
    print(f'{len(RO_files)} files to read')
    obs_data = {}
    # for file_obs_data in executor.map(read_input, pool_inputs, repeat(qc), repeat(addLSW), repeat(only_bang)):
    for ifile in RO_files:
        file_obs_data = get_obs_data(ifile, args)
        if not file_obs_data:
            print(f"INFO: non-nominal file skipping")
            continue
        if obs_data:
            file_obs_data[('sequenceNumber', 'MetaData')] += 1
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs = {}
    if args.date:
        dtg = datetime.strptime(args.date, '%Y%m%d%H')
        GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
        date_time_int32 = np.array(int(dtg.strftime("%Y%m%d%H")), dtype='int32')
        GlobalAttrs['date_time'] = date_time_int32.item()

    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {
        'totalElectronContent': ['Location'],
    }

    # write them out
    nlocs = obs_data[('totalElectronContent', 'ObsValue')].shape[0]
    DimDict = {'Location': nlocs}
    meta_data_types = def_meta_types()
    for k, v in meta_data_types.items():
        locationKeyList.append((k, v))
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('totalElectronContent', 'ObsValue')]['units'] = 'Unknown'
    VarAttrs[('totalElectronContent', 'ObsError')]['units'] = 'Unknown'
    VarAttrs[('elevationAngleGNSS', 'MetaData')]['units'] = 'degree'
    VarAttrs[('latitude', 'MetaData')]['units'] = 'degree'
    VarAttrs[('longitude', 'MetaData')]['units'] = 'degree'
    VarAttrs[('xECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('yECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('zECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('xECFPositionGNSS', 'MetaData')]['units'] = 'km'
    VarAttrs[('yECFPositionGNSS', 'MetaData')]['units'] = 'km'
    VarAttrs[('zECFPositionGNSS', 'MetaData')]['units'] = 'km'
    VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string

    VarAttrs[('totalElectronContent', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('totalElectronContent', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('totalElectronContent', 'PreQC')]['_FillValue'] = int_missing_value

    VarAttrs[('elevationAngleGNSS', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('xECFPosition', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('yECFPosition', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('zECFPosition', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('xECFPositionGNSS', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('yECFPositionGNSS', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('zECFPositionGNSS', 'MetaData')]['_FillValue'] = float_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_meta_data(ds):

    # get some of the global attributes that we are interested in
    meta_data_keys = def_meta_data()
    meta_data_types = def_meta_types()

    # these are the MetaData we are interested in
    profile_meta_data = {}
    psize = len(ds['time'])
    for k, v in meta_data_keys.items():
        try:
            attrValue = getattr(ds, v)
        except Exception as e:
            print(f'  WARNING: could not retrieve key: {k} -- Skipping')
            continue
        if meta_data_types[k] == 'long':
            profile_meta_data[k] = np.array(np.repeat(attrValue, psize), dtype=np.int64)
        elif meta_data_types[k] == 'integer':
            profile_meta_data[k] = np.array(np.repeat(attrValue, psize), dtype=ioda_int_type)
        elif meta_data_types[k] == 'float':
            profile_meta_data[k] = np.array(np.repeat(attrValue, psize), dtype=ioda_float_type)
        else:  # something else (what do we do with it)
            print(f"Found neither float nor in, type={type(v)}; skipping")

    # the time convert to epoch and handle array of values
    profile_meta_data['dateTime'] = np.array(ds['time'].add_offset + ds['time'][:], np.int64)

    # bespoke table of letter to WMO code
    transmitterConstellationId = get_GNSS_constellation(ds.conid)
    profile_meta_data['satelliteConstellationRO'] = np.array(np.repeat(transmitterConstellationId, psize), dtype=ioda_int_type)

    # bespoke character string to WMO identifier
    satelliteId = get_GNSS_mission(ds)
    profile_meta_data['satelliteIdentifier'] = np.array(np.repeat(satelliteId, psize), dtype=ioda_int_type)

    return profile_meta_data


def get_obs_data(ifile, get_obs_data_args):
    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    ds = nc.Dataset(ifile, "r")
    # ds.variables.keys()
    # ['time', 'TEC', 'S4', 'elevation', 'caL1_SNR', 'pL2_SNR', 'x_LEO', 'y_LEO', 'z_LEO', 'x_GPS', 'y_GPS', 'z_GPS']
    # ds.ncattrs()

    profile_meta_data = get_meta_data(ds)
    for k in profile_meta_data.keys():
        obs_data[(k, 'MetaData')] = profile_meta_data[k]

    import pdb
    pdb.set_trace()
    import sys
    sys.exit()
    # number to keep track of profile
    obs_data[('sequenceNumber', 'MetaData')] = np.array(np.repeat(get_obs_data_args.recordnumber, ds['x_LEO'].size), dtype=ioda_int_type)
    # Elevation angle of LEO-GPS link
    obs_data[("elevationAngleGNSS", "MetaData")] = np.array(ds['elevation'][:])
    # GPS x position (ECF) at time of signal transmission
    obs_data[("xECFPositionGNSS", "MetaData")] = np.array(ds['x_GPS'][:])
    obs_data[("yECFPositionGNSS", "MetaData")] = np.array(ds['y_GPS'][:])
    obs_data[("zECFPositionGNSS", "MetaData")] = np.array(ds['z_GPS'][:])
    # LEO x position (ECF) at time of signal reception
    obs_data[("xECFPosition", "MetaData")] = np.array(ds['x_LEO'][:])
    obs_data[("yECFPosition", "MetaData")] = np.array(ds['y_LEO'][:])
    obs_data[("zECFPosition", "MetaData")] = np.array(ds['z_LEO'][:])
    obs_data = get_geolocation(obs_data)
    # the observation value
    obs_data[("totalElectronContent", "ObsValue")] = np.array(ds['TEC'][:])

    return obs_data


def def_meta_data():

    # define the keys to retrieve for global meta data attributes (scalars)
    # this does NOT retrieve the ('Location') information (arrays)
    #       "elevationAngleGNSS": 'elevation'
    #       "GNSSxECFPosition": 'x_GPS'
    # antenna_id
    # attflag
    # podflag
    # leodcb_flag
    # leodcb_rms
    # gpsdcb_flag
    # gpsdcb_rms
    # leveling_err

    meta_data_keys = {
        "satelliteTransmitterId": 'prn_id',
        "satelliteSubIdentifier": 'leo_id',
        "antennaReceiverId": 'antenna_id',
    }

    return meta_data_keys


def def_meta_types():

    meta_data_types = {
        "latitude": "float",
        "longitude": "float",
        "dateTime": "long",
        "elevationAngleGNSS": "float",
        "xECFPositionGNSS": "float",
        "yECFPositionGNSS": "float",
        "zECFPositionGNSS": "float",
        "xECFPosition": "float",
        "yECFPosition": "float",
        "zECFPosition": "float",
        "antennaReceiverId": 'float',
        "satelliteIdentifier": 'integer',
        "satelliteSubIdentifier": 'integer',
        "satelliteTransmitterId": 'integer',
        "satelliteConstellationRO": 'integer',
    }

    return meta_data_types


def get_GNSS_constellation(constellationId):
    # convert letter codes to WMO constellation ID
    transmitterConstellationId = int_missing_value
    if constellationId == 'G':
        transmitterConstellationId = 401
    elif constellationId == 'R':
        transmitterConstellationId = 402
    elif constellationId == 'E':
        transmitterConstellationId = 403
    elif constellationId == 'C':
        transmitterConstellationId = 404
    else:
        transmitterConstellationId = int_missing_value
    return transmitterConstellationId


def get_GNSS_mission(ds):
    # return WMO satellite ID
    try:
        mission = ds.mission
    except Exception as e:
        return int_missing_value

    satID = int_missing_value
    if mission == 'COSEQ':
        satID = 749 + ds.leo_id
    return satID


def get_geolocation(obs_data):
    # wrapper to compute a reasonably accurate latitude and longitude
    #  from the Earth-centered Earth fixed coordinates
    obs_data[("latitude", "MetaData")] = np.full_like(obs_data[("xECFPosition", "MetaData")], float_missing_value)
    obs_data[("longitude", "MetaData")] = np.full_like(obs_data[("xECFPosition", "MetaData")], float_missing_value)
    for i, x in enumerate(obs_data[("xECFPosition", "MetaData")]):
        lat, lon, height = xyz2llh(x,
                                   obs_data[("yECFPosition", "MetaData")][i],
                                   obs_data[("zECFPosition", "MetaData")][i])
        obs_data[("latitude", "MetaData")][i] = lat
        obs_data[("longitude", "MetaData")][i] = lon

    return obs_data


def xyz2llh(x, y, z):
    """
    Function to convert xyz ECEF to llh
    convert cartesian coordinate into geographic coordinate
    ellipsoid definition: WGS84
      a = 6,378,137m
      f = 1 / 298.257

    Input
      x: coordinate x meters
      y: coordinate y meters
      z: coordinate z meters
    Output
      lat: latitude rad
      lon: longitude rad
      h: height meters
    """
    # --- WGS84 constants
    a = 6378137.0
    f = 1.0 / 298.257223563
    # --- derived constants
    b = a - f*a

    # convert ECF from km to meters
    x *= 1000.
    y *= 1000.
    z *= 1000.

    # how do you use NPY_1_PI
    rad2deg = 180. / np.pi

    e = np.sqrt(a**2 - b**2.0)/a
    lon = np.arctan2(y, x)
    p = np.sqrt(x**2 + y**2)
    h_old = 0.0
    # first guess with h = 0 meters
    lat = np.arctan2(z, p*(1.0 - e**2))
    cs = np.cos(lat)
    sn = np.sin(lat)
    N = a**2 / np.sqrt((a*cs)**2 + (b*sn)**2)
    h = p/cs - N
    # k = 0
    while abs(h - h_old) > 1.0e-6:
        h_old = h
        lat = np.arctan2(z, p*(1.0 - e**2*N/(N+h)))
        cs = np.cos(lat)
        sn = np.sin(lat)
        N = a**2 / np.sqrt((a*cs)**2 + (b*sn)**2)
        h = p/cs - N
    lat *= rad2deg
    lon *= rad2deg
    if lon > 180:
        lon -= 360.
    return lat, lon, h


if __name__ == "__main__":

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the GNSS TEC data from netCDF file'
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of GNSS TEC observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="full path and name of IODA output file",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, required=False, default=None)
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             '(default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-r', '--recordnumber',
        help=' optional record number to associate with profile ',
        type=int, default=1)

#   optional.add_argument(
#       '-q', '--qualitycontrol',
#       help='turn on quality control georeality checks',
#       default=False, action='store_true', required=False)

    args = parser.parse_args()
    main(args)
