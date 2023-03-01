#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import argparse
from datetime import datetime, timedelta
import dateutil.parser
from concurrent.futures import ProcessPoolExecutor
import numpy as np
import os
import netCDF4 as nc
import h5py as h5

import lib_python.ioda_conv_engines as iconv
from lib_python.orddicts import DefaultOrderedDict
from lib_python.def_jedi_utils import concat_obs_dict

# globals
ioda_float_type = 'float32'
ioda_int_type = 'int32'
float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]


def main(args):

    args.date = datetime.strptime(args.date, '%Y%m%d%H')

    # read / process files in parallel
    pool_input_01 = args.input
    pool_input_02 = np.arange(len(args.input))
    pool_inputs = [[i, j] for i, j in zip(pool_input_01, pool_input_02)]
    obs_data = {}

    # create a thread pool
    with ProcessPoolExecutor(max_workers=args.threads) as executor:
        for file_obs_data in executor.map(read_input, pool_inputs):
            if not file_obs_data:
                print(f"INFO: non-nominal file skipping")
                continue
            if obs_data:
                concat_obs_dict(obs_data, file_obs_data)
            else:
                obs_data = file_obs_data

    if len(obs_data) == 0:
        print('ERROR: no occultations to write out')
        sys.exit()

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs = {}
    GlobalAttrs['datetimeReference'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {
        'bendingAngle': ['Location'],
        'atmosphericRefractivity': ['Location']
    }

    # write them out
    nlocs = obs_data[('bendingAngle', 'ObsValue')].shape[0]
    DimDict = {'Location': nlocs}
    meta_data_types = def_meta_types()
    for k, v in meta_data_types.items():
        locationKeyList.append((k, v))
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('bendingAngle', 'ObsValue')]['units'] = 'Radians'
    VarAttrs[('bendingAngle', 'ObsError')]['units'] = 'Radians'
    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['units'] = 'N units'
    VarAttrs[('atmosphericRefractivity', 'ObsError')]['units'] = 'N units'
    VarAttrs[('height', 'MetaData')]['units'] = 'm'
    VarAttrs[('latitude', 'MetaData')]['units'] = 'degree_north'
    VarAttrs[('longitude', 'MetaData')]['units'] = 'degree_east'
    VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
    VarAttrs[('sensorAzimuthAngle', 'MetaData')]['units'] = 'degree'
    VarAttrs[('geoidUndulation', 'MetaData')]['units'] = 'm'
    VarAttrs[('earthRadiusCurvature', 'MetaData')]['units'] = 'm'
    VarAttrs[('geopotentialHeight', 'MetaData')]['units'] = 'gpm'
    VarAttrs[('partialBendingAngle', 'MetaData')]['units'] = 'Radians'

    VarAttrs[('bendingAngle', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('bendingAngle', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('bendingAngle', 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('atmosphericRefractivity', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('atmosphericRefractivity', 'PreQC')]['_FillValue'] = int_missing_value

    VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('height', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('geopotentialHeight', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('partialBendingAngle', 'MetaData')]['_FillValue'] = float_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def read_input(input_file):
    """
    Reads/converts input file(s)

    Arguments:

        input_args: an input filename or names
            input_file: The name of file to read

    Returns:

        A dictionary holding the variables (obs_data) needed by the IODA writer
    """
    input_file = input_file[0]
    print("Reading: %s" % input_file)
    ifile = h5.File(input_file, 'r')

    obs_data = get_obs_data(ifile)

    return obs_data


def get_obs_data(ifile):

    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    # get data from file
    file_stamp = str(ifile.attrs['fileStamp'])[2:-1].split('.')
    nlocations = ifile['MSL_alt'].size
    height = ifile['MSL_alt'][:]*10e2
    satelliteC = get_satelliteC(file_stamp[5][0])
    aircraftId = get_aircraftIdentifier(file_stamp[0])
    aircraftIs = get_aircraftInstrument(str(ifile.attrs['aircraftAROInstrument'])[2:-1])
    aircraftTn = get_aircrafttailnumber(str(ifile.attrs['aircraftTailNumber'])[2:-1])
    aircraftAn = get_aircraftAntenna(str(ifile.attrs['aircraftAROAntenna'])[2:-1])
    ascFlag = 0  # descending: from top to bottom
    centerP = get_centerId(str(ifile.attrs['center'])[2:-1])
    impactH = cal_impactHeightRO(ifile['Impact_parm'], ifile.attrs['rfict'])
    geop = geometric2geopotential(ifile.attrs['lat'][0], height)

    # Populate the obs_data dictionary
    # value, ob_error, qualityFlag
    obs_data[('bendingAngle', "ObsValue")] = np.array(ifile['Bend_ang'][:], dtype=ioda_float_type)
    obs_data[('bendingAngle', "ObsError")] = np.array(ifile['Bend_ang_stdv'][:], dtype=ioda_float_type)
    obs_data[('bendingAngle', "PreQC")] = np.full((nlocations), int(str(ifile.attrs['bad'])[2:-1]), dtype=ioda_int_type)

    # value, ob_error, qc
    obs_data[('atmosphericRefractivity', "ObsValue")] = np.array(ifile['Ref'][:], dtype=ioda_float_type)
    obs_data[('atmosphericRefractivity', "ObsError")] = np.array(ifile['Ref_stdv'][:], dtype=ioda_float_type)
    obs_data[('atmosphericRefractivity', "PreQC")] = np.full((nlocations), int(str(ifile.attrs['bad'])[2:-1]), dtype=ioda_int_type)

    # metadata
    obs_data[('latitude', 'MetaData')] = np.array(ifile['Lat'][:], dtype=ioda_float_type)
    obs_data[('longitude', 'MetaData')] = np.array(ifile['Lon'][:], dtype=ioda_float_type)
    obs_data[('dateTime', 'MetaData')] = np.full((nlocations), get_dateTime(ifile))
    obs_data[('impactParameterRO', 'MetaData')] = np.array(ifile['Impact_parm'][:]*10e2, dtype=ioda_float_type)
    obs_data[('impactHeightRO', 'MetaData')] = np.array(impactH, dtype=ioda_float_type)
    obs_data[('height', 'MetaData')] = np.array(height, dtype=ioda_float_type)
    obs_data[('sensorAzimuthAngle', 'MetaData')] = np.array(ifile['Azim'][:], dtype=ioda_float_type)
    obs_data[('sequenceNumber', 'MetaData')] = np.array(ifile['record_number'][:], dtype=ioda_int_type)
    obs_data[('satelliteConstellationRO', 'MetaData')] = np.full((nlocations), satelliteC, dtype=ioda_int_type)
    obs_data[('satelliteTransmitterId', 'MetaData')] = np.full((nlocations), int(file_stamp[5][1:3]), dtype=ioda_int_type)
    obs_data[('aircraftIdentifier', 'MetaData')] = np.full((nlocations), aircraftId, dtype=ioda_int_type)
    obs_data[('aircraftAROInstrument', 'MetaData')] = np.full((nlocations), aircraftIs, dtype=ioda_int_type)
    obs_data[('aircraftTailNumber', 'MetaData')] = np.full((nlocations), aircraftTn, dtype=ioda_int_type)
    obs_data[('aircraftAROAntenna', 'MetaData')] = np.full((nlocations), aircraftAn, dtype=ioda_int_type)
    obs_data[('satelliteAscendingFlag', 'MetaData')] = np.full((nlocations), ascFlag, dtype=ioda_int_type)
    obs_data[('dataProviderOrigin', 'MetaData')] = np.full((nlocations), centerP, dtype=ioda_int_type)
    obs_data[('geoidUndulation', 'MetaData')] = np.full((nlocations), ifile.attrs['rgeoid']*10e2, dtype=ioda_float_type)
    obs_data[('earthRadiusCurvature', 'MetaData')] = np.full((nlocations), ifile.attrs['rfict']*10e2, dtype=ioda_float_type)
    obs_data[('geopotentialHeight', 'MetaData')] = np.array(geop, dtype=ioda_float_type)
    obs_data[('partialBendingAngle', 'MetaData')] = np.array(ifile['Opt_bend_ang'][:], dtype=ioda_float_type)

    return obs_data


def get_dateTime(ifile):

    # do the hokey time structure to time structure
    year = ifile.attrs['year'][0]
    month = ifile.attrs['month'][0]
    day = ifile.attrs['day'][0]
    hour = ifile.attrs['hour'][0]
    minute = ifile.attrs['minute'][0]
    second = ifile.attrs['second'][0]  # non-integer value
    second = round(second)

    # get string date, translate to a datetime object, then offset from epoch
    dtg = ("%4i-%.2i-%.2iT%.2i:%.2i:%.2iZ" % (year, month, day, hour, minute, second))
    this_datetime = datetime.strptime(dtg, "%Y-%m-%dT%H:%M:%SZ")
    time_offset = round((this_datetime - epoch).total_seconds())
    dateTime = np.int64(time_offset)

    return dateTime


def def_meta_data():

    meta_data_keys = {
        # "qualityFlags": 'radioOccultationDataQualityFlags',
        "latitude": 'latitude',
        "longitude": 'longitude',
        "impactParameterRO": 'impactParameterRO',
        "impactHeightRO": 'impactHeightRO',
        "height": 'geometricAltitude',
        "sensorAzimuthAngle": 'bearingOrAzimuth',
        "sequenceNumber": 'recordNumber',
        "satelliteConstellationRO": 'satelliteClassification',
        "satelliteTransmitterId": 'platformTransmitterIdNumber',
        "aircraftIdentifier": 'aircraftIdentifier',
        "aircraftAROInstrument": 'aircraftAROInstrument',
        "aircraftTailNumber": 'aircraftAROTailNumber',
        "aircraftAROAntenna": 'aircraftAROSAntenna',
        "satelliteAscendingFlag": 'originalOccultationAscendingDescendingFlag',
        "dataProviderOrigin": 'centre',
        "geoidUndulation": 'geoidUndulation',
        "earthRadiusCurvature": 'earthLocalRadiusOfCurvature',
        "geopotentialHeight": 'geopotentialHeight',
        "partialBendingAngle": 'partialBendingAngle',
    }

    return meta_data_keys


def def_meta_types():

    meta_data_types = {
        "latitude": "float",
        "longitude": "float",
        "dateTime": "long",
        'impactParameterRO': 'float',
        'impactHeightRO': 'float',
        'height': 'float',
        "sensorAzimuthAngle": 'float',
        "sequenceNumber": 'integer',
        "satelliteConstellationRO": 'integer',
        "satelliteTransmitterId": 'integer',
        "aircraftIdentifier": 'integer',
        "aircraftAROInstrument": 'integer',
        "aircraftTailNumber": 'integer',
        "aircraftAROAntenna": 'integer',
        "satelliteAscendingFlag": 'integer',
        "dataProviderOrigin": 'integer',
        "geoidUndulation": 'float',
        "earthRadiusCurvature": 'float',
        "geopotentialHeight": 'float',
        "partialBendingAngle": 'float',
        # "qualityFlags": 'integer',
    }

    return meta_data_types


def get_centerId(center):
    if center == 'IGPP/SIO/UCSD':
        centerId = 61  # Id for IGPP/SIO/UCSD (provisionally)
    return centerId


def get_satelliteC(satelliteC):
    if satelliteC == 'G':
        satelliteC_ID = 401
    elif satelliteC == 'R':
        satelliteC_ID = 402
    elif satelliteC == 'E':
        satelliteC_ID = 403
    return satelliteC_ID


def get_aircraftIdentifier(arcftId):
    if arcftId == 'N49T':
        arcftid = 790
    elif arcftId == 'N42T':
        arcftid = 791
    elif arcftId == 'N43T':
        arcftid = 792
    elif arcftId == 'R44T':
        arcftid = 793
    elif arcftId == 'R45T':
        arcftid = 794
    elif arcftId == 'R46T':
        arcftid = 795
    elif arcftId == 'R47T':
        arcftid = 796
    return arcftid


def get_aircraftInstrument(arcftIs):
    if arcftIs == 'SEPT ASTERXU':
        arcftis = 1
    elif arcftIs == 'SEPT ASTERXSB3':
        arcftis = 2
    return arcftis


def get_aircrafttailnumber(tailN):
    if tailN == 'NOAA2':
        tailnumber = 42
    if tailN == 'NOAA3':
        tailnumber = 43
    if tailN == 'NOAA9':
        tailnumber = 49
    elif tailN == 'AF300':
        tailnumber = 5300
    elif tailN == 'AF303':
        tailnumber = 5303
    elif tailN == 'AF304':
        tailnumber = 5304
    elif tailN == 'AF305':
        tailnumber = 5305
    elif tailN == 'AF306':
        tailnumber = 5306
    elif tailN == 'AF307':
        tailnumber = 5307
    elif tailN == 'AF308':
        tailnumber = 5308
    elif tailN == 'AF309':
        tailnumber = 5309
    return tailnumber


def get_aircraftAntenna(arcftAn):
    if arcftAn == 'AERAT1675_381 NONE':
        arcftan = 1
    elif arcftAn == 'NOV42G1215A NONE':
        arcftan = 2
    return arcftan


def cal_impactHeightRO(impactparam, rfict):
    impactheight = (impactparam - rfict)*10e2
    return impactheight


def geometric2geopotential(dlat, height):
    # ABOUT: 28.12.2016: vectorized for speed
    # Adapted from Pawel's geometric2geopotential matlab function
    # geom2geop converts geometric to geopotential height expressed in
    # geopotential meters
    #
    # Somigliana's Equation is defined for normal gravity on the ellipsoid
    # while geopotential height is strictly relative to the geoid.
    #
    # 1) if input height wrt ellipsoid -> output hgeop wrt ellipsoid
    # Geoid undulation need to be substracted from hgeop to compute
    # geopotential height as the output is not strictly a geopotential height.
    #
    # 2) if input height wrt geoid -> output hgeop wrt geoid
    # The output is a geopotential height relative to the geoid.
    # Note that Somagliana's equation is strictly based on computations wrt ellipsoid,
    # but the error in assuming these for conversions wrt geoid is small.
    #
    # Reference ellipsoid: WGS84
    #
    # Input data:
    #               lat:    geodetic latitude                   [deg]
    #               height: geometric height                    [m]
    # Outputs:
    #               hgeop:  geopotential height                 [m]

    # CONSTANTS
    # equatorial gravity
    g_equat = 9.7803253359

    # Somagliana's constant
    k_somig = 1.931853e-3

    # gravitational ratio
    gm_ratio = 0.003449787

    # WGS84 semi-major axis [m]
    a_earth = 6378137

    # WGS84 semi-minor axis [m]
    b_earth = 6356752.3142

    # WGS84 flattening
    f_earth = (a_earth - b_earth)/a_earth

    # WGS84 power eccentricy (e^2)
    e2_earth = (a_earth**2 - b_earth**2)/a_earth**2

    # WMO gravity [m/s2]
    g0 = 9.80665

    # HANDLE INPUT VARIABLES
    # Define dimension of atmospheric profile
    ndim = len(height)

    # convert latitude in degree to radians
    pi = np.arccos(-1.0)
    deg2rad = pi/180.0
    lat = dlat*deg2rad

    # DERIVE GRAVITY
    # 1.1. Calculate effective radius [m]
    R_eff = a_earth/(1 + f_earth + gm_ratio - 2.*f_earth*np.sin(lat)**2)

    # Effective radius of Earth for geopotential height conversion following
    # Somigliana's equation relative to WGS-84 reference ellipsoid
    # 1.2 Calculate gravity - normal gravity on surface of ellipsoid
    g = g_equat*(1 + k_somig*np.sin(lat)**2)/np.sqrt(1 - e2_earth*np.sin(lat)**2)

    # Geopotential height
    hgeop = (g/g0)*(np.dot(R_eff, height)/(R_eff + height))

    return hgeop


if __name__ == "__main__":

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the GNSS-ARO data from NETCDF file'
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of NETCDF GNSS-ARO observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="full path and name of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             '(default: %(default)s)',
        type=int, default=1)

    args = parser.parse_args()
    main(args)
