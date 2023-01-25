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
from pathlib import Path
from itertools import repeat
import netCDF4 as nc

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

from eccodes import *

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

def convertDayOfYearToEpoch(year,ddd,hh,minu):
  datein = datetime.datetime.strptime('{}-{}'.format(year, ddd), '%Y-%j').date()
  datefromddd = datein.strftime('%Y%m%d')
  mm = int(datein.strftime('%m'))
  dd = int(datein.strftime('%d')) 
  epoch_default = datetime.datetime(1970, 1, 1, 0, 0)
  epoch_file = datetime.datetime(year,mm,dd,hh,minu)
  epoch = (epoch_file-epoch_default).total_seconds()
  return epoch


def get_gnsssatclass(gnssclassid):
    if gnssclassid == 'G':
        gnsssatclass_ID = 401 
    elif gnssclassid == 'R':
        gnsssatclass_ID = 402
    elif gnssclassid == 'E':
        gnsssatclass_ID = 403
    return gnsssatclass_ID


def get_ascendingflag(ascendingflag):
  # rising (from bottom -troposphere to top -vacuum) and setting (from top to bottom) occultation
  if ascendingflag == 'R':
    ascendingflagid = 0
  elif ascendingflag == 'S':
    ascendingflagid = 1
  return ascendingflagid


def get_aircraftsatelliteIdentifier(occultingsatID):  #TODO: check this id for the aircraft
  if occultingsatID == 'N49T':
    occultingsatid = 1
  if occultingsatID == 'C130':
    occultingsatid = 2
  return occultingsatid


def get_aircraftIdentifier(occultingsatIS): #TODO: check this id for the intrument
  if occultingsatIS == 'N49T':
    occultingsatis = 1
  if occultingsatIS == 'C130':
    occultingsatis = 2
  return occultingsatis


def cal_impactHeightRO(impactparam, rfict):
  impactheight = (impactparam - rfict)*10e2 # km to m
  return impactheight


def get_obs_data(input_files):
    """
    Get data from input files
    """
    # Build up arrays in loop over input_files
    dateTime                         = np.asarray([])
    lat                              = np.asarray([])
    tpLat                            = np.asarray([])
    lon                              = np.asarray([])
    time                             = np.asarray([])
    sequenceNumber                   = np.asarray([])
    satelliteConstellationRO         = np.asarray([])
    satelliteTransmitterId           = np.asarray([])
    aircraftsatelliteIdentifier      = np.asarray([])
    aircraftIdentifier               = np.asarray([])
    satelliteAscendingFlag           = np.asarray([])
    dataProviderOrigin               = np.asarray([])
    height                           = np.asarray([])
    impactParameterRO                = np.asarray([])
    impactHeightRO                   = np.asarray([])
    sensorAzimuthAngle               = np.asarray([])
    geoidUndulation                  = np.asarray([])
    earthRadiusCurvature             = np.asarray([])
    atmosphericRefractivity_obserror = np.asarray([])
    bendingAngle_obserror            = np.asarray([])
    atmosphericRefractivity_obsvalue = np.asarray([])
    bendingAngle_obsvalue            = np.asarray([])

    nrec = 1
    print('Reading files and variables')
    for full_file_name in np.sort(input_files):
      file_name= full_file_name.split('/')[-1]
      # Get data from file name stamp
      sfil            = file_name.split('_')[1].split('.')
      aircraft_id     = sfil[0]
      year            = int(sfil[1])
      ddd             = int(sfil[2])
      hh              = int(sfil[3])
      minu            = int(sfil[4])
      gnssClassId     = get_gnsssatclass(sfil[5][0])
      referenceGnssId = int(sfil[5][1:3])
      ascendingFlag   = get_ascendingflag(sfil[5][3])
      occultingSatId  = get_aircraftsatelliteIdentifier(sfil[0])
      dateEpoch       = convertDayOfYearToEpoch(year,ddd,hh,minu)

      # Read files
      h          = h5.File(full_file_name, 'r')
      nlocations = h['MSL_alt'].size
      tplat      = np.full((nlocations), h.attrs['lat'][0], dtype=ioda_float_type)
      recordN    = np.array(np.repeat(nrec,nlocations), dtype=np.int64)
      nrec       = nrec + 1
      daT        = np.full((nlocations), dateEpoch, dtype=ioda_int_type)
      Tm         = np.full((nlocations), -999, dtype=ioda_int_type) #TODO: need to calculate the offset
      gnssSatid  = np.full((nlocations), gnssClassId, dtype=ioda_int_type)
      refSatid   = np.full((nlocations), referenceGnssId, dtype=ioda_int_type)
      occSatid   = np.full((nlocations), occultingSatId, dtype=ioda_int_type)
      occSatis   = np.full((nlocations), -999, dtype=ioda_int_type)
      assendingF = np.full((nlocations), ascendingFlag, dtype=ioda_int_type)
      procC      = np.full((nlocations), 60, dtype=ioda_int_type) # 60 for UCAR #TODO: add an appropriate identifier for the process center
      impactP    = h['Impact_parm'][:]*10e2 # km to m
      impactH    = cal_impactHeightRO(h['Impact_parm'], h.attrs['rfict'])
      geoidH     = np.full((nlocations), h.attrs['rgeoid']*10e2, dtype=ioda_int_type) # km to m
      earthR     = np.full((nlocations), h.attrs['rfict']*10e2, dtype=ioda_int_type) # km to m
      malt       = h['MSL_alt'][:]*10e2 # km to m

      # append variable values from each processor
      dateTime                         = np.append( dateTime,                         daT )
      lat                              = np.append( lat,                              h['Lat'] )
      tpLat                            = np.append( tpLat,                            tplat )
      lon                              = np.append( lon,                              h['Lon'] )
      height                           = np.append( height,                           malt )
      sensorAzimuthAngle               = np.append( sensorAzimuthAngle,               h['Azim'] )
      time                             = np.append( time,                             Tm )
      sequenceNumber                   = np.append( sequenceNumber,                   recordN )
      satelliteConstellationRO         = np.append( satelliteConstellationRO,         gnssSatid )
      satelliteTransmitterId           = np.append( satelliteTransmitterId,           refSatid )
      aircraftsatelliteIdentifier      = np.append( aircraftsatelliteIdentifier,      occSatid )
      aircraftIdentifier               = np.append( aircraftIdentifier,               occSatis )
      satelliteAscendingFlag           = np.append( satelliteAscendingFlag,           assendingF )
      dataProviderOrigin               = np.append( dataProviderOrigin,               procC )
      impactParameterRO                = np.append( impactParameterRO,                impactP )
      impactHeightRO                   = np.append( impactHeightRO,                   impactH )
      geoidUndulation                  = np.append( geoidUndulation,                  geoidH )
      earthRadiusCurvature             = np.append( earthRadiusCurvature,             earthR )
      atmosphericRefractivity_obserror = np.append( atmosphericRefractivity_obserror, h['Ref_stdv'] )
      bendingAngle_obserror            = np.append( bendingAngle_obserror,            h['Bend_ang_stdv'] )
      atmosphericRefractivity_obsvalue = np.append( atmosphericRefractivity_obsvalue, h['Ref'] )
      bendingAngle_obsvalue            = np.append( bendingAngle_obsvalue,            h['Bend_ang'] )

    # Populate the obs_data dictionary
    obs_data[('latitude', 'MetaData')]                   = assign_values(lat)
    obs_data[('latitudeTP', 'MetaData')]                 = assign_values(tpLat)
    obs_data[('longitude', 'MetaData')]                  = assign_values(lon)
    obs_data[('time', 'MetaData')]                       = assign_values(time)
    obs_data[('dateTime', 'MetaData')]                   = assign_values(dateTime)
    obs_data[('height', 'MetaData')]                     = assign_values(height)
    obs_data[('sensorAzimuthAngle', 'MetaData')]         = assign_values(sensorAzimuthAngle)
    obs_data[('sequenceNumber', 'MetaData')]             = assign_values(sequenceNumber)
    obs_data[('satelliteConstellationRO', 'MetaData')]   = assign_values(satelliteConstellationRO)
    obs_data[('satelliteTransmitterId', 'MetaData')]     = assign_values(satelliteTransmitterId)
    obs_data[('aircraftsatelliteIdentifier', 'MetaData')]= assign_values(aircraftsatelliteIdentifier)
    obs_data[('aircraftIdentifier', 'MetaData')]         = assign_values(aircraftIdentifier)
    obs_data[('satelliteAscendingFlag', 'MetaData')]     = assign_values(satelliteAscendingFlag)
    obs_data[('dataProviderOrigin', 'MetaData')]         = assign_values(dataProviderOrigin)
    obs_data[('impactParameterRO', 'MetaData')]          = assign_values(impactParameterRO)
    obs_data[('impactHeightRO', 'MetaData')]             = assign_values(impactHeightRO)
    obs_data[('geoidUndulation', 'MetaData')]            = assign_values(geoidUndulation)
    obs_data[('earthRadiusCurvature', 'MetaData')]       = assign_values(earthRadiusCurvature)
    obs_data[('atmosphericRefractivity', "ObsError")]    = assign_values(atmosphericRefractivity_obserror)
    obs_data[('bendingAngle', "ObsError")]               = assign_values(bendingAngle_obserror)
    obs_data[('atmosphericRefractivity', "ObsValue")]    = assign_values(atmosphericRefractivity_obsvalue)
    obs_data[('bendingAngle', "ObsValue")]               = assign_values(bendingAngle_obsvalue)

    return obs_data


def def_meta_data():

    meta_data_keys = {
        "qualityFlags": 'radioOccultationDataQualityFlags',
        "geoidUndulation": 'geoidUndulation',
        "sensorAzimuthAngle": 'bearingOrAzimuth',
        # "timeIncrement": 'timeIncrement',
        "earthRadiusCurvature": 'earthLocalRadiusOfCurvature',
        "satelliteIdentifier": 'satelliteIdentifier',
        "satelliteInstrument": 'satelliteInstruments',
        "dataProviderOrigin": 'centre',
        "satelliteTransmitterId": 'platformTransmitterIdNumber',
        "satelliteConstellationRO": 'satelliteClassification',
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
        "qualityFlags": 'integer',
        "geoidUndulation": 'float',
        "earthRadiusCurvature": 'float',
        "satelliteIdentifier": 'integer',
        "satelliteInstrument": 'integer',
        "dataProviderOrigin": 'string',
        "satelliteTransmitterId": 'integer',
        "satelliteConstellationRO": 'integer',
    }

    return meta_data_types


def assign_values(data):
    if data.dtype == float:
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
        return np.array(data, dtype=ioda_int_type)


def main(args):
    h0 = time.time()

    print('Starting '+__name__)

    prefix = 'atmPrf_'
    ext = '.nc'
    fname = prefix+'*'+ext

    obsoutfiles = []
    for files in os.listdir(args.input):
      if fnmatch.fnmatch(files, fname):
        obsoutfiles.append(args.input+'/'+files)

    obs_data = get_obs_data(obsoutfiles)

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

    VarAttrs[('bendingAngle', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('bendingAngle', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('bendingAngle', 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('atmosphericRefractivity', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('atmosphericRefractivity', 'PreQC')]['_FillValue'] = int_missing_value

    VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('height', 'MetaData')]['_FillValue'] = float_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)

    hf = time.time()
    print('Time elapsed: ',hf  - h0)
    print('Finished '+__name__+' successfully')

if __name__ == "__main__":

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the GNSS-RO data from NETCDF file'
            ' convert into IODA formatted output files. '
            ' Multiple files are appended')
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


    args = parser.parse_args()
    main(args)
