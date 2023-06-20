# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import sys

sys.path.append('/work/noaa/da/eliu/JEDI-iodaconv/ioda-bundle/build/lib/')
sys.path.append('/work/noaa/da/eliu/JEDI-iodaconv/ioda-bundle/build/lib/pyiodaconv/')
sys.path.append('//work/noaa/da/eliu/JEDI-iodaconv/ioda-bundle/build/lib/python3.9/pyioda/')

import numpy as np
import numpy.ma as ma
import bufr
import calendar
import time
import math 
#import lib_python.ioda_conv_engines as iconv
#from lib_python.orddicts import DefaultOrderedDict
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict
import datetime
from collections import namedtuple

DATA_PATH   = '/work/noaa/da/eliu/JEDI-iodaconv/run_iasi/testinput/2021080100/gdas.t00z.mtiasi.tm00.bufr_d'

def Compute_SensorScanPosition(fov, sensor):

    if sensor == 'iasi':
       # IASI: remap fovn from 1-120 to 1-60/1-60
       # Original FOVN
       # 1 3 5 7 ...... 117 119
       # 2 4 6 8 ...... 118 120
       # Remappedl FOVN
       # 1 2 3 4 ......  59  60 
       # 1 2 3 4 ......  59  60 

       nfov = fov.shape[0]
       sensor_scan_position = np.float32(np.int32((fov - 1 ) / 2 + 1))
    else:
       print('Compute_SensorScanPosition: not implemented for ', sensor, ' yet') 

    return sensor_scan_position

def Compute_SensorViewAngle(fov, scanpos, sensor):

    # IASI: compute sensor scan angle
    if sensor == 'iasi':
       start      = -48.330
       step       =   3.334
       stepAdjust =   0.625
 
       nfov              = fov.shape[0]
       tmp               = np.where((scanpos %2 == 1), stepAdjust, -stepAdjust)
       sensor_view_angle = start + np.float32(np.int32((fov-1)/4)) * step + tmp
    else: 
       print('Compute_SensorViewAngle: not implemented for ', sensor, ' yet') 

    return sensor_view_angle

def ConvertScaledRadiance_to_Radiance(channum, begchan, endchan, cscale, srad):

    float_missing_value = srad.fill_value 

    nlocs, nchans = srad.shape
    nbands        = begchan.shape[1]

    channum  = channum.flatten()
    srad     = srad.flatten()
    begchan  = begchan.flatten()
    endchan  = endchan.flatten()
    cscale   = cscale.flatten()
    nobs     = len(srad) 
    radiance = np.full_like(srad, float_missing_value, dtype=float) 
    iloc     = np.int32(np.floor(np.arange(nobs) / nchans))

    for ibnd in range(nbands):
        bandoffset    = iloc * nbands + ibnd
        channel_mask  = (channum >= begchan[bandoffset]) & (channum <= endchan[bandoffset])
        valid_indices = np.where(channel_mask)[0]
    
        valid_srad   = srad[valid_indices]
        valid_cscale = cscale[bandoffset][valid_indices]
    
        valid_scalefactor       = 10 ** -np.array(valid_cscale, dtype=float)
        radiance[valid_indices] = valid_srad * valid_scalefactor

    radiance = radiance.reshape(nlocs, nchans)

    return radiance 

def bufr_to_ioda():

    # Define instrument sensor and satellite information
    instrument = namedtuple('instrument', ['name', 'sensor_id', 'satellite_name', 'satellite_id'])

    instruments = [
        instrument('iasi', 221, 'metop-a', 4),
        instrument('iasi', 221, 'metop-b', 3),
        instrument('iasi', 221, 'metop-c', 5)
    ]
    print('Instrument Info ... ')
    print(instruments)
    print('Total number of instrument = ', len(instruments))

    for instrument in instruments:
        print('Sensor Name = ', instrument.name)
        print('Sensor ID   = ', instrument.sensor_id)
        print('Satellite   = ', instrument.satellite_name)
        print('Satellite   = ', instrument.satellite_id)

    print('Sensor Name = ', instruments[0].name)
    print('Sensor ID   = ', instruments[0].sensor_id)
    print('Satellite   = ', instruments[0].satellite_name)
    print('Satellite   = ', instruments[0].satellite_id)

    sensor_name = instruments[0].name
    print('sensor name = ', sensor_name)

    # ============================================
    # Make the QuerySet for all the data we want
    # ============================================
    start_time = time.time()

    print('Making QuerySet ...')
    q = bufr.QuerySet()

    # MetaData
    q.add('latitude',                   '*/CLATH')
    q.add('longitude',                  '*/CLONH')
    q.add('satelliteId',                '*/SAID') 
    q.add('year',                       '*/YEAR') 
    q.add('month',                      '*/MNTH') 
    q.add('day',                        '*/DAYS') 
    q.add('hour',                       '*/HOUR') 
    q.add('minute',                     '*/MINU') 
    q.add('second',                     '*/SECO') 
    q.add('sensorId',                   '*/SIID[1]') 
    q.add('scanLineNumber',             '*/SLNM') 
    q.add('gqisFlagQual',               '*/QGFQ') 
    q.add('fieldOfViewNumber',          '*/FOVN') 
    q.add('solarZenithAngle',           '*/SOZA') 
    q.add('solarAzimuthAngle',          '*/SOLAZI') 
    q.add('sensorZenithAngle',          '*/SAZA') 
    q.add('sensorAzimuthAngle',         '*/BEARAZ') 
    q.add('heightOfStation',            '*/SELV') 
    q.add('fractionOfClearPixelsInFov', '*/IASIL1CS/FCPH') 
    q.add('sensorChannelNumber',        '*/IASICHN/CHNM') 

    # ObsValue
    q.add('scaledSpectralRadiance',     '*/IASICHN/SCRA')
    q.add('startChannel',               '*/IASIL1CB/STCH')
    q.add('endChannel',                 '*/IASIL1CB/ENCH')
    q.add('scaleFactor',                '*/IASIL1CB/CHSF')

    end_time = time.time()
    running_time = end_time - start_time
    print('Running time for making QuerySet : ', running_time, 'seconds')

    # ==============================================================
    # Open the BUFR file and execute the QuerySet to get ResultSet
    # Use the ResultSet returned to get numpy arrays of the data
    # ==============================================================
    start_time = time.time()

    print('Executing QuerySet to get ResultSet ...')
    with bufr.File(DATA_PATH) as f:
       r = f.execute(q)
 
    print(' ... Executing QuerySet: get metadata ...')
    # Variable with Dimension: [Location]
    satid     = r.get('satelliteId')  
    instid    = r.get('sensorId') 
    year      = r.get('year')
    month     = r.get('month')
    day       = r.get('day')
    hour      = r.get('hour')
    minute    = r.get('minute')
    second    = r.get('second')
    lat       = r.get('latitude')
    lon       = r.get('longitude')
    fovn      = r.get('fieldOfViewNumber')
    scanline  = r.get('scanLineNumber')
    qcflag    = r.get('gqisFlagQual')
    solzenang = r.get('solarZenithAngle')
    solaziang = r.get('solarAzimuthAngle')
    satzenang = r.get('sensorZenithAngle')
    sataziang = r.get('sensorAzimuthAngle')
    satheight = r.get('heightOfStation')

    print(' ... Executing QuerySet: get cluster data ...')
    # Variable with Dimension: [Location, Cluster]
    fcph      = r.get('fractionOfClearPixelsInFov')

    print(' ... Executing QuerySet: get obs ...')
    # Variable with Dimension: [Location, Channel] 
    channum   = r.get('sensorChannelNumber')
    scaledrad = r.get('scaledSpectralRadiance', type='float')

    print(' ... Executing QuerySet: get obs scaling info ...')
    # Variable with Dimension: [Location, Band] 
    begchan   = r.get('startChannel')
    endchan   = r.get('endChannel')
    scalecoef = r.get('scaleFactor')

    print(' ... Executing QuerySet: get datatime ...')
    # DateTime: seconds scine Epoch time
    timestamp = r.get_datetime('year','month','day','hour','minute','second')

    print(' ... Executing QuerySet: Done!')

    print(' ... Executing QuerySet: Check BUFR variable generic dimension and type ...')
    # Check BUFR variable generic dimension and type
    print('     year      shape = ', year.shape)
    print('     qcflag    shape = ', qcflag.shape)
    print('     fcph      shape = ', fcph.shape)
    print('     begchan   shape = ', begchan.shape)
    print('     endchan   shape = ', endchan.shape)
    print('     scalecoef shape = ', scalecoef.shape)
    print('     channum   shape = ', channum.shape)
    print('     scaledrad shape = ', scaledrad.shape)

    print('     scaledrad type  = ', scaledrad.dtype)  
    print('     scalecoef type  = ', scalecoef.dtype)  
    print('     begchan   type  = ', begchan.dtype)  
    print('     channum   type  = ', channum.dtype)  
    print('     timestamp type  = ', timestamp.dtype)  
    print('     lat       type  = ', lat.dtype)  
    print('     fovn      type  = ', fovn.dtype)  
    print('     scanline  type  = ', scanline.dtype)  
    print('     satheight type  = ', satheight.dtype)  
    print('     qcflag    type  = ', qcflag.dtype)  
    print('     fcph      type  = ', fcph.dtype)  

    end_time = time.time()
    running_time = end_time - start_time
    print('Running time for executing QuerySet to get ResultSet : ', running_time, 'seconds')

    # =========================
    # Create derived variables
    # =========================
    start_time = time.time()

    print('Creating derived variables ...')

    # Set dimensions for derived variables
    nlocs = lat.shape[0]
 
    # Fraction of Clear Pixel in FOV 
    fcph = fcph * 0.01  

    print(' ... Creating derived variable - sensor scan position ...')
    # Sensor Scan Positione
    scanpos = Compute_SensorScanPosition(fovn, sensor_name)
    print('     fovn (orig) min/max = ', fovn.min(), fovn.max())
    print('     scanpos     min/max = ', scanpos.min(), scanpos.max())

    print(' ... Creating derived variable - sensor view position ...')
    # Sensor View Angle 
    viewang = Compute_SensorViewAngle(fovn, scanpos, sensor_name)
    print('     sensorViewAngle min/max = ', viewang.min(), viewang.max())

    print(' ... Creating derived variable - spectral radiance ...')
    # Radiance
    radiance = ConvertScaledRadiance_to_Radiance(channum, begchan, endchan, scalecoef, scaledrad)
    print('     radiance min/max = ', radiance.min(), radiance.max())

    end_time = time.time()
    running_time = end_time - start_time
    print('Running time for creating derived variables : ', running_time, 'seconds')

    # ==================================================
    # Split data and save output based on satellite id 
    # ==================================================
    print('Split data based on satellite id and Write IODA output')

    # Find unique satellite identifiers in data to process
    unique_satids = np.unique(satid)
    print(' ... Number of Unique satellite identifiers: ', len(unique_satids))
    print(' ... Unique satellite identifiers: ', unique_satids)

    print(' ... Loop through unique satellite identifier ... : ', unique_satids)
    for sat in unique_satids.tolist():
        start_time = time.time()

        # Find matched instrument from instrument namedtuple
        matched = list(filter(lambda instrument: instrument.satellite_id == sat, instruments))
        print(' ... sat         = ', sat)
        print(' ... instruments = ', instruments)
        print(' ... Matched instrument = ', matched)
        satellite_id = list(map(lambda instrument: instrument.satellite_id, matched))
        satellite    = list(map(lambda instrument: instrument.satellite_name, matched))
        sensor       = list(map(lambda instrument: instrument.name, matched))
        print(' ... Matched satellite_id = ', satellite_id)
        print(' ... Matched satellite    = ', satellite)
        print(' ... Matched sensor       = ', sensor)
        satinst      = sensor[0]+'_'+satellite[0]

        # Define a boolean mask to subset data from the original data object
        mask       = satid == sat
        lon2       = lon[mask]
        lat2       = lat[mask]
        timestamp2 = timestamp[mask].astype(np.int64)
        satid2     = satid[mask]
        instid2    = instid[mask]
        scanline2  = scanline[mask]
        qcflag2    = qcflag[mask]
        fovn2      = fovn[mask]
        scanpos2   = scanpos[mask]
        solzenang2 = solzenang[mask]
        solaziang2 = solaziang[mask]
        satzenang2 = satzenang[mask]
        sataziang2 = sataziang[mask]
        viewang2   = viewang[mask].astype(np.float32)
        satheight2 = satheight[mask].astype(np.float32)
        channum2   = channum[mask]
        fcph2      = fcph[mask].astype(np.float32)
        radiance2  = radiance[mask].astype(np.float32)

        # Write the data to IODA file
        # Create the variable creation parameters.
        OUTPUT_PATH = '/work/noaa/da/eliu/JEDI-iodaconv/run_iasi/testrun_py2/2021080100/gdas.t00z.'+satinst+'.nc'
        print(' ... ... Create output file ...', OUTPUT_PATH)

        # Get defult missing values from IODA Engines
        float_missing_value = iconv.get_default_fill_val(np.float32)
        int_missing_value   = iconv.get_default_fill_val(np.int32)
        long_missing_value  = iconv.get_default_fill_val(np.int64)

        # Create parameters: set fill values (consistent with the input data)
        print(' ... ... Set creation parameters, set fill value')
        float32_fill_value  = lat2.fill_value
        int32_fill_value    = fovn2.fill_value
        int64_fill_value    = timestamp2.fill_value.astype(np.int64) 
        print(' ... ... Checking float32_fill_value = ', float32_fill_value)
        print(' ... ... Checking int32_fill_value   = ', int32_fill_value)
        print(' ... ... Checking int64_fill_value   = ', int64_fill_value)

        # Create the dimensions for subset
        print(' ... ... Create dimensions')
        num_location = lat2.shape[0]
        num_channel  = channum2.shape[1]
        num_cluster  = fcph2.shape[1]

        print('         num_location = ', num_location)
        print('         num_channel  = ', num_channel)
        print('         num_cluster  = ', num_cluster)

        # Setup output data to IODA
        print(' ... ... Initialize output variable dictionary to IODA')
        VarOut = {}
        print(' ... ... Setup variable output to IODA')
        VarOut[('dateTime',                   'MetaData')] = timestamp2
        VarOut[('latitude',                   'MetaData')] = lat2
        VarOut[('longitude',                  'MetaData')] = lon2
        VarOut[('satelliteIdentifier',        'MetaData')] = satid2
        VarOut[('instrumentIdentifier',       'MetaData')] = instid2
        VarOut[('heightOfStation',            'MetaData')] = satheight2
        VarOut[('fieldOfViewNumber',          'MetaData')] = fovn2
        VarOut[('scanLineNumber',             'MetaData')] = scanline2
        VarOut[('qualityFlags',               'MetaData')] = qcflag2
        VarOut[('solarZenithAngle',           'MetaData')] = solzenang2
        VarOut[('solarAzimuthAngle',          'MetaData')] = solaziang2
        VarOut[('sensorZenithAngle',          'MetaData')] = satzenang2
        VarOut[('sensorAzimuthAngle',         'MetaData')] = sataziang2
        VarOut[('sensorViewAngle',            'MetaData')] = viewang2
        VarOut[('sensorScanPosition',         'MetaData')] = scanpos2
        VarOut[('fractionOfClearPixelsInFov', 'MetaData')] = fcph2
        VarOut[('sensorChannelNumber',        'MetaData')] = channum2
        VarOut[('radiance',                   'ObsValue')] = radiance2

        # Setup attributes for IODA
        locationKeyList = [
            ("latitude",  "float"),
            ("longitude", "float"),
            ("dateTime",  "long")
        ]

        print(' ... ... Initialize attribute dictionaries to IODA')
        # Initialize attribute dictionaries
        GlobalAttrs = {}
        VarAttrs    = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        VarDims     = {}
        DimDict     = {}

        print(' ... ... Setup global attributes to IODA')
        # Define keys for global attributes
        GlobalAttrs['platformCommonName'] = "IASI"
        GlobalAttrs['platformLongDescription'] = "MTYP 021-241 IASI 1C RADIANCES (VARIABLE CHNS) (METOP)"

        print(' ... ... Setup dictionary for global dimensions to IODA ...')
        # Define keys for variable dimensions  
        DimDict = {
#           'Location': num_location, 
#           'Channel' : num_channel, 
#           'Cluster' : num_cluster, 
            'Location': np.arange(0, num_location), 
            'Channel' : np.array(channum2[0, :]), 
            'Cluster' : np.arange(0, num_cluster) 
        }

        print(' ... ... Setup dictionary for variable dimensions to IODA...')
        # Define keys for variables 
        VarDims = {
            'fractionOfClearPixelsInFov' : ['Location', 'Cluster'],
            'sensorChannelNumber'        : ['Location', 'Channel'], 
            'radiance'                   : ['Location', 'Channel'] 
        }

        print(' ... ... Setup dictionary for variable attributes (units, fill values and data description/long name) to IODA ...')
        # Define keys for variables attributes 
        print(' ... ... Create variables: name, type, dimension and attributes')
        # Datetime
        VarAttrs[('dateTime', 'MetaData')]['units']      = 'seconds since 1970-01-01T00:00:00Z' 
        VarAttrs[('dateTime', 'MetaData')]['_FillValue'] = int64_fill_value 
        VarAttrs[('dateTime', 'MetaData')]['long_name']  = 'Elapsed seconds since January 1, 1970 at midnight UTC time minus the leap seconds' 

        # Longitude
        VarAttrs[('longitude', 'MetaData')]['long_name']   = 'Longitude' 
        VarAttrs[('longitude', 'MetaData')]['_FillValue']  = float32_fill_value 
        VarAttrs[('longitude', 'MetaData')]['valid_range'] = np.array([-180, 180], dtype=np.float32) 

        # Latitude 
        VarAttrs[('latitude', 'MetaData')]['long_name']   = 'Latitude' 
        VarAttrs[('latitude', 'MetaData')]['_FillValue']  = float32_fill_value 
        VarAttrs[('latitude', 'MetaData')]['valid_range'] = np.array([-90, 90], dtype=np.float32) 

        # Satellite Identifier 
        VarAttrs[('satelliteIdentifier', 'MetaData')]['long_name']  = 'Satellite Identifier' 
        VarAttrs[('satelliteIdentifier', 'MetaData')]['_FillValue'] = int32_fill_value 
  
        # Instrument Identifier  
        VarAttrs[('instrumentIdentifier', 'MetaData')]['long_name']  = 'Satellite Instrument Identifier' 
        VarAttrs[('instrumentIdentifier', 'MetaData')]['_FillValue'] = int32_fill_value 
 
        # Scan Line Number  
        VarAttrs[('scanLineNumber', 'MetaData')]['long_name']  = 'Scan Line Number' 
        VarAttrs[('scanLineNumber', 'MetaData')]['_FillValue'] = int32_fill_value 
 
        # Quality Flags  
        VarAttrs[('qualityFlags', 'MetaData')]['long_name']  = 'Individual IASI-System Quality Flag' 
        VarAttrs[('qualityFlags', 'MetaData')]['_FillValue'] = int32_fill_value 
 
        # Field-of-View Number  
        VarAttrs[('fieldOfViewNumber', 'MetaData')]['long_name']  = 'Field of View Number' 
        VarAttrs[('fieldOfViewNumber', 'MetaData')]['_FillValue'] = int32_fill_value 
 
        # Scan Position  
        VarAttrs[('sensorScanPosition', 'MetaData')]['long_name'] = 'Field of View Number' 
        VarAttrs[('sensorScanPosition', 'MetaData')]['_FillValue'] = float32_fill_value 
 
       # Solar Zenith Angle 
        VarAttrs[('solarZenithAngle', 'MetaData')]['units']       = 'degree' 
        VarAttrs[('solarZenithAngle', 'MetaData')]['_FillValue']  = float32_fill_value 
        VarAttrs[('solarZenithAngle', 'MetaData')]['long_name']   = 'Solar Zenith Angle' 
        VarAttrs[('solarZenithAngle', 'MetaData')]['valid_range'] = np.array([0, 180], dtype=np.float32)

        # Solar Azimuth Angle 
        VarAttrs[('solarAzimuthAngle', 'MetaData')]['units']       = 'degree' 
        VarAttrs[('solarAzimuthAngle', 'MetaData')]['_FillValue']  = float32_fill_value 
        VarAttrs[('solarAzimuthAngle', 'MetaData')]['long_name']   = 'Solar Azimuth Angle' 
        VarAttrs[('solarAzimuthAngle', 'MetaData')]['valid_range'] = np.array([0, 360], dtype=np.float32)

        # Sensor Zenith Angle 
        VarAttrs[('sensorZenithAngle', 'MetaData')]['units']       = 'degree' 
        VarAttrs[('sensorZenithAngle', 'MetaData')]['_FillValue']  = float32_fill_value 
        VarAttrs[('sensorZenithAngle', 'MetaData')]['long_name']   = 'Sensor Zenith Angle' 
        VarAttrs[('sensorZenithAngle', 'MetaData')]['valid_range'] = np.array([0, 90], dtype=np.float32) 

        # Sensor Azimuth Angle 
        VarAttrs[('sensorAzimuthAngle', 'MetaData')]['units']       = 'degree' 
        VarAttrs[('sensorAzimuthAngle', 'MetaData')]['_FillValue']  = float32_fill_value 
        VarAttrs[('sensorAzimuthAngle', 'MetaData')]['long_name']   = 'Sensor Azimuth Angle' 
        VarAttrs[('sensorAzimuthAngle', 'MetaData')]['valid_range'] = np.array([0, 360], dtype=np.float32) 

        # Sensor View Angle 
        VarAttrs[('sensorViewAngle', 'MetaData')]['units']      = 'degree' 
        VarAttrs[('sensorViewAngle', 'MetaData')]['_FillValue'] = float32_fill_value 
        VarAttrs[('sensorViewAngle', 'MetaData')]['long_name']  = 'Sensor View Angle' 

        # Height of Station 
        VarAttrs[('heightOfStation', 'MetaData')]['units']      = 'm' 
        VarAttrs[('heightOfStation', 'MetaData')]['_FillValue'] = float32_fill_value 

        # Sensor Channel Number 
        VarAttrs[('sensorChannelNumber', 'MetaData')]['long_name'] = 'Sensor Channel Number' 
        VarAttrs[('sensorChannelNumber', 'MetaData')]['_FillValue'] = int32_fill_value 
 
        # Fraction of Clear Pixels in FOV 
        VarAttrs[('fractionOfClearPixelsInFov', 'MetaData')]['long_name']   = 'Fraction of Clear Pixels in a Field of View' 
        VarAttrs[('fractionOfClearPixelsInFov', 'MetaData')]['_FillValue']  = float32_fill_value 
        VarAttrs[('fractionOfClearPixelsInFov', 'MetaData')]['valid_range'] = np.array([0, 1], dtype=np.float32) 

        # Spectral Radiance 
        VarAttrs[('radiance', 'ObsValue')]['units']      = 'W m-2 sr-1 m' 
        VarAttrs[('radiance', 'ObsValue')]['_FillValue'] = float32_fill_value 
        VarAttrs[('radiance', 'ObsValue')]['long_name']  = 'Spectral Radiance' 

        print(' ... Write data to IODA variables')
        # Write data to IODA variables
        print(' ... ... Writer ...')
        writer = iconv.IodaWriter(OUTPUT_PATH, locationKeyList, DimDict)

        print(' ... ... Writer:BuildIODA ...')
        writer.BuildIoda(VarOut, VarDims, VarAttrs, GlobalAttrs)
        print(' ... Done ! IODA file created for', satinst)

    print('All Done!')

if __name__ == '__main__':

   start_time = time.time()

   bufr_to_ioda()

   end_time = time.time()
   running_time = end_time -start_time
   print('Total running time: ', running_time, 'seconds') 
