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
import datetime 
from collections import namedtuple
import ioda_obs_space as ioda_ospace

REFERENCE_TIME = '2021-08-01T00:00:00Z'           # Analysis time
SOURCE_FILE    = 'gdas.t00z.mtiasi.tm00.bufr_d'   # BUFR Dump file
DATA_PATH      = '/work/noaa/da/eliu/JEDI-iodaconv/run_iasi/testinput/2021080100/'+SOURCE_FILE

# Define and initialize  global variables
global float32_fill_value 
global int32_fill_value 
global int64_fill_value 

float32_fill_value = np.float32(0) 
int32_fill_value   = np.int32(0) 
int64_fill_value   = np.int64(0) 

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
       sensor_scan_position = np.zeros(nfov, dtype=np.float32)
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
 
       nfov = fov.shape[0]
       tmp  = np.where((scanpos %2 == 1), stepAdjust, -stepAdjust)
       sensor_view_angle = np.zeros(nfov, dtype=np.float32)
       sensor_view_angle = start + np.float32(np.int32((fov-1)/4)) * step + tmp
    else: 
       print('Compute_SensorViewAngle: not implemented for ', sensor, ' yet') 

    return sensor_view_angle

def ConvertScaledRadiance_to_Radiance(channum, begchan, endchan, cscale, srad):

    nlocs, nchans = srad.shape
    nbands        = begchan.shape[1]

    channum  = channum.flatten()
    srad     = srad.flatten()
    begchan  = begchan.flatten()
    endchan  = endchan.flatten()
    cscale   = cscale.flatten()
    nobs     = len(srad) 
    # radiance has the same shape and data type as srad
    radiance = np.full_like(srad, float32_fill_value)
    iloc     = np.int32(np.floor(np.arange(nobs) / nchans))

    for ibnd in range(nbands):
        bandoffset    = iloc * nbands + ibnd
        channel_mask  = (channum >= begchan[bandoffset]) & (channum <= endchan[bandoffset])
        valid_indices = np.where(channel_mask)[0]
    
        valid_srad   = srad[valid_indices]
        valid_cscale = cscale[bandoffset][valid_indices]
    
        valid_scalefactor       = 10 ** -np.array(valid_cscale, dtype=np.float32)
        radiance[valid_indices] = valid_srad * valid_scalefactor

    radiance = radiance.reshape(nlocs, nchans)

    return radiance 

def bufr_to_ioda():

    # Define instrument sensor and satellite information
    instrument = namedtuple('instrument', ['name', 'sensor_id', \
                                           'satellite_name', 'satellite_id', \
                                           'platformCommonName', 'platformLongDescription' \
                                          ])

    instruments = [
        instrument('iasi', 221, 'metop-a', 4, 'Meteorological Operational Satellite - A', 'EUMETSAT Polar System in sunsynchronous orbit'),   
        instrument('iasi', 221, 'metop-b', 3, 'Meteorological Operational Satellite - B', 'EUMETSAT Polar System in sunsynchronous orbit'),   
        instrument('iasi', 221, 'metop-c', 5, 'Meteorological Operational Satellite - C', 'EUMETSAT Polar System in sunsynchronous orbit')    
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
    q.add('fractionOfClearPixelsInFov', '*/IASIL1CS{1}/FCPH') 
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
    satheight = r.get('heightOfStation', type='float')

    print(' ... Executing QuerySet: get cluster data ...')
    # Variable with Dimension: [Location, Cluster]
    fcph      = r.get('fractionOfClearPixelsInFov', type='float')

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
    # DateTime: seconds since Epoch time
    # IODA has no support for numpy datetime arrays dtype=datetime64[s]
    timestamp    = r.get_datetime('year','month','day','hour','minute','second').astype(np.int64)
    datetime_min = datetime.datetime.fromtimestamp(timestamp.min())
    datetime_max = datetime.datetime.fromtimestamp(timestamp.max())

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

    print('     channum   type  = ', channum.dtype)  
    print('     begchan   type  = ', scaledrad.dtype)  
    print('     scalecoef type  = ', scalecoef.dtype)  
    print('     scaledrad type  = ', scaledrad.dtype)  
    print('     satid     type  = ', satid.dtype)  
    print('     instid    type  = ', instid.dtype)  
    print('     lat       type  = ', lat.dtype)  
    print('     lon       type  = ', lon.dtype)  
    print('     qcflag    type  = ', qcflag.dtype)  
    print('     scanline  type  = ', scanline.dtype)  
    print('     fovn      type  = ', fovn.dtype)  
    print('     fcph      type  = ', fcph.dtype)  
    print('     satheight type  = ', satheight.dtype)  
    print('     solzenang type  = ', solzenang.dtype)  
    print('     satzenang type  = ', satzenang.dtype)  
    print('     timestamp type  = ', timestamp.dtype)  

    # Global variables declaration
    # Set global fill values
    float32_fill_value = satzenang.fill_value 
    int32_fill_value   = qcflag.fill_value 
    int64_fill_value   = timestamp.fill_value.astype(np.int64) 
    print('     float32_fill_value  = ', float32_fill_value)  
    print('     int32_fill_value    = ', int32_fill_value)  
    print('     int64_fill_value    = ', int64_fill_value)  

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
    scanpos = Compute_SensorScanPosition(fovn, sensor_name).astype(np.float32)
    print('     fovn (orig) min/max = ', fovn.min(), fovn.max())
    print('     scanpos     min/max = ', scanpos.min(), scanpos.max())

    print(' ... Creating derived variable - sensor view position ...')
    # Sensor View Angle 
    viewang = Compute_SensorViewAngle(fovn, scanpos, sensor_name).astype(np.float32)
    print('     sensorViewAngle min/max = ', viewang.min(), viewang.max())

    print(' ... Creating derived variable - spectral radiance ...')
    # Radiance
    radiance = ConvertScaledRadiance_to_Radiance(channum, begchan, endchan, scalecoef, scaledrad).astype(np.float32)
    print('     radiance min/max = ', radiance.min(), radiance.max())

    print('     Check drived variables type ... ')
    print('     radiance type = ', radiance.dtype)
    print('     scanpos  type = ', scanpos.dtype)
    print('     viewang  type = ', viewang.dtype)
    print('     fcph     type = ', fcph.dtype)

    end_time = time.time()
    running_time = end_time - start_time
    print('Running time for creating derived variables : ', running_time, 'seconds')

    # =====================================
    # Split output based on satellite id
    # Create IODA ObsSpace
    # Write IODA output
    # =====================================
    print('Split data based on satellite id, Create IODA ObsSpace and Write IODA output')

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
        satellite_id         = list(map(lambda instrument: instrument.satellite_id, matched))
        satellite            = list(map(lambda instrument: instrument.satellite_name, matched))
        sensor_id            = list(map(lambda instrument: instrument.sensor_id, matched))
        sensor               = list(map(lambda instrument: instrument.name, matched))
        platform_name        = list(map(lambda instrument: instrument.name, matched))
        platform_description = list(map(lambda instrument: instrument.name, matched))
        print(' ... Matched satellite_id = ', satellite_id)
        print(' ... Matched satellite    = ', satellite)
        print(' ... Matched sensor_id    = ', sensor_id)
        print(' ... Matched sensor       = ', sensor)
        satinst      = sensor[0]+'_'+satellite[0] 

        print(' ... Split data for', satinst, 'satid = ', sat)

        # Define a boolean mask to subset data from the original data object
        mask       = satid == sat
        lon2       = lon[mask]
        lat2       = lat[mask]
        timestamp2 = timestamp[mask]
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
        viewang2   = viewang[mask]
        satheight2 = satheight[mask]
        channum2   = channum[mask]
        fcph2      = fcph[mask]
        radiance2  = radiance[mask]
        scaledrad2 = scaledrad[mask]

        print(' ... Check drived variables type for subset ... ')
        print('     radiance2 type = ', radiance.dtype)
        print('     scanpos2  type = ', scanpos.dtype)
        print('     viewang2  type = ', viewang.dtype)
        print('     fcph2     type = ', fcph.dtype)

        print(' ... Create ObsSpae for', satinst, 'satid = ', sat)
        # Create the dimensions
#       dims = {
#          'Location' : radiance2.shape[0],
#          'Channel'  : radiance2.shape[1],
#          'Cluster'  : fcph2.shape[1]
#       }
        dims = {
           'Location' : np.arange(0, radiance2.shape[0]),
           'Channel'  : np.array(channum2[0, :])
#          'Cluster'  : np.arange(0, fcph2.shape[1])
        }
    
        # Create IODA ObsSpace 
        OUTPUT_PATH = '/work/noaa/da/eliu/JEDI-iodaconv/run_iasi/testrun_py3/2021080100/gdas.t00z.'+satinst+'.nc'
        print(' ... ... Create output file ...', OUTPUT_PATH)
        obsspace = ioda_ospace.ObsSpace(OUTPUT_PATH, mode='w', dim_dict=dims)

        # Create Global attributes
        print(' ... ... Create global attributes')
        obsspace.write_attr('sourceFiles', SOURCE_FILE)
        obsspace.write_attr('source', 'MTYP 021-241 IASI 1C RADIANCES (VARIABLE CHNS) (METOP)')
        obsspace.write_attr('processingLevel', 'Level-1C')
        obsspace.write_attr('converter', 'BUFR')
        obsspace.write_attr('datetimeRange', [ str(datetime_min), str(datetime_max)] )
        obsspace.write_attr('datetimeReference', REFERENCE_TIME)
        obsspace.write_attr('platform', satellite_id)
        obsspace.write_attr('sensor', sensor_id)
        obsspace.write_attr('platformCommonName', platform_name)
        obsspace.write_attr('platformLongDescription', platform_description)

        # Create IODA variables
        print(' ... ... Create variables: name, type, units, and attributes')
        # Longitude
        obsspace.create_var('MetaData/longitude', dtype=lon2.dtype, fillval=lon2.fill_value) \
            .write_attr('units', 'degrees_east') \
            .write_attr('valid_range', np.array([-180, 180], dtype=np.float32)) \
            .write_attr('long_name', 'Longitude') \
            .write_data(lon2)

        # Latitude 
        obsspace.create_var('MetaData/latitude', dtype=lat.dtype, fillval=lat2.fill_value) \
            .write_attr('units', 'degrees_north') \
            .write_attr('valid_range', np.array([-90, 90], dtype=np.float32)) \
            .write_attr('long_name', 'Latitude') \
            .write_data(lat2)

        # Datetime
        obsspace.create_var('MetaData/dateTime', dtype=np.int64, fillval=int64_fill_value) \
            .write_attr('units', 'seconds since 1970-01-01T00:00:00Z') \
            .write_attr('long_name', 'Datetime') \
            .write_data(timestamp2)

        # Satellite Identifier  
        obsspace.create_var('MetaData/satelliteIdentifier', dtype=satid2.dtype, fillval=satid2.fill_value) \
            .write_attr('long_name', 'Satellite Identifier') \
            .write_data(satid2)

        # Instrument Identifier  
        obsspace.create_var('MetaData/instrumentIdentifier', dtype=instid2.dtype, fillval=instid2.fill_value) \
            .write_attr('long_name', 'Satellite Instrument Identifier') \
            .write_data(instid2)
 
        # Scan Line Number  
        obsspace.create_var('MetaData/scanLineNumber', dtype=scanline2.dtype, fillval=scanline2.fill_value) \
            .write_attr('long_name', 'Scan Line Number') \
            .write_data(scanline2)
 
        # Quality Flags  
        obsspace.create_var('MetaData/qualityFlags', dtype=qcflag2.dtype, fillval=qcflag2.fill_value) \
            .write_attr('long_name', 'Individual IASI-System Quality Flag') \
            .write_data(qcflag2)
 
        # Field-of-View Number  
        obsspace.create_var('MetaData/fieldOfViewNumber', dtype=fovn2.dtype, fillval=fovn2.fill_value) \
            .write_attr('long_name', 'Field of View Number') \
            .write_data(fovn2)
 
        # Scan Position (derived variable, need to specified fill value explicitly) 
        obsspace.create_var('MetaData/sensorScanPosition', dtype=scanpos2.dtype, fillval=float32_fill_value) \
            .write_attr('long_name', 'Sensor Scan Position') \
            .write_data(scanpos2)
 
        # Solar Zenith Angle 
        obsspace.create_var('MetaData/solarZenithAngle', dtype=solzenang2.dtype, fillval=solzenang2.fill_value) \
            .write_attr('units', 'degree') \
            .write_attr('valid_range', np.array([0, 180], dtype=np.float32)) \
            .write_attr('long_name', 'Solar Zenith Angle') \
            .write_data(solzenang2)

        # Solar Azimuth Angle 
        obsspace.create_var('MetaData/solarAzimuthAngle', dtype=solaziang2.dtype, fillval=solaziang2.fill_value) \
            .write_attr('units', 'degree') \
            .write_attr('valid_range', np.array([0, 360], dtype=np.float32)) \
            .write_attr('long_name', 'Solar Azimuth Angle') \
            .write_data(solaziang2)

        # Sensor Zenith Angle 
        obsspace.create_var('MetaData/sensorZenithAngle', dtype=satzenang2.dtype, fillval=satzenang2.fill_value) \
            .write_attr('units', 'degree') \
            .write_attr('valid_range', np.array([0, 90], dtype=np.float32)) \
            .write_attr('long_name', 'Sensor Zenith Angle') \
            .write_data(satzenang2)

        # Sensor Azimuth Angle 
        obsspace.create_var('MetaData/sensorAzimuthAngle', dtype=sataziang2.dtype, fillval=sataziang2.fill_value) \
            .write_attr('units', 'degree') \
            .write_attr('valid_range', np.array([0, 360], dtype=np.float32)) \
            .write_attr('long_name', 'Sensor Azimuth Angle') \
            .write_data(sataziang2)

        # Sensor View Angle (derived variable, need to specified fill value explicitly) 
        obsspace.create_var('MetaData/sensorViewAngle', dtype=viewang2.dtype, fillval=float32_fill_value) \
            .write_attr('units', 'degree') \
            .write_attr('long_name', 'Sensor View Angle') \
            .write_data(viewang2)

        # Height of Station 
        obsspace.create_var('MetaData/stationElevation', dtype=satheight2.dtype, fillval=satheight2.fill_value) \
            .write_attr('units', 'm') \
            .write_attr('long_name', 'Altitude of Satellite') \
            .write_data(satheight2)

        # Sensor Channel Number 
        obsspace.create_var('MetaData/sensorChannelNumber', dim_list=['Location','Channel'], dtype=channum2.dtype, fillval=channum2.fill_value) \
            .write_attr('long_name', 'Sensor Channel Number') \
            .write_data(channum2)

        # Fraction of Clear Pixels in FOV 
        obsspace.create_var('MetaData/fractionOfClearPixelsInFOV', dim_list=['Location'], dtype=fcph2.dtype, fillval=fcph2.fill_value) \
            .write_attr('units', '1') \
            .write_attr('valid_range', np.array([0, 1], dtype=np.float32)) \
            .write_attr('long_name', 'Fraction of Clear Pixels in a Field of View') \
            .write_data(fcph2)

        # Spectral Radiance (derived variable, need to specified fill value explicitly)
        obsspace.create_var('ObsValue/spectralRadiance', dim_list=['Location','Channel'], dtype=radiance2.dtype, fillval=float32_fill_value) \
            .write_attr('units', 'W m-2 sr-1 m') \
            .write_attr('long_name', 'Spectral Radiance') \
            .write_data(radiance2)

#        # Spectral Radiance (derived variable, need to specified fill value explicitly)
#        obsspace.create_var('ObsValue/scaledRadiance', dim_list=['Location','Channel'], dtype=scaledrad2.dtype, fillval=scaledrad2.fill_value) \
#            .write_attr('units', 'W m-2 sr-1 m') \
#            .write_attr('long_name', 'Scaled Spectral  Radiance') \
#            .write_data(scaledrad2)

        end_time = time.time()
        running_time = end_time - start_time
        print('Running time for splitting and output IODA for', satinst, running_time, 'seconds')

    print("All Done!")

if __name__ == '__main__':

    start_time = time.time()

    bufr_to_ioda()

    end_time = time.time()
    running_time = end_time -start_time
    print('Total running time: ', running_time, 'seconds') 



