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
import ioda
import calendar
import time
import math 
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
    # DateTime: seconds since Epoch time
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
    print('     lat       type  = ', lat.dtype)  
    print('     fcph      type  = ', fcph.dtype)  
    print('     satheight type  = ', satheight.dtype)  
    print('     fovn      type  = ', fovn.dtype)  
    print('     timestamp type  = ', timestamp.dtype)  

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

    # =========================================================
    # Split output based on satellite id and Write IODA output
    # =========================================================
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

        # Write the data to IODA file
        # Create the variable creation parameters.
        OUTPUT_PATH = '/work/noaa/da/eliu/JEDI-iodaconv/run_iasi/testrun_py/2021080100/gdas.t00z.'+satinst+'.nc'
        print(' ... ... Create output file ...', OUTPUT_PATH)
        g = ioda.Engines.HH.createFile(name=OUTPUT_PATH,
                                       mode=ioda.Engines.BackendCreateModes.Truncate_If_Exists)

        # Create parameters: set fill values (consistent with the input data) and compress
        print(' ... ... Set creation parameters, set fill value and compress')
        pfloat = ioda.VariableCreationParameters()
        pint   = ioda.VariableCreationParameters()
        pint64 = ioda.VariableCreationParameters()

        pfloat.setFillValue.float(lat.fill_value)
        pint.setFillValue.int(fovn.fill_value)
        pint64.setFillValue.int(timestamp.fill_value.astype(np.int64))

        print(' ... ... Checking pfloat fill_value = ', lat.fill_value) 
        print(' ... ... Checking pint   fill_value = ', fovn.fill_value) 
        print(' ... ... Checking pint64 fill_value = ', timestamp.fill_value.astype(np.int64)) 

        pfloat.compressWithGZIP()
        pint.compressWithGZIP()
        pint64.compressWithGZIP()
 
        # Create the dimensions and dimension-related data for subset
        print(' ... ... Create dimensions and dimension-related data for subset')
        num_location = lat2.shape[0]
        num_channel  = channum2.shape[1]
        num_cluster  = fcph2.shape[1]

        print('         num_location = ', num_location)
        print('         num_channel  = ', num_channel)
        print('         num_cluster  = ', num_cluster)

        dim_location = g.vars.create('Location', ioda.Types.int32, [num_location])
        dim_location.scales.setIsScale('Location')
        dim_location.writeVector.int(np.arange(num_location))
 
        dim_channel  = g.vars.create('Channel', ioda.Types.int32, [num_channel])
        dim_channel.scales.setIsScale('Channel')
        dim_channel.writeVector.int(np.array(channum2[0,:]))

        dim_cluster  = g.vars.create('Cluster', ioda.Types.int32, [num_cluster])
        dim_cluster.scales.setIsScale('Cluster')
        dim_cluster.writeVector.int(np.arange(num_cluster))

        # Create Global attributes
        print(' ... ... Create global attributes')
        global_out = g.atts.create('platformCommonName', ioda.Types.str, [1])
        global_out.writeVector.str(['IASI'])

        global_out = g.atts.create('platformLongDescription', ioda.Types.str, [1])
        global_out.writeVector.str(['MTYP 021-241 IASI 1C RADIANCES (VARIABLE CHNS) (METOP)'])

        # Create IODA variables
        print(' ... ... Create variables: name, type, dimension and attributes')
        # Longitude
        lon_out = g.vars.create('MetaData/longitude', ioda.Types.float, scales=[dim_location], params=pfloat)
        lon_out.atts.create('units', ioda.Types.str).writeVector.str(['degrees_east'])
        lon_out.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([-180, 180])
        lon_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Longitude'])
    
        # Latitude 
        lat_out = g.vars.create('MetaData/latitude', ioda.Types.float,  scales=[dim_location], params=pfloat)
        lat_out.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([-90, 90])
        lat_out.atts.create('units', ioda.Types.str).writeVector.str(['degrees_north'])
        lat_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Latitude'])

        # Datetime
        timestamp_out = g.vars.create('MetaData/dateTime',  ioda.Types.int64,  scales=[dim_location], params=pint64)
        timestamp_out.atts.create('units', ioda.Types.str).writeVector.str(['seconds since 1970-01-01T00:00:00Z'])
        timestamp_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Datetime'])

        # Satellite Identifier  
        satid_out = g.vars.create('MetaData/satelliteIdentifier', ioda.Types.int32,  scales=[dim_location], params=pint)
        satid_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Satellite Identifier'])
  
        # Instrument Identifier  
        instid_out = g.vars.create('MetaData/instrumentIdentifier', ioda.Types.int32,  scales=[dim_location], params=pint)
        instid_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Satellite Instrument Identifier'])
 
        # Scan Line Number  
        scanline_out = g.vars.create('MetaData/scanLineNumber', ioda.Types.int32,  scales=[dim_location], params=pint)
        scanline_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Scan Line Number'])
 
        # Quality Flags  
        qcflag_out = g.vars.create('MetaData/qualityFlags', ioda.Types.int32,  scales=[dim_location], params=pint)
        qcflag_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Individual IASI-System Quality Flag'])
 
        # Field-of-View Number  
        fovn_out = g.vars.create('MetaData/fieldOfViewNumber', ioda.Types.int32,  scales=[dim_location], params=pint)
        fovn_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Field of View Number'])
 
        # Scan Position  
        scanpos_out = g.vars.create('MetaData/sensorScanPosition', ioda.Types.float,  scales=[dim_location], params=pfloat)
        scanpos_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Sensor Scan Position'])
 
        # Solar Zenith Angle 
        solzenang_out = g.vars.create('MetaData/solarZenithAngle', ioda.Types.float,  scales=[dim_location], params=pfloat)
        solzenang_out.atts.create('units', ioda.Types.str).writeVector.str(['degree'])
        solzenang_out.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([0, 180])
        solzenang_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Solar Zenith Angle'])

        # Solar Azimuth Angle 
        solaziang_out = g.vars.create('MetaData/solarAzimuthAngle', ioda.Types.float,  scales=[dim_location], params=pfloat)
        solaziang_out.atts.create('units', ioda.Types.str).writeVector.str(['degree'])
        solaziang_out.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([0, 360])
        solaziang_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Solar Azimuth Angle'])

        # Sensor Zenith Angle 
        satzenang_out = g.vars.create('MetaData/sensorZenithAngle', ioda.Types.float,  scales=[dim_location], params=pfloat)
        satzenang_out.atts.create('units', ioda.Types.str).writeVector.str(['degree'])
        satzenang_out.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([0, 90])
        satzenang_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Sensor Zenith Angle'])

        # Sensor Azimuth Angle 
        sataziang_out = g.vars.create('MetaData/sensorAzimuthAngle', ioda.Types.float,  scales=[dim_location], params=pfloat)
        sataziang_out.atts.create('units', ioda.Types.str).writeVector.str(['degree'])
        sataziang_out.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([0, 360])
        sataziang_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Sensor Azimuth Angle'])

        # Sensor View Angle 
        viewang_out = g.vars.create('MetaData/sensorViewAngle', ioda.Types.float,  scales=[dim_location], params=pfloat)
        viewang_out.atts.create('units', ioda.Types.str).writeVector.str(['degree'])
        viewang_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Sensor View Angle'])

        # Height of Station 
        satheight_out= g.vars.create('MetaData/heightOfStation', ioda.Types.float,  scales=[dim_location], params=pfloat)
        satheight_out.atts.create('units', ioda.Types.str).writeVector.str(['m'])
        satheight_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Altitude of Satellite'])

        # Sensor Channel Number 
        channum_out = g.vars.create('MetaData/sensorChannelNumber', ioda.Types.int32,  scales=[dim_location, dim_channel], params=pint)
        channum_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Sensor Channel Number'])
 
        # Fraction of Clear Pixels in FOV 
        fcph_out = g.vars.create('MetaData/fractionOfClearPixelsInFov', ioda.Types.float,  scales=[dim_location, dim_cluster], params=pfloat)
        fcph_out.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([0, 1])
        fcph_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Fraction of Clear Pixels in a Field of View'])

        # Spectral Radiance 
        radiance_out = g.vars.create('ObsValue/radiance', ioda.Types.float,  scales=[dim_location, dim_channel], params=pfloat)
        radiance_out.atts.create('units', ioda.Types.str).writeVector.str(['W m-2 sr-1 m'])
        radiance_out.atts.create('long_name', ioda.Types.str).writeVector.str(['Spectral Radiance'])

        # Write data to IODA variables and global attributes
        print(' ... Write data to IODA')
        print(' ... ... Write metadata data to variables')
        lon_out.writeNPArray.float(lon2.flatten())
        lat_out.writeNPArray.float(lat2.flatten())
        timestamp_out.writeNPArray.int64(timestamp2.flatten())
        satid_out.writeNPArray.int32(satid2.flatten())
        instid_out.writeNPArray.int32(instid2.flatten())
        scanline_out.writeNPArray.int32(scanline2.flatten())
        qcflag_out.writeNPArray.int32(qcflag2.flatten())
        fovn_out.writeNPArray.int(fovn2.flatten())
        scanpos_out.writeNPArray.float(scanpos2.flatten())
        solzenang_out.writeNPArray.float(solzenang2.flatten())
        solaziang_out.writeNPArray.float(solaziang2.flatten())
        satzenang_out.writeNPArray.float(satzenang2.flatten())
        sataziang_out.writeNPArray.float(sataziang2.flatten())
        viewang_out.writeNPArray.float(viewang2.flatten())
        satheight_out.writeNPArray.float(satheight2.flatten())
        print(' ... ... Write channel number to IODA')
        channum_out.writeNPArray.int32(channum2.flatten())
        fcph_out.writeNPArray.float(fcph2.flatten())
        print(' ... ... Write observation (radiance) to IODA')
        radiance_out.writeNPArray.float(radiance2.flatten())

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



