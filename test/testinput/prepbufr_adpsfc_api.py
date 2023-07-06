# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import sys
#sys.path.append('/work2/noaa/da/nesposito/ioda-bundle_apifix/build/lib/python3.9/')
#sys.path.append('/work2/noaa/da/nesposito/ioda-bundle_apifix/build/lib/python3.9/pyiodaconv/')
#sys.path.append('/work2/noaa/da/nesposito/ioda-bundle_apifix/build/lib/python3.9/pyioda/')

import numpy as np
import numpy.ma as ma
import pyiodaconv as iconv
import pyiodaconv as iconio
from pyiodaconv import bufr
from pyioda import ioda
import calendar
import time

DATA_PATH = './testinput/gdas.t12z.adpsfc.prepbufr'
OUTPUT_PATH = './testrun/prepbufr_adpsfc_api.nc'

def test_bufr_to_ioda():
   # Make the QuerySet for all the data we want
   q = bufr.QuerySet()
#MetaData
   q.add('latitude', '*/YOB')
   q.add('longitude', '*/XOB')
   q.add('obsTimeMinusCycleTime', '*/DHR')
   q.add('stationElevation', '*/ELV')
#   q.add('stationElevation', '*/Z___INFO/Z__EVENT/ZOB')
   q.add('temperatureEventCode', '*/T___INFO/T__EVENT{1}/TPC') 
#ObsValue
   q.add('airTemperature', '*/T___INFO/T__EVENT{1}/TOB')
   q.add('virtualTemperature', '*/T___INFO/TVO')
   q.add('stationPressure', '*/P___INFO/P__EVENT{1}/POB')
   q.add('windNorthward', '*/W___INFO/W__EVENT{1}/VOB')
   q.add('windEastward', '*/W___INFO/W__EVENT{1}/UOB')
   q.add('specificHumidity', '*/Q___INFO/Q__EVENT{1}/QOB')
#QualityMark
   q.add('stationPressureQM', '*/P___INFO/P__EVENT{1}/PQM')
   q.add('airTemperatureQM', '*/T___INFO/T__EVENT{1}/TQM') 
   q.add('virtualTemperatureQM', '*/T___INFO/T__EVENT{1}/TQM') 
   q.add('specificHumidityQM', '*/Q___INFO/Q__EVENT{1}/QQM')
   q.add('windNorthwardQM', '*/W___INFO/W__EVENT{1}/WQM')
   q.add('windEastwardQM', '*/W___INFO/W__EVENT{1}/WQM')
   
   # Open the BUFR file and execute the QuerySet
   with bufr.File(DATA_PATH) as f:
      print("execute")
      r = f.execute(q)

   # Use the ResultSet returned to get numpy arrays of the data
#MetaData group
   lat = r.get('latitude')
   lon = r.get('longitude')
   lon[lon>180] -= 360  #Convert Longitude from [0,360] to [-180,180]
   print("Get time")
   dhr = r.get('obsTimeMinusCycleTime') #Needs to be converted to seconds since Epoch time from [-3,3]
   print("cycleTimeSinceEpoch") #For now, file time is put in manually 
   cycleTimeSinceEpoch = np.int64(calendar.timegm(time.strptime('2021 08 01 00 00', '%Y %m %d %H %M')))
   print("cycleTimeSinceEpoch: ", cycleTimeSinceEpoch)
   dhr = np.int64(dhr*3600)
   dhr += cycleTimeSinceEpoch
   elv = r.get('stationElevation')
   tpc = r.get('temperatureEventCode', type='int')

#ObsValue group
   print("Temperatures")
   tob = r.get('airTemperature')
   tob += 273.15
   tvo = r.get('virtualTemperature')
   tvo += 273.15
   print("stationPressure")
   pob = r.get('stationPressure')
   pob *= 100
   uob = r.get('windEastward') 
   vob = r.get('windNorthward') 
   print("humidity")
   qob = r.get('specificHumidity', type='float')
   qob *= 0.000001

#Quality Marker group
   tobqm = r.get('airTemperatureQM')
   tvoqm = r.get('virtualTemperatureQM')
   pobqm = r.get('stationPressureQM')
   qobqm = r.get('specificHumidityQM')
   uobqm = r.get('windEastwardQM')
   vobqm = r.get('windNorthwardQM')

   # Write the data to an IODA file
   g = ioda.Engines.HH.createFile(name=OUTPUT_PATH,
                                  mode=ioda.Engines.BackendCreateModes.Truncate_If_Exists)

   # Create the dimensions
   print("Create dimensions")
   num_locs = lat.shape[0]

   dim_location = g.vars.create('Location', ioda.Types.int32, [num_locs])
   dim_location.scales.setIsScale('Location')

   print("creation parameters, set fill value and compress")
   pfloat = ioda.VariableCreationParameters()
   pint = ioda.VariableCreationParameters()
   pint64 = ioda.VariableCreationParameters()

   pfloat.setFillValue.float(lat.fill_value)
   pint.setFillValue.int(tobqm.fill_value)
   pint64.setFillValue.int64(dhr.fill_value)

   pfloat.compressWithGZIP()
   pint.compressWithGZIP()
   pint64.compressWithGZIP()

   # Create the variables
   print("Create MetaData")
   longitude = g.vars.create('MetaData/longitude', ioda.Types.float, scales=[dim_location], params=pfloat)
   longitude.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([-179.99999, 180.0])
   longitude.atts.create('units', ioda.Types.str).writeVector.str(['degree_east'])
   longitude.atts.create('long_name', ioda.Types.str).writeVector.str(['Longitude'])

   latitude = g.vars.create('MetaData/latitude', ioda.Types.float,  scales=[dim_location], params=pfloat)
   latitude.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([-90.0, 90.0])
   latitude.atts.create('units', ioda.Types.str).writeVector.str(['degree_north'])
   latitude.atts.create('long_name', ioda.Types.str).writeVector.str(['Latitude'])

   datetime = g.vars.create('MetaData/dateTime',  ioda.Types.int64,  scales=[dim_location], params=pint64)
   datetime.atts.create('units', ioda.Types.str).writeVector.str(['seconds since 1970-01-01T00:00:00Z'])

   stationelevation = g.vars.create('MetaData/stationElevation', ioda.Types.float, scales=[dim_location], params=pfloat)
   stationelevation.atts.create('units', ioda.Types.str).writeVector.str(['m'])
   stationelevation.atts.create('long_name', ioda.Types.str).writeVector.str(['Station Elevation'])

   temperatureeventcode = g.vars.create('MetaData/temperatureEventCode', ioda.Types.int, scales=[dim_location], params=pint)
   temperatureeventcode.atts.create('units', ioda.Types.str).writeVector.str(['1'])
   temperatureeventcode.atts.create('long_name', ioda.Types.str).writeVector.str(['Temperature Event Code'])

   print("create ObsValue group")
   airtemperature = g.vars.create('ObsValue/airTemperature', ioda.Types.float, scales=[dim_location],params=pfloat)
   airtemperature.atts.create('units', ioda.Types.str).writeVector.str(['K'])
   airtemperature.atts.create('long_name', ioda.Types.str).writeVector.str(['Air Temperature'])

   virtualtemperature = g.vars.create('ObsValue/virtualTemperature', ioda.Types.float, scales=[dim_location],params=pfloat)
   virtualtemperature.atts.create('units', ioda.Types.str).writeVector.str(['K'])
   virtualtemperature.atts.create('long_name', ioda.Types.str).writeVector.str(['Virtual Temperature'])

   stationpressure = g.vars.create('ObsValue/stationPressure', ioda.Types.float, scales=[dim_location], params=pfloat)
   stationpressure.atts.create('units', ioda.Types.str).writeVector.str(['Pa'])
   stationpressure.atts.create('long_name', ioda.Types.str).writeVector.str(['Station Pressure'])

   pressure = g.vars.create('MetaData/pressure', ioda.Types.float, scales=[dim_location], params=pfloat)
   pressure.atts.create('units', ioda.Types.str).writeVector.str(['Pa'])
   pressure.atts.create('long_name', ioda.Types.str).writeVector.str(['Pressure'])

   windeastward = g.vars.create('ObsValue/windEastward', ioda.Types.float, scales=[dim_location], params=pfloat)
   windeastward.atts.create('units', ioda.Types.str).writeVector.str(['m s-1'])
   windeastward.atts.create('long_name', ioda.Types.str).writeVector.str(['Eastward Wind'])

   windnorthward = g.vars.create('ObsValue/windNorthward', ioda.Types.float, scales=[dim_location], params=pfloat)
   windnorthward.atts.create('units', ioda.Types.str).writeVector.str(['m s-1'])
   windnorthward.atts.create('long_name', ioda.Types.str).writeVector.str(['Northward Wind'])

   specifichumidity = g.vars.create('ObsValue/specificHumidity', ioda.Types.float, scales=[dim_location], params=pfloat)
   specifichumidity.atts.create('units', ioda.Types.str).writeVector.str(['kg kg-1'])
   specifichumidity.atts.create('long_name', ioda.Types.str).writeVector.str(['Specific Humidity'])   

   print("Create Quality Marker group")
   airtemperatureqm = g.vars.create('QualityMarker/airTemperature', ioda.Types.int, scales=[dim_location], params=pint)
   airtemperatureqm.atts.create('units', ioda.Types.str).writeVector.str(['1'])
   airtemperatureqm.atts.create('long_name', ioda.Types.str).writeVector.str(['Air Temperature Quality Marker'])

   virtualtemperatureqm = g.vars.create('QualityMarker/virtualTemperature', ioda.Types.int, scales=[dim_location], params=pint)
   virtualtemperatureqm.atts.create('units', ioda.Types.str).writeVector.str(['1'])
   virtualtemperatureqm.atts.create('long_name', ioda.Types.str).writeVector.str(['Virtual Temperature Quality Marker'])

   stationpressureqm = g.vars.create('QualityMarker/stationPressure', ioda.Types.int, scales=[dim_location], params=pint)
   stationpressureqm.atts.create('units', ioda.Types.str).writeVector.str(['1'])
   stationpressureqm.atts.create('long_name', ioda.Types.str).writeVector.str(['Station Pressure Quality Marker'])

   specifichumidityqm = g.vars.create('QualityMarker/specificHumidity', ioda.Types.int, scales=[dim_location], params=pint)
   specifichumidityqm.atts.create('units', ioda.Types.str).writeVector.str(['1'])
   specifichumidityqm.atts.create('long_name', ioda.Types.str).writeVector.str(['Specific Humidity Quality Marker'])

   windnorthwardqm = g.vars.create('QualityMarker/windNorthward', ioda.Types.int, scales=[dim_location], params=pint)
   windnorthwardqm.atts.create('units', ioda.Types.str).writeVector.str(['1'])
   windnorthwardqm.atts.create('long_name', ioda.Types.str).writeVector.str(['Northward Quality Marker'])

   windeastwardqm = g.vars.create('QualityMarker/windEastward', ioda.Types.int, scales=[dim_location], params=pint)
   windeastwardqm.atts.create('units', ioda.Types.str).writeVector.str(['1'])
   windeastwardqm.atts.create('long_name', ioda.Types.str).writeVector.str(['Eastward Wind Quality Marker'])

   # Write the data to the variables
   print("Write data to variables")
   longitude.writeNPArray.float(lon.flatten())
   latitude.writeNPArray.float(lat.flatten())
   datetime.writeNPArray.int64(dhr.flatten())
   stationelevation.writeNPArray.float(elv.flatten())
   temperatureeventcode.writeNPArray.int(tpc.flatten())

   airtemperature.writeNPArray.float(tob.flatten())
   virtualtemperature.writeNPArray.float(tvo.flatten())
   stationpressure.writeNPArray.float(pob.flatten())
   pressure.writeNPArray.float(pob.flatten())
   windnorthward.writeNPArray.float(vob.flatten())
   windeastward.writeNPArray.float(uob.flatten())
   specifichumidity.writeNPArray.float(qob.flatten())

   airtemperatureqm.writeNPArray.int(tobqm.flatten())
   virtualtemperatureqm.writeNPArray.int(tvoqm.flatten())
   stationpressureqm.writeNPArray.int(pobqm.flatten())
   specifichumidityqm.writeNPArray.int(qobqm.flatten())
   windeastwardqm.writeNPArray.int(uobqm.flatten())
   windnorthwardqm.writeNPArray.int(vobqm.flatten())

   print("end")

if __name__ == '__main__':
   test_bufr_to_ioda()

