# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import numpy as np
import numpy.ma as ma
from pyiodaconv.def_jedi_utils import long_missing_value
from pyiodaconv import bufr
from pyioda import ioda
import calendar
import time

DATA_PATH = './testinput/gdas.t00z.sfcshp.prepbufr'
OUTPUT_PATH = './testrun/prepbufr_sfcshp_api.nc'

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
   q.add('seaSurfaceTemperature', '*/SST_INFO/SSTEVENT{1}/SST1')
#QualityMark
   q.add('stationPressureQM', '*/P___INFO/P__EVENT{1}/PQM')
   q.add('airTemperatureQM', '*/T___INFO/T__EVENT{1}/TQM') 
   q.add('virtualTemperatureQM', '*/T___INFO/T__EVENT{1}/TQM') 
   q.add('specificHumidityQM', '*/Q___INFO/Q__EVENT{1}/QQM')
   q.add('windNorthwardQM', '*/W___INFO/W__EVENT{1}/WQM')
   q.add('windEastwardQM', '*/W___INFO/W__EVENT{1}/WQM')
   q.add('seaSurfaceTemperatureQM', '*/SST_INFO/SSTEVENT{1}/SSTQM')

   # Open the BUFR file and execute the QuerySet
   with bufr.File(DATA_PATH) as f:
      print("execute")
      r = f.execute(q)

   # Use the ResultSet returned to get numpy arrays of the data
#MetaData group
   lat = r.get('latitude')
   lon = r.get('longitude')
   lon[lon>180] -= 360  #Convert Longitude from [0,360] to [-180,180]

   # The time is entering in as a float32 value representing an offset from the cycle
   # time in hours (so fractions of hours can exist). This needs to be first converted
   # to seconds while still a float32, then converted to an int64 for the dateTime variable.
   # Another consideration is that the get function returns a masked array with an
   # appropriate fill value assigned. When converting to an int64, the fill value needs
   # to get updated, and then before writing into the output ioda file, the masked array
   # function filled() needs to be called which will convert the values marked invalid
   # to the fill value.
   print("Get time")
   dhr = (r.get('obsTimeMinusCycleTime') * 3600).astype(np.int64)
   np.ma.set_fill_value(dhr, long_missing_value)
   print("cycleTimeSinceEpoch") #For now, file time is put in manually 
   cycleTimeSinceEpoch = np.int64(calendar.timegm(time.strptime('2021 08 01 00 00', '%Y %m %d %H %M')))
   print("cycleTimeSinceEpoch: ", cycleTimeSinceEpoch)
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
   sst1 = r.get('seaSurfaceTemperature')

#Quality Marker group
   tobqm = r.get('airTemperatureQM')
   tvoqm = r.get('virtualTemperatureQM')
   pobqm = r.get('stationPressureQM')
   qobqm = r.get('specificHumidityQM')
   uobqm = r.get('windEastwardQM')
   vobqm = r.get('windNorthwardQM')
   sstqm = r.get('seaSurfaceTemperatureQM')

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

   seasurfacetemperature = g.vars.create('ObsValue/seaSurfaceTemperature', ioda.Types.float, scales=[dim_location],params=pfloat)
   seasurfacetemperature.atts.create('units', ioda.Types.str).writeVector.str(['K'])
   seasurfacetemperature.atts.create('long_name', ioda.Types.str).writeVector.str(['Sea Surface Temperature'])

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

   seasurfacetemperatureqm = g.vars.create('QualityMarker/seaSurfaceTemperature', ioda.Types.int, scales=[dim_location],params=pint)
   seasurfacetemperatureqm.atts.create('units', ioda.Types.str).writeVector.str(['1'])
   seasurfacetemperatureqm.atts.create('long_name', ioda.Types.str).writeVector.str(['Sea Surface Temperature Quality Marker'])

   # Write the data to the variables
   print("Write data to variables")
   longitude.writeNPArray.float(lon.filled().flatten())
   latitude.writeNPArray.float(lat.filled().flatten())
   datetime.writeNPArray.int64(dhr.filled().flatten())
   stationelevation.writeNPArray.float(elv.filled().flatten())
   temperatureeventcode.writeNPArray.int(tpc.filled().flatten())

   airtemperature.writeNPArray.float(tob.filled().flatten())
   virtualtemperature.writeNPArray.float(tvo.filled().flatten())
   stationpressure.writeNPArray.float(pob.filled().flatten())
   pressure.writeNPArray.float(pob.filled().flatten())
   windnorthward.writeNPArray.float(vob.filled().flatten())
   windeastward.writeNPArray.float(uob.filled().flatten())
   specifichumidity.writeNPArray.float(qob.filled().flatten())
   seasurfacetemperature.writeNPArray.float(sst1.filled().flatten())

   airtemperatureqm.writeNPArray.int(tobqm.filled().flatten())
   virtualtemperatureqm.writeNPArray.int(tvoqm.filled().flatten())
   stationpressureqm.writeNPArray.int(pobqm.filled().flatten())
   specifichumidityqm.writeNPArray.int(qobqm.filled().flatten())
   windeastwardqm.writeNPArray.int(uobqm.filled().flatten())
   windnorthwardqm.writeNPArray.int(vobqm.filled().flatten())
   seasurfacetemperatureqm.writeNPArray.int(sstqm.filled().flatten())

   print("end")

if __name__ == '__main__':
   test_bufr_to_ioda()

