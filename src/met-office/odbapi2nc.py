#!/usr/bin/env python

from collections import defaultdict, namedtuple
import sys
import os
import argparse
from math import exp
import ioda_conv_ncio as iconv

#This function takes two integers like date=20180415 time=61532 and converts them to a
#ISO 8601 standard date/time string like "2018-04-15T06:15:32Z"
def IntDateTimeToString(date, time):
    #Make sure passed values are int's since we're counting on integer division
    date = int(date)
    time = int(time)
    #Define consts to prevent new int objects being created every time we use these numbers
    TEN_THOW = 10000
    HUNDRED = 100

    year = date // TEN_THOW
    date = date - year * TEN_THOW
    month = date // HUNDRED
    day = date - month * HUNDRED

    hour = time // TEN_THOW
    time = time - hour * TEN_THOW
    minute = time // HUNDRED
    second = time - minute * HUNDRED

    return "%d-%02d-%02dT%02d:%02d:%02dZ" % (year, month, day, hour, minute, second)

def ConvertRelativeToSpecificHumidity(rh, rh_err, t, p):
    T_KELVIN = 273.15
    ES_ALPHA = 6.112
    ES_BETA = 17.67
    ES_GAMMA = 243.5
    GAS_CONSTANT = 287.0
    GAS_CONSTANT_V = 461.6
    HUNDRED = 100.0
    
    rdOverRv = GAS_CONSTANT / GAS_CONSTANT_V
    rdOverRv1 = 1.0 - rdOverRv
    t_celcius = t - T_KELVIN
    #p = p / HUNDRED # Convert from Pa to hPa

    #Calculate saturation vapor pressure
    es = ES_ALPHA * exp(ES_BETA * t_celcius / (t_celcius + ES_GAMMA))
    #Calculate saturation specific humidity
    qs = rdOverRv * es / (p - rdOverRv1 * es)
    #Calculate specific humidity
    q = qs * rh / HUNDRED
    q_err = qs * rh_err / HUNDRED
    return q, q_err


#NOTE: As of December 11, 2018, the ODB API python package is built into the Singularity image and
#      put in /usr/local/lib/python2.7/dist-packages/odb, so the code below regarding the path
#      is unneeded in that environment (but it doesn't hurt anything). 
#      If not in Singularity/Charliecloud, then note ODB API must be built on the system with the 
#      ENABLE_PYTHON flag on or else the python module won't be there.
odbPythonPath = os.getenv('ODBPYTHONPATH', os.environ['HOME'] + '/projects/odb/install/lib/python2.7/site-packages/odb')
#print odbPythonPath

try:
    sys.path.index(odbPythonPath)
except ValueError:
    sys.path.append(odbPythonPath)

import odb

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
#ap.add_argument("obs_type", help="observation type")
ap.add_argument("input_odb2", help="path to input Met Office ODB API file")
ap.add_argument("output_netcdf", help="path to output netCDF4 file")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output netcdf file")

MyArgs = ap.parse_args()

#ObsType = MyArgs.obs_type
Odb2Fname = MyArgs.input_odb2
NetcdfFname = MyArgs.output_netcdf
ClobberOfile = MyArgs.clobber

# Check files
BadArgs = False
if (not os.path.isfile(Odb2Fname)): 
    print("ERROR: {0:s}: Specified input file does not exist: {1:s}".format(ScriptName, Odb2Fname))
    print("")
    BadArgs = True

if (os.path.isfile(NetcdfFname)):
    if (ClobberOfile):
        print("WARNING: {0:s}: Overwriting nc file: {1:s}".format(ScriptName, NetcdfFname))
        print("")
    else:
        print("ERROR: {0:s}: Specified nc file already exists: {1:s}".format(ScriptName, NetcdfFname))
        print("ERROR: {0:s}:   Use -c option to overwrite.".format(ScriptName))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

sondeVarnoDict = {
    2:  "air_temperature",
    3:  "eastward_wind",
    4:  "northward_wind",
    29: "relative_humidity"
}

recordKeyList = [
    ( "station_id", "string" ),
    ( "analysis_date_time", "string" )
    ]

locationKeyList = [
    ( "latitude", "float" ),
    ( "longitude", "float" ),
    ( "air_pressure", "float" ),
    ( "date_time", "string" )
    ]

# Instantiate a netcdf writer object, and get the obs data names from
# the writer object.
nc_writer = iconv.NcWriter(NetcdfFname, recordKeyList, locationKeyList)

ncOvalName = nc_writer.OvalName()
ncOerrName = nc_writer.OerrName()
ncOqcName  = nc_writer.OqcName()

IODA_MISSING_VAL = 1.0e9 #IODA converts any value larger than 1e8 to "Missing Value"
varCategories = [ncOvalName, ncOerrName, ncOqcName]

#The top-level dictionary is keyed by (statid, andate, antime), which uniquely identifiies a profile (balloon launch).
#The second-level dictionary is keyed by (lat, lon, pressure, date, time), which uniquely identifies a location
#The third (bottom) level is keyed by a variable name and contains the value of the variable at the location.
obsDataDictTree = defaultdict(lambda:defaultdict(dict))

fetchColumns = 'statid, andate, antime, stalt, lon, lat, vertco_reference_1, date, time, obsvalue, varno, obs_error, ' \
               'report_status.active, report_status.passive, report_status.rejected, report_status.blacklisted, ' \
               'datum_status.active, datum_status.passive, datum_status.rejected, datum_status.blacklisted'
tupleNames = fetchColumns.replace('.', '_')
FetchRow = namedtuple('FetchRow', tupleNames)
conn = odb.connect(Odb2Fname)
c = conn.cursor()

#vertco_type=1 indicates pressure
#vertco_type=11 indicates "derived pressure", which is a pressure value calculated from airplane flight level
#               instead of directly measured. Derived pressure is not present in radiosonde files, but is in
#               aircraft files. We are treating both types identically in this code.
sql = "select " + fetchColumns + " from \"" + Odb2Fname + "\"" + \
    " where (vertco_type=1 or vertco_type=11) and " + \
    "(varno=2 or varno=3 or varno=4 or varno=29);"
print sql
c.execute(sql)
row = c.fetchone()
refDateTimeString = "UnSeT"
while row is not None:
    row = FetchRow._make(row)
    anDateTimeString = IntDateTimeToString(row.andate, row.antime)
    if (refDateTimeString == "UnSeT"):
        refDateTimeString = anDateTimeString
    obsDateTimeString = IntDateTimeToString(row.date, row.time)
    #Encode the 8 QC bitfields in the ODB API file into a single value for IODA
    qcVal = (row.report_status_active      * 128 +
             row.report_status_passive     *  64 +
             row.report_status_rejected    *  32 +
             row.report_status_blacklisted *  16 +
             row.datum_status_active       *   8 +
             row.datum_status_passive      *   4 +
             row.datum_status_rejected     *   2 +
             row.datum_status_blacklisted)
    varName = sondeVarnoDict[row.varno]

    profileKey = row.statid.rstrip(), anDateTimeString
    #TODO: For now we convert pressure (vertco_reference_1) to hPa here. No units stored yet.
    pressure = row.vertco_reference_1 / 100.0 if row.vertco_reference_1 is not None else IODA_MISSING_VAL
    locationKey = row.lat, row.lon, pressure, obsDateTimeString
    ovalKey = varName, ncOvalName
    oerrKey = varName, ncOerrName
    oqcKey = varName, ncOqcName

    oval = row.obsvalue if row.obsvalue is not None else IODA_MISSING_VAL
    oerr = row.obs_error if row.obs_error is not None else IODA_MISSING_VAL
    if qcVal is None:
        qcVal = IODA_MISSING_VAL
    
    #Assignment code below is done this way for two reasons:
    # 1. Want to make sure all locations get into IODA, even if they only have 
    #    missing values. (Preserve all the data we can.)
    # 2. There can be multiple entries in the file for each locationKey, but 
    #    we can only keep one. So we choose an entry that is not null/missing, if present.
    if (ovalKey not in obsDataDictTree[profileKey][locationKey] or
         obsDataDictTree[profileKey][locationKey][ovalKey] == IODA_MISSING_VAL):
        obsDataDictTree[profileKey][locationKey][ovalKey] = oval
    if (oerrKey not in obsDataDictTree[profileKey][locationKey] or
         obsDataDictTree[profileKey][locationKey][oerrKey] == IODA_MISSING_VAL):
        obsDataDictTree[profileKey][locationKey][oerrKey] = oerr
    if (oqcKey not in obsDataDictTree[profileKey][locationKey] or
         obsDataDictTree[profileKey][locationKey][oqcKey] == IODA_MISSING_VAL):
        obsDataDictTree[profileKey][locationKey][oqcKey] = qcVal

    # obsDataDictTree[profileKey][locationKey][ovalKey] = row.obsvalue
    # obsDataDictTree[profileKey][locationKey][oerrKey] = row.obs_error
    # obsDataDictTree[profileKey][locationKey][oqcKey] = qcVal
    
    row = c.fetchone()

#After all the data from the file is in the dictionary tree, populate "gaps" with IODA missing value.
for profileKey in obsDataDictTree:
    for locationKey in obsDataDictTree[profileKey]:
        for varName in sondeVarnoDict.values():
            for varCat in varCategories:
                if (varName, varCat) not in obsDataDictTree[profileKey][locationKey]:
                    obsDataDictTree[profileKey][locationKey][varName, varCat] = IODA_MISSING_VAL

#For now, we convert relative to specific humidity here.
#This code should be removed eventually, as this is not the right place to convert variables.
#print "statid,lat,lon,datetime,rh,rh_err,t,p,q,q_err"
for profileKey in obsDataDictTree:
    for locationKey in obsDataDictTree[profileKey]:
        if ("relative_humidity", ncOvalName) in obsDataDictTree[profileKey][locationKey]:
            obsDict = obsDataDictTree[profileKey][locationKey]
            rh = obsDict[("relative_humidity", ncOvalName)]
            rh_err = obsDict[("relative_humidity", ncOerrName)]
            t = obsDict.get(("air_temperature", ncOvalName))
            p = locationKey[2]
            if (t is not None and rh is not None and rh_err is not None and p is not None and 
            t != IODA_MISSING_VAL and rh != IODA_MISSING_VAL and rh_err != IODA_MISSING_VAL and p != IODA_MISSING_VAL):
                q, q_err = ConvertRelativeToSpecificHumidity(rh, rh_err, t, p)

                obsDict[("specific_humidity", ncOvalName)] = q
                obsDict[("specific_humidity", ncOerrName)] = q_err
                obsDict[("specific_humidity", ncOqcName)] = obsDict[("relative_humidity", ncOqcName)]
            else:
                obsDict[("specific_humidity", ncOvalName)] = IODA_MISSING_VAL
                obsDict[("specific_humidity", ncOerrName)] = IODA_MISSING_VAL
            obsDict[("specific_humidity", ncOqcName)] = obsDict[("relative_humidity", ncOqcName)]
                #print ",".join(map(lambda x: str(x), [profileKey[0],locationKey[0],locationKey[1],locationKey[3],rh,rh_err,t,p,q,q_err]))

# print "Top level len: ", len(obsDataDictTree)
# print "Num Locations: ", len(obsDataDictTree[profileKey])
print ""

# Call the writer. Pass in the reference date time string for writing the
# version 1 netcdf file. The reference date time string won't be necessary when
# we switch to the version 2 netcdf file.
AttrData = {
  'odb_version' : 2,
  'date_time_string' : refDateTimeString
   }

nc_writer.BuildNetcdf(obsDataDictTree, AttrData)
