#!/usr/bin/env python

from collections import defaultdict, namedtuple
import sys
import os
import argparse

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

#NOTE: As of December 11, 2018, the ODB API python package is built into the Singularity image and
#      put in /usr/local/lib/python2.7/dist-packages/odb, so the code below regarding the path
#      is unneeded in that environment (but it doesn't hurt anything). 
#      If not in Singularity/Charliecloud, then note ODB API must be built on the system with the 
#      ENABLE_PYTHON flag on or else the python module won't be there.
odbPythonPath = os.getenv('ODBPYTHONPATH', os.environ['HOME'] + '/projects/odb/install/lib/python2.7/site-packages/odb')
print odbPythonPath

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

#The top-level dictionary is keyed by (statid, andate, antime), which uniquely identifiies a profile (balloon launch).
#The second-level dictionary is keyed by (lat, lon, pressure, date, time), which uniquely identifies a location
#The third (bottom) level is keyed by a variable name and contains the value of the variable at the location.
obsDataDictTree = defaultdict(lambda:defaultdict(dict))

fetchColumns = 'statid, andate, antime, stalt, lon, lat, vertco_reference_1, date, time, obsvalue, varno, obs_error, ' \
               'report_status.active, report_status.rejected, datum_status.active, datum_status.rejected'
tupleNames = fetchColumns.replace('.', '_')
FetchRow = namedtuple('FetchRow', tupleNames)
conn = odb.connect(Odb2Fname)
c = conn.cursor()

sql = "select " + fetchColumns + " from \"" + Odb2Fname + "\"" + \
    " where vertco_type=1 and (varno=2 or varno=3 or varno=4 or varno=29);"
print sql
c.execute(sql)
row = c.fetchone()
while row is not None:
    row = FetchRow._make(row)
    #for row in map(FetchRow._make, c.fetchall()):
    anDateTimeString = IntDateTimeToString(row.andate, row.antime)
    obsDateTimeString = IntDateTimeToString(row.date, row.time)
    if (row.report_status_active == 1 and \
        row.datum_status_active == 1 and \
        row.report_status_rejected != 1 and \
        row.datum_status_rejected != 1):
        qcVal = 1
    else:
        qcVal = 0

    varName = sondeVarnoDict[row.varno]
    profileKey = row.statid, anDateTimeString
    locationKey = row.lat, row.lon, row.vertco_reference_1, obsDateTimeString

    obsDataDictTree[profileKey][locationKey][varName + "@ObsValue"] = row.obsvalue
    obsDataDictTree[profileKey][locationKey][varName + "@ObsError"] = row.obs_error
    obsDataDictTree[profileKey][locationKey][varName + "@ObsQc"] = qcVal
    
    row = c.fetchone()
    #print varName, profileKey, locationKey
    #print varName + "@ObsValue", obsDataDictTree[profileKey][locationKey][varName + "@ObsValue"]

# print "Top level len: ", len(obsDataDictTree)
# print "Num Locations: ", len(obsDataDictTree[profileKey])
