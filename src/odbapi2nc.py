#!/usr/bin/env python

from collections import defaultdict, namedtuple
import sys
import os
import argparse

#TODO:Figure out a more general way to add the path to the ODB API Python module.
#     For now, user needs to export environment variable ODBPYTHONPATH (if not 
#     using default) before running script.
#NOTE: ODB API must be built on the system with the ENABLE_PYTHON flag on or else
#      the python module won't be there.
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

#The top-level dictionary is keyed by (statid, andate, antime), which uniquely identifiies a profile (balloon launch).
#The second-level dictionary is keyed by (lat, lon, pressure, date, time), which uniquely identifies a location
#The third (bottom) level is keyed by a variable name and contains the value of the variable at the location.
stationDict = defaultdict(defaultdict(dict))

fetchColumns = 'statid, andate, antime, stalt, lon, lat, vertco_reference_1, date, time, obsvalue, varno, obs_error, report_status.rejected, datum_status.rejected'
tupleNames   = 'statid, andate, antime, stalt, lon, lat, vertco_reference_1, date, time, obsvalue, varno, obs_error, report_status_rejected, datum_status_rejected'
FetchRow = namedtuple('FetchRow', tupleNames)
conn = odb.connect(Odb2Fname)
c = conn.cursor()

sql = "select " + fetchColumns + " from \"" + Odb2Fname + "\" where vertco_type=1 and (varno=2 or varno=3 or varno=4 or varno=29);"
print sql
c.execute(sql)
for row in map(FetchRow._make, c.fetchall()):
    print row.varno,row.obsvalue

