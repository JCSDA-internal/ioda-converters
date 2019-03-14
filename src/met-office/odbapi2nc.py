#!/usr/bin/env python

from collections import defaultdict, namedtuple
import sys
import os
import argparse
from math import exp
import ioda_conv_ncio as iconv
import ioda_conv_util
import var_convert
import yaml
import io

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

def CreateKeyTuple(keyDefinitionDict, row, selectColumns, ColumnVarDict, VertcoVarDict):
    returnKey = []
    for keyVariableName in keyDefinitionDict:
        keyVariableValue = None
        if keyVariableName in ColumnVarDict:
            keyVariableValue = row[selectColumns.index(ColumnVarDict[keyVariableName])]
        elif keyVariableName in VertcoVarDict:
            if row[selectColumns.index("vertco_type")] == VertcoVarDict[keyVariableName]:
                keyVariableValue = row[selectColumns.index("vertco_reference_1")]
                # If we're returning a pressure value, we divide by 100 to convert from Pa to hPa.
                # vertco_type=1 ==> pressure, vertco_type=11 ==> derived pressure (from aircraft altitude)
                if (VertcoVarDict[keyVariableName] == 1 or VertcoVarDict[keyVariableName] == 11) and keyVariableValue is not None:
                    keyVariableValue /= 100.0
        elif keyVariableName == "analysis_date_time":
            keyVariableValue = ioda_conv_util.IntDateTimeToString(row[selectColumns.index("andate")], row[selectColumns.index("antime")])
        elif keyVariableName == "date_time":
            keyVariableValue = ioda_conv_util.IntDateTimeToString(row[selectColumns.index("date")], row[selectColumns.index("time")])

        if keyVariableValue == None:
            if keyDefinitionDict[keyVariableName] == "float":
                keyVariableValue = IODA_MISSING_VAL
            elif keyDefinitionDict[keyVariableName] == "string":
                keyVariableValue = ""

        if keyDefinitionDict[keyVariableName] == "string":
            keyVariableValue = keyVariableValue.rstrip()
        returnKey.append(keyVariableValue)
    returnKey = tuple(returnKey)
    return returnKey

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("input_odb2", help="path to input ODB-2 file")
ap.add_argument("input_def", help="path to input yaml definition file")
ap.add_argument("output_netcdf", help="path to output netCDF4 file")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output netcdf file")
ap.add_argument("-q", "--qcfilter", action="store_true",
                help="only export rows with good qc flags")

MyArgs = ap.parse_args()

#ObsType = MyArgs.obs_type
Odb2Fname = MyArgs.input_odb2
DefFname = MyArgs.input_def
NetcdfFname = MyArgs.output_netcdf
ClobberOfile = MyArgs.clobber
qcFilter = MyArgs.qcfilter

# Check files
BadArgs = False
if (not os.path.isfile(Odb2Fname)): 
    print("ERROR: {0:s}: Specified input file does not exist: {1:s}".format(ScriptName, Odb2Fname))
    print("")
    BadArgs = True

if (not os.path.isfile(DefFname)): 
    print("ERROR: {0:s}: Specified definition file does not exist: {1:s}".format(ScriptName, DefFname))
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

# Read in the contents of the definition file
with io.open(DefFname, 'r') as defstream:
    importDef = yaml.load(defstream)

columnVariableDict = importDef['column_variables']
vertcoVariableDict = importDef['vertco_variables']
varnoVariableDict = importDef['varno_variables']

# Define strings that act as keys in yaml definition file.
# The passed yaml file must use these strings, of course.
#yamlColumnVariables = 'column_variables'
# yamlVarnoVariables = 'varno_variables'
# yamlVertcoVariables = 'vertco_variables'
yamlRecordKey ='record_key'
yamlLocationKey = 'location_key'

#print columnVariableDict

# Assemble list of columns to select
selectColumns = []
selectColumns.append("varno")
selectColumns.append("vertco_type")
selectColumns.append("vertco_reference_1")
selectColumns.append("andate")
selectColumns.append("antime")
selectColumns.append("date")
selectColumns.append("time")
selectColumns.append("obsvalue")
selectColumns.append("obs_error")
selectColumns.append("report_status.active")
selectColumns.append("report_status.passive")
selectColumns.append("report_status.rejected")
selectColumns.append("report_status.blacklisted")
selectColumns.append("datum_status.active")
selectColumns.append("datum_status.passive")
selectColumns.append("datum_status.rejected")
selectColumns.append("datum_status.blacklisted")

# The columns above are always selected.
# Next we add the columns that are required by the variables
# in the "column_variables" section of the yaml definition.
for varName in columnVariableDict:
    selectColumns.append(columnVariableDict[varName])

# Assemble the columns to select into a string we'll use in sql
selectColumnsString = ""
for index, columnName in enumerate(selectColumns):
    if (index > 0):
        selectColumnsString += ", "
    selectColumnsString += columnName

#print selectColumnsString

# Assemble the vertco_type 'where' clause we'll use in sql
vertcoTypeSqlString = ""
for index, vertcoVariable in enumerate(vertcoVariableDict):
    if (index > 0):
        vertcoTypeSqlString += " or "
    vertcoTypeSqlString += "vertco_type="+str(vertcoVariableDict[vertcoVariable])

print vertcoTypeSqlString

# Assemble the varno 'where' clause we'll use in sql
varnoSqlString = ""
for index, varnoValue in enumerate(varnoVariableDict):
    if (index > 0):
        varnoSqlString += " or "
    varnoSqlString += "varno="+str(varnoValue)

print varnoSqlString

recordKeyList = []
for varName in importDef[yamlRecordKey]:
    recordKeyList.append((varName, importDef[yamlRecordKey][varName]))

#print recordKeyList

locationKeyList = []
for varName in importDef[yamlLocationKey]:
    locationKeyList.append((varName, importDef[yamlLocationKey][varName]))

#print locationKeyList

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

tupleNames = selectColumnsString.replace('.', '_')
FetchRow = namedtuple('FetchRow', tupleNames)
conn = odb.connect(Odb2Fname)
c = conn.cursor()

# Construct the entire sql statement
sql = "select " + selectColumnsString + " from \"" + Odb2Fname + "\"" + \
    " where (" + vertcoTypeSqlString +") and (" + varnoSqlString + ")"
if qcFilter:
    sql += " and report_status.active=1 and report_status.passive=0 and report_status.rejected=0 and" \
    " report_status.blacklisted=0 and datum_status.active=1 and datum_status.passive=0 and datum_status.rejected=0" \
    " and datum_status.blacklisted=0"
sql += ';'
print sql

c.execute(sql)
row = c.fetchone()
refDateTimeString = None
while row is not None:
    if (refDateTimeString is None):
        refDateTimeString = ioda_conv_util.IntDateTimeToString(row[selectColumns.index("andate")], row[selectColumns.index("antime")])
    obsDateTimeString = ioda_conv_util.IntDateTimeToString(row[selectColumns.index("date")], row[selectColumns.index("time")])
    #Encode the 8 QC bitfields in the ODB API file into a single value for IODA
    qcVal = (row[selectColumns.index("report_status.active")]      * 128 +
             row[selectColumns.index("report_status.passive")]     *  64 +
             row[selectColumns.index("report_status.rejected")]    *  32 +
             row[selectColumns.index("report_status.blacklisted")] *  16 +
             row[selectColumns.index("datum_status.active")]       *   8 +
             row[selectColumns.index("datum_status.passive")]      *   4 +
             row[selectColumns.index("datum_status.rejected")]     *   2 +
             row[selectColumns.index("datum_status.blacklisted")])

    varName = varnoVariableDict[row[selectColumns.index("varno")]]

    #recordKey = row.statid.rstrip(), anDateTimeString
    recordKey = CreateKeyTuple(importDef[yamlRecordKey], row, selectColumns, columnVariableDict, vertcoVariableDict)
    locationKey = CreateKeyTuple(importDef[yamlLocationKey], row, selectColumns, columnVariableDict, vertcoVariableDict)

    ovalKey = varName, ncOvalName
    oerrKey = varName, ncOerrName
    oqcKey = varName, ncOqcName

    rowObsValue = row[selectColumns.index("obsvalue")]
    rowObsError = row[selectColumns.index("obs_error")]
    oval =  rowObsValue if rowObsValue is not None else IODA_MISSING_VAL
    oerr = rowObsError if rowObsError is not None else IODA_MISSING_VAL
    if qcVal is None:
        qcVal = IODA_MISSING_VAL
    
    #Assignment code below is done this way for two reasons:
    # 1. Want to make sure all locations get into IODA, even if they only have 
    #    missing values. (Preserve all the data we can.)
    # 2. There can be multiple entries in the file for each locationKey, but 
    #    we can only keep one. So we choose an entry that is not null/missing, if present.
    if (ovalKey not in obsDataDictTree[recordKey][locationKey] or
         obsDataDictTree[recordKey][locationKey][ovalKey] == IODA_MISSING_VAL):
        obsDataDictTree[recordKey][locationKey][ovalKey] = oval
    if (oerrKey not in obsDataDictTree[recordKey][locationKey] or
         obsDataDictTree[recordKey][locationKey][oerrKey] == IODA_MISSING_VAL):
        obsDataDictTree[recordKey][locationKey][oerrKey] = oerr
    if (oqcKey not in obsDataDictTree[recordKey][locationKey] or
         obsDataDictTree[recordKey][locationKey][oqcKey] == IODA_MISSING_VAL):
        obsDataDictTree[recordKey][locationKey][oqcKey] = qcVal

    row = c.fetchone()

#After all the data from the file is in the dictionary tree, populate "gaps" with IODA missing value.
for recordKey in obsDataDictTree:
    for locationKey in obsDataDictTree[recordKey]:
        for varName in varnoVariableDict.values():
            for varCat in varCategories:
                if (varName, varCat) not in obsDataDictTree[recordKey][locationKey]:
                    obsDataDictTree[recordKey][locationKey][varName, varCat] = IODA_MISSING_VAL

#For now, we convert relative to specific humidity here.
#This code should be removed eventually, as this is not the right place to convert variables.
#print "statid,lat,lon,datetime,rh,rh_err,t,p,q,q_err"
for recordKey in obsDataDictTree:
    for locationKey in obsDataDictTree[recordKey]:
        if ("relative_humidity", ncOvalName) in obsDataDictTree[recordKey][locationKey]:
            obsDict = obsDataDictTree[recordKey][locationKey]
            rh = obsDict[("relative_humidity", ncOvalName)]
            rh_err = obsDict[("relative_humidity", ncOerrName)]
            t = obsDict.get(("air_temperature", ncOvalName))
            p = locationKey[2]
            if (t is not None and rh is not None and rh_err is not None and p is not None and 
            t != IODA_MISSING_VAL and rh != IODA_MISSING_VAL and rh_err != IODA_MISSING_VAL and p != IODA_MISSING_VAL):
                q, q_err = var_convert.ConvertRelativeToSpecificHumidity(rh, rh_err, t, p)

                obsDict[("specific_humidity", ncOvalName)] = q
                obsDict[("specific_humidity", ncOerrName)] = q_err
                obsDict[("specific_humidity", ncOqcName)] = obsDict[("relative_humidity", ncOqcName)]
            else:
                obsDict[("specific_humidity", ncOvalName)] = IODA_MISSING_VAL
                obsDict[("specific_humidity", ncOerrName)] = IODA_MISSING_VAL
            obsDict[("specific_humidity", ncOqcName)] = obsDict[("relative_humidity", ncOqcName)]
                #print ",".join(map(lambda x: str(x), [recordKey[0],locationKey[0],locationKey[1],locationKey[3],rh,rh_err,t,p,q,q_err]))

# print "Top level len: ", len(obsDataDictTree)
# print "Num Locations: ", len(obsDataDictTree[recordKey])

# Call the writer. Pass in the reference date time string for writing the
# version 1 netcdf file. The reference date time string won't be necessary when
# we switch to the version 2 netcdf file.
AttrData = {
  'odb_version' : 2,
  'date_time_string' : refDateTimeString
   }

nc_writer.BuildNetcdf(obsDataDictTree, AttrData)
