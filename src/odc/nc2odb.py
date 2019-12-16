#!/usr/bin/env python

import sys
import os
import tempfile
import argparse
import datetime as dt
import netCDF4 as nc
import codyssey as odc

###################################################################################
# SUBROUTINES
###################################################################################


def CharArrayToStrings(CharArray):
    Strings = []
    for ByteList in CharArray:
        Strings.append(b''.join(ByteList).decode('ascii'))
    return Strings


def ConvertDateTime(DtStrings):
    Dates = []
    Times = []
    for DtStr in DtStrings:
        Dt = dt.datetime.strptime(DtStr, "%Y-%m-%dT%H:%M:%SZ")

        Date = Dt.year * 10000
        Date += Dt.month * 100
        Date += Dt.day

        Time = Dt.hour * 10000
        Time += Dt.minute * 100
        Time += Dt.second

        Dates.append(Date)
        Times.append(Time)

    return (Dates, Times)


###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("input_netcdf", help="path to input netCDF4 file")
ap.add_argument("output_odb", help="path to output ODB file")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output ODB file")
ap.add_argument("-k", "--keep_csv", action="store_true",
                help="do not delete the temporary CSV file")

MyArgs = ap.parse_args()

NetcdfFname = MyArgs.input_netcdf
OdbFname = MyArgs.output_odb
ClobberOfile = MyArgs.clobber
KeepCsv = MyArgs.keep_csv

# Check files
BadArgs = False
if (not os.path.isfile(NetcdfFname)):
    print("ERROR: {0:s}: Specified input netCDF4 file does not exist: {1:s}".format(
        ScriptName, NetcdfFname))
    print("")
    BadArgs = True

if (os.path.isfile(OdbFname)):
    if (ClobberOfile):
        print("WARNING: {0:s}: Overwriting ODB file: {1:s}".format(
            ScriptName, OdbFname))
        print("")
    else:
        print("ERROR: {0:s}: Specified ODB file already exists: {1:s}".format(
            ScriptName, OdbFname))
        print("ERROR: {0:s}:   Use -c option to overwrite.".format(ScriptName))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

# Arguments are okay. Read in the odb file using the xarray interface. Then covert to
# a pandas dataframe, write out the pandas into the odb file.

# Select the variables that are to be transferred to the output which are all the ones
# with nlocs as the first dimension. Also change datetime@MetatData to two integer
# variables: date@MetaData and time@MetaData (with formats YYYYMMDD, hhmmss respectively).
NcDs = nc.Dataset(NetcdfFname)
IntMissingValue = 2147483647
DoubleMissingValue = -2147483647

for NcDim in NcDs.dimensions:
    DimName = NcDs.dimensions[NcDim].name
    DimSize = NcDs.dimensions[NcDim].size
    if (DimName == "nlocs"):
        Nlocs = DimSize

VarList = {}
Nvars = 0
for NcVar in NcDs.variables:
    VarName = NcDs[NcVar].name
    VarType = NcDs[NcVar].datatype
    VarDims = NcDs[NcVar].dimensions
    VarData = NcDs[NcVar][:]

    # only convert the vectors that are indexed by nlocs
    # strings have 2 dimensions (nlocs, string_size) so allow those
    if (VarDims[0] == "nlocs"):
        if (VarType == "|S1"):
            OdbType = "STRING"
            OdbData = CharArrayToStrings(VarData.filled(""))
            IsVector = True
        if ((VarType == "int32") or (VarType == "int64")):
            OdbType = "INTEGER"
            OdbData = VarData.astype('int64').filled(IntMissingValue)
            IsVector = (len(VarDims) == 1)
        if ((VarType == "float32") or (VarType == "float64")):
            OdbType = "DOUBLE"
            OdbData = VarData.astype('float64').filled(DoubleMissingValue)
            IsVector = (len(VarDims) == 1)

        if (VarName == "datetime@MetaData"):
            (Dates, Times) = ConvertDateTime(OdbData)
            VarList['date@MetaData:INTEGER'] = Dates
            VarList['time@MetaData:INTEGER'] = Times
            Nvars += 2
        else:
            if ((IsVector) and (VarName != "time@MetaData")):
                OdbName = "{}:{}".format(VarName, OdbType)
                VarList[OdbName] = OdbData
                Nvars += 1

print("Number of locations processed: ", Nlocs)
print("Number of variables selected: ", Nvars)
print("")

# Open a temporary file and write out the data in CSV format. Then convert to an ODB
# file using the odc import command.
(CsvFile, CsvFname) = tempfile.mkstemp(prefix="ODB_CSV_")

# As a workaround for a bug in the reader, add an extra column (at the first column position)
# that does not contain any missing values. For some reason, if the very first entry in
# the frame (first row, first column) contains a missing value, the frame data gets
# corrupted.

# Write the header
Line = ['DuMmYiNdEx@MetaData:INTEGER']
for VarName in VarList:
    Line.append(VarName)

OutLine = ",".join(Line) + "\n"
os.write(CsvFile, str.encode(OutLine))

# Write the data
RowNum = 0
for i in range(Nlocs):
    Line = ["{}".format(RowNum)]
    for VarName in VarList:
        if (VarName.endswith("STRING")):
            Line.append("{0:s}".format(VarList[VarName][i]))
        else:
            Line.append("{}".format(VarList[VarName][i]))

    OutLine = ",".join(Line) + "\n"
    os.write(CsvFile, str.encode(OutLine))

    RowNum += 1

os.close(CsvFile)

# Run odc import
OdcCmd = "odc import {} {}".format(CsvFname, OdbFname)
print("Running: ", OdcCmd)
print("")
os.system(OdcCmd)
print("")

# clean up
if(KeepCsv):
    print("Keeping temporary CSV file: ", CsvFname)
else:
    print("Removing temporary CSV file: ", CsvFname)
    os.remove(CsvFname)
