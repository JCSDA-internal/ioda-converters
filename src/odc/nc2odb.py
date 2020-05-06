#!/usr/bin/env python

import sys
import os
import subprocess
from pathlib import Path
prefix_lib_path = Path(__file__).absolute().parent.parent/'lib'
prefix_bin_path = prefix_lib_path.parent/'bin'
sys.path.append(str(prefix_lib_path))
sys.path.append(str(prefix_lib_path/'python{}.{}'.format(*sys.version_info)/'site-packages'))
os.environ['PATH'] = "%s:%s" % (os.environ['PATH'], str(prefix_bin_path))

import tempfile
import argparse
import datetime as dt
import netCDF4 as nc

###################################################################################
# SUBROUTINES
###################################################################################


def CharArrayToStrings(CharArray):
    Strings = []
    for ByteList in CharArray:
        Strings.append(b''.join(ByteList).decode('ascii'))
    return Strings


def ConvertDtStrings(DtStrings):
    Dates = []
    Times = []
    for DtStr in DtStrings:
        Dt = dt.datetime.strptime(DtStr, "%Y-%m-%dT%H:%M:%SZ")
        (Date, Time) = DtToDateTime(Dt)

        Dates.append(Date)
        Times.append(Time)

    return (Dates, Times)


def ConvertDtRefOffset(RefDtInt, OffsetTime):
    # convert the reference date time to a datetime object
    TempRef = RefDtInt
    Year = TempRef // 1000000
    TempRef = TempRef % 1000000
    Month = TempRef // 10000
    TempRef = TempRef % 10000
    Day = TempRef // 100
    Hour = TempRef % 100
    RefDt = dt.datetime(Year, Month, Day, Hour)

    Dates = []
    Times = []
    for Otime in OffsetTime:
        OffsetSeconds = round(3600 * Otime)
        OffsetDt = dt.timedelta(seconds=OffsetSeconds)
        Dt = RefDt + OffsetDt
        (Date, Time) = DtToDateTime(Dt)

        Dates.append(Date)
        Times.append(Time)

    return (Dates, Times)


def DtToDateTime(Dt):
    Date = Dt.year * 10000
    Date += Dt.month * 100
    Date += Dt.day

    Time = Dt.hour * 10000
    Time += Dt.minute * 100
    Time += Dt.second

    return (Date, Time)


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

# Number of locations is the size of the "nlocs" dimension
for NcDim in NcDs.dimensions:
    DimName = NcDs.dimensions[NcDim].name
    DimSize = NcDs.dimensions[NcDim].size
    if (DimName == "nlocs"):
        Nlocs = DimSize

# Determine if we have the new or old style of date time representation in the file
# Old style is reference datetime with time offsets, new style is ISO8601 datetime
# strings. Convert the data according to the style, and initialize the VarList
# dictionary with these
if ("datetime@MetaData" in NcDs.variables):
    print("Input netcdf date time repsrentation: ISO 8601 datetime strings")
    VarData = NcDs["datetime@MetaData"][:]
    (Dates, Times) = ConvertDtStrings(CharArrayToStrings(VarData))
else:
    if (("time@MetaData" in NcDs.variables) and ("date_time" in NcDs.ncattrs())):
        print("Input netcdf date time repsrentation: Reference date time with time offset")
        RefDt = NcDs.getncattr("date_time")
        VarData = NcDs["time@MetaData"][:].astype('float64')
        (Dates, Times) = ConvertDtRefOffset(RefDt, VarData)
    else:
        print("ERROR: could not find date time representation in the input netcdf file")
        sys.exit(3)

VarList = {
    'date@MetaData:INTEGER': Dates,
    'time@MetaData:INTEGER': Times
}
Nvars = len(VarList)

# Walk through the variables, convert missing marks to odb values, and convert date time
# information to date and time integer representation. Then save the processed data for
# the building of the CSV file.
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

        if ((IsVector) and (VarName != "datetime@MetaData") and (VarName != "time@MetaData")):
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
SortedVarList = sorted(VarList)
for VarName in SortedVarList:
    Line.append(VarName)

OutLine = ",".join(Line) + "\n"
os.write(CsvFile, str.encode(OutLine))

# Write the data
RowNum = 0
for i in range(Nlocs):
    Line = ["{}".format(RowNum)]
    for VarName in SortedVarList:
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
subprocess.call(OdcCmd, shell=True)
print("")

# clean up
if(KeepCsv):
    print("Keeping temporary CSV file: ", CsvFname)
else:
    print("Removing temporary CSV file: ", CsvFname)
    os.remove(CsvFname)
