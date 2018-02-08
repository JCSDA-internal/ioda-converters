#!/usr/bin/env python

from __future__ import print_function
import ncepbufr
import numpy as np
import sys
import os
import netCDF4
from netCDF4 import Dataset
import struct

###########################################################################
# SUBROUTINES
###########################################################################

def ReadBufrData(Dname, Btype, Dtype):
    # This routine will read one data piece (one mnemonic) from the BUFR file.
    #
    # The bufr.read_subset() method will return a 2D array
    # for non events, and a 3D array for events.
    #
    #   Non-event: data[nm, nlev]
    #     nm is the number of mnemonics
    #     nlev is the nubmer of levels
    #
    #   Event: data[nm, nlev, nec]
    #      nm and nlev as for non-event
    #      nec is the number of event codes
    #
    # Since we are calling these routines with one mnemonic at a time
    # squeeze off the first dimension (it will always be size one).
    #
    # bufr.read_subset() will return a floating point number (for any
    # type) or an empty list if the mnemonic didn't exist. For strings
    # (Dtype = 'string') read the floating point number as characters. Otherwise
    # convert to integer or leave alone.
    #
    # Keep Dtype values in sync with entries in the DataTypes dictionary. For now,
    # these values are "string", "integer" and "float".

    MissingInt = -9999

    # Check for valid Btype and Dtype
    if ((Btype != 'data') and (Btype != 'event') and
        (Btype != 'rep') and (Btype != 'seq')):
        print("ERROR: ReadBufrData: unrecognized bufr type: ", Btype)
        sys.exit(-1)

    if ((Dtype != 'string') and (Dtype != 'integer') and
        (Dtype != 'float')):
        print("ERROR: ReadBufrData: unrecognized data type: ", Dtype)
        sys.exit(-1)
    
    # Determine flags for the read_subset() call
    Eflag = False  # not an event
    Rflag = False  # not a repetition
    Sflag = False  # not a sequence
    if (Btype == 'event'):
        Eflag = True
    elif (Btype == 'rep'):
        Rflag = True
    elif (Btype == 'seq'):
        Sflag = True

    # Dnum will be a numpy array of floats
    Dnum = np.squeeze(bufr.read_subset(Dname, events=Eflag, rep=Rflag, seq=Sflag ).data, axis=0)

    # Make sure return value is a numpy.array type.
    if (Dnum.size == 0):
        # array is empty

        if (Dtype == 'string'):
           # character array
           Dval = np.array([ '' ])
        elif (Dtype == 'integer'): 
           Dval = np.array([ MissingInt ])
        elif (Dtype == 'float'):
           Dval = np.array([ np.nan ])
    else:
        # array is not empty

        if (Dtype == 'string'):
            # convert to list of strings
            # assume that an ID is a 1D array of float with only one
            # entry
            #
            # The bytes.join().decode() method wants the byte values
            # < 127 so that they can be mapped to the old style ascii
            # character set. In order to accommodate this, unpack the
            # float value into bytes. Then check the bytes and replace
            # all values > 127 with a blank character. Then convert
            # the byte lists to strings. Replace byte value
            # equal to zero with a blank as well.
            ByteList = list(struct.unpack('8c', Dnum[0]))

            # replace chars < 1 and > 127 with blank space
            for j in range(len(ByteList)):
                ByteVal = struct.unpack('@B', ByteList[j])[0]
                if ( (ByteVal < 1) or (ByteVal > 127)):
                    ByteList[j] = b' '

            TempStr = bytes.join(b'', ByteList).decode('ascii') 
            Dval = np.array(TempStr, dtype='S8')
        elif (Dtype == 'integer'):
            # convert missing vals to MissingInt
            Dval = Dnum.astype(np.integer)
            Dval[Dnum >= 10**9] = MissingInt
        elif (Dtype == 'float'):
            # convert missing vals to nans
            Dval = np.copy(Dnum)
            Dval[Dnum >= 10**9] = np.nan

    return Dval

def CreateNcVar(Fid, Dname, Btype, Dtype, NobsDname, NlevsDname,
                NeventsDname, StrDname, MaxStringLen):
    # This routine will create a variable in the output netCDF file
    #
    # The dimensions are set according to the Dtype and Btype inputs.
    #
    #   Dtype       Dims
    #  'string'  [nobs, nstring]
    #  'integer' [nobs, nlevs]
    #  'float'   [nobs, nlevs]
    #
    # If Btype is 'event', then append the nevents dim on the end
    # of the dim specification.
    #
    #  'string'  [nobs, nstring, nevents]
    #  'integer' [nobs, nlevs, nevents]
    #  'float'   [nobs, nlevs, nevents]
    #
    if (Dtype == 'string'):
        Vtype = 'S1'
        DimSpec = [NobsDname, StrDname]
        ChunkSpec = [1, MaxStringLen]
    elif (Dtype == 'integer'):
        Vtype = 'i4'
        DimSpec = [NobsDname, NlevsDname]
        ChunkSpec = [1, 1]
    elif (Dtype == 'float'):
        Vtype = 'f4'
        DimSpec = [NobsDname, NlevsDname]
        ChunkSpec = [1, 1]

    if (Btype == 'data'):
        Vname = Dname
    elif (Btype == 'event'):
        Vname = "{0:s}_benv".format(Dname)
        DimSpec.append(NeventsDname)
        ChunkSpec.append(1)

    Fid.createVariable(Vname, Vtype, DimSpec, chunksizes=ChunkSpec, zlib=False)

def WriteNcVar(Fid, Nobs, Dname, Btype, Dval, MaxStringLen):
    # This routine will write into a variable in the output netCDF file

    # Set the variable name according to Btype
    if (Btype == 'data'):
        Vname = Dname
    elif (Btype == 'event'):
        Vname = "{0:s}_benv".format(Dname)

    # For the string data, convert to a numpy character array
    if ((Dval.dtype.char == 'S') or (Dval.dtype.char == 'U')):
        StrSpec = "S{0:d}".format(MaxStringLen)
        Value = netCDF4.stringtochar(Dval.astype(StrSpec))
    else:
        Value = Dval.copy()

    # Find the dimension size of the value and use that to write out
    # the variable.
    Ndims = Value.ndim
    if (Ndims == 1):
        Nlevs = Value.shape[0]
        Fid[Vname][Nobs,0:Nlevs] = Value
    elif (Ndims == 2):
        Nlevs, N2 = Value.shape
        Fid[Vname][Nobs,0:Nlevs,0:N2] = Value
    elif (Ndims == 3):
        Nlevs, N2, N3 = Value.shape
        Fid[Vname][Nobs,Nlevs,0:N2,0:N3] = Value
    

###########################################################################
# MAIN
###########################################################################

# ObsList holds the list of message types for a given obs type. The format 
# for each element of the list is:
#
#  <obs_type> : [ <level_type> <list_of_bufr_message_types>,
#                 <list_of_bufr_data_types>, <list_of_bufr_event_types> ]
#
ObsList = {
    # Aircraft with conventional obs
    # Specs are from GSI read_prepbufr.f90
    'Aircraft': [
        # level type (single or multi)
        'single', 

        # BUFR message types
        [ 'AIRCFT', 'AIRCAR' ],

        # BUFR data types
        [ 'SID',  'ACID', 'XOB',  'YOB',  'DHR',  'TYP',  'ELV',  'SAID', 'T29',
          'POB',  'QOB',  'TOB',  'ZOB',  'UOB',  'VOB',  'PWO',
          'MXGS', 'HOVI', 'CAT',  'PRSS', 'TDO',  'PMO',
          'POE',  'QOE',  'TOE',  'WOE',  'PWE',
          'PQM',  'QQM',  'TQM',  'ZQM',  'WQM',  'PWQ',  'PMQ',
          'XDR',  'YDR',  'HRDR', 'POAF', 'IALR' ],

        # BUFR event types
        [ 'TPC',  'TOB',  'TQM' ]
        ],

    }

# DataTypes maps the mnemonic to its associated data type
# 
# For now the allowed types are:
#   'string'     for CCITT IA5 units in the BUFR table
#   'integer'    for CODE TABLE units in the BUFR table
#   'float'      all other units in the BUFR table
#
# Keep these types in sync with the ReadBufrData() routine
DataTypes = {
    'SID'  : 'string',
    'ACID' : 'string',
    'XOB'  : 'float',
    'YOB'  : 'float',
    'DHR'  : 'float',
    'TYP'  : 'integer',
    'ELV'  : 'float',
    'SAID' : 'integer',
    'T29'  : 'integer',
    'POB'  : 'float',
    'QOB'  : 'float',
    'TOB'  : 'float',
    'ZOB'  : 'float',
    'UOB'  : 'float',
    'VOB'  : 'float',
    'PWO'  : 'float',
    'MXGS' : 'float',
    'HOVI' : 'float',
    'CAT'  : 'integer',
    'PRSS' : 'float',
    'TDO'  : 'float',
    'PMO'  : 'float',
    'POE'  : 'float',
    'QOE'  : 'float',
    'TOE'  : 'float',
    'WOE'  : 'float',
    'PWE'  : 'float',
    'PQM'  : 'integer',
    'QQM'  : 'integer',
    'TQM'  : 'integer',
    'ZQM'  : 'integer',
    'WQM'  : 'integer',
    'PWQ'  : 'integer',
    'PMQ'  : 'integer',
    'XDR'  : 'float',
    'YDR'  : 'float',
    'HRDR' : 'float',
    'POAF' : 'integer',
    'IALR' : 'float', 
    'TPC'  : 'integer',
    'TOB'  : 'float',
    'TQM'  : 'integer',
    }


# Grab input arguemnts
ScriptName = os.path.basename(sys.argv[0])
UsageString = "USAGE: {0:s} <obs_type> <input_prepbufr> <output_netcdf>".format(ScriptName)

if len(sys.argv) != 4:
    print("ERROR: must supply exactly 3 arguments")
    print(UsageString)
    sys.exit(1)

ObsType = sys.argv[1]
PrepbufrFname = sys.argv[2]
NetcdfFname = sys.argv[3]

print("Converting BUFR to netCDF")
print("  Observation Type: {0:s}".format(ObsType))
print("  Input BUFR file: {0:s}".format(PrepbufrFname))
print("  Output netCDF file: {0:s}".format(NetcdfFname))
print("")

# Set up selection lists
LevelType   = ObsList[ObsType][0]
MessageList = ObsList[ObsType][1]
DataList    = ObsList[ObsType][2]
EventList   = ObsList[ObsType][3]

# open files
bufr = ncepbufr.open(PrepbufrFname)
nc = Dataset(NetcdfFname, 'w', format='NETCDF4')

# Create the dimensions and variables in the output netcdf files.
#
# ReadBurfData will return a 1D array for non events,
# and a 2D array for events.
#
#   Non-event: data[nlevs]
#     nlevs is the nubmer of levels
#
#   Event: data[nlevs, nevents]
#     nlevs is the nubmer of levels
#     nevents is the number of event codes
#
# Each variable will add a dimension (in the front) to hold
# each observation (subset). This results in:
#
#   Non-event: data[nobs,nlevs]
#   Event: data[nobs,nlevs,nevents]
#
# Make the dimensions adjustable so different sizes of data pieces
# can be accommodated.
#
# Make a dimension for string data type variables. These have to be
# stored as character arrays.
MaxStringLen = 10

NobsDname = "nobs"
NlevsDname = "nlevs"
NeventsDname = "nevents"
StrDname = "nstring"

# dims plus corresponding vars to hold coordinate values
nc.createDimension(NobsDname, None)
nc.createDimension(NlevsDname, None)
nc.createDimension(NeventsDname, None)
nc.createDimension(StrDname, MaxStringLen)

# coordinates
#   these are just counts so use unsigned ints
nc.createVariable(NobsDname, 'u4', (NobsDname), chunksizes=[1])
nc.createVariable(NlevsDname, 'u4', (NlevsDname), chunksizes=[1])
nc.createVariable(NeventsDname, 'u4', (NeventsDname), chunksizes=[1])
nc.createVariable(StrDname, 'u4', (StrDname), chunksizes=[1])

# variables
MtypeVname = "msg_type"
MtypeDtype = "string"
MdateVname = "msg_date"
MdateDtype = "integer"
CreateNcVar(nc, MtypeVname, 'data', MtypeDtype,
            NobsDname, NlevsDname, NeventsDname, StrDname, MaxStringLen)
CreateNcVar(nc, MdateVname, 'data', MdateDtype,
            NobsDname, NlevsDname, NeventsDname, StrDname, MaxStringLen)

for Dname in DataList:
    CreateNcVar(nc, Dname, 'data', DataTypes[Dname],
                NobsDname, NlevsDname, NeventsDname, StrDname, MaxStringLen)

for Ename in EventList:
    CreateNcVar(nc, Ename, 'event', DataTypes[Ename],
                NobsDname, NlevsDname, NeventsDname, StrDname, MaxStringLen)

# Walk through the BUFR file grabbing the data pieces from the
# selected messages, and recording those pieces into the output
# netCDF file.
NumMsgs = 0
NumSelectedMsgs = 0
NumObs = 0
while (bufr.advance() == 0): 
    NumMsgs += 1
    MsgType = np.array(bufr.msg_type)
    MsgDate = np.array([bufr.msg_date])

    # Select only the messages that belong to this observation type
    if (bufr.msg_type in MessageList):
        # Write out obs into the netCDF file as they are read from
        # the BUFR file. Need to start with index zero in the netCDF
        # file so don't increment the counter until after the write.
        while (bufr.load_subset() == 0):
            # Record message type and date with each subset. This is
            # inefficient in storage (lots of redundancy), but is the
            # expected format for now.
            WriteNcVar(nc, NumObs, MtypeVname, 'data', MsgType, MaxStringLen)
            WriteNcVar(nc, NumObs, MdateVname, 'data', MsgDate, MaxStringLen)

            for Dname in DataList:
                Dval = ReadBufrData(Dname, 'data', DataTypes[Dname])
                WriteNcVar(nc, NumObs, Dname, 'data', Dval, MaxStringLen)

            for Ename in EventList:
                Eval = ReadBufrData(Ename, 'event', DataTypes[Ename])
                WriteNcVar(nc, NumObs, Ename, 'event', Eval, MaxStringLen)

            NumObs += 1

        NumSelectedMsgs += 1


# Fill in coordinate values. Simply put in the numbers 1 through N for each
# dimension variable according to that dimension's size.
FileNumObs    = nc.dimensions[NobsDname].size
FileNumLevels = nc.dimensions[NlevsDname].size
FileNumEvents = nc.dimensions[NeventsDname].size
FileStringLen = nc.dimensions[StrDname].size

nc[NobsDname][0:FileNumObs]       = np.arange(FileNumObs) + 1
nc[NlevsDname][0:FileNumLevels]   = np.arange(FileNumLevels) + 1
nc[NeventsDname][0:FileNumEvents] = np.arange(FileNumEvents) + 1
nc[StrDname][0:FileStringLen]     = np.arange(FileStringLen) + 1


nc.virtmp_code = bufr.get_program_code('VIRTMP')

print("{0:d} messages selected out of {1:d} total messages".format(NumSelectedMsgs, NumMsgs))
print("  {0:d} observations read from input BUFR file".format(NumObs))
print("  {0:d} observations recorded in output netCDF file".format(FileNumObs))


bufr.close()

nc.sync()
nc.close()
