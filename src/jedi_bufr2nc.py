#!/usr/bin/env python

from __future__ import print_function
import ncepbufr
import numpy as np
import netCDF4
import sys
import os
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
            # assume that an ID is a 1D array of numbers
            #
            # The bytes.join().decode() method wants the byte values
            # < 127 so that they can be mapped to the old style ascii
            # character set. In order to accommodate this, unpack the
            # float value into bytes. Then check the bytes and replace
            # all values > 127 with a blank character. Then convert
            # the byte lists to strings. Replace byte value
            # equal to zero with a blank as well.
            TempStrList = []
            for i in range(Dnum.size):
                ByteList = list(struct.unpack('8c', Dnum[i]))

                # replace chars < 1 and > 127 with blank space
                for j in range(len(ByteList)):
                    ByteVal = struct.unpack('@B', ByteList[j])[0]
                    if ( (ByteVal < 1) or (ByteVal > 127)):
                        ByteList[j] = b' '

                TempStr = bytes.join(b'', ByteList).decode('ascii') 
                TempStrList.append(TempStr)
            Dval = np.array(TempStrList, dtype='S8')
        elif (Dtype == 'integer'):
            # convert missing vals to MissingInt
            Dval = Dnum.astype(np.integer)
            Dval[Dnum >= 10**9] = MissingInt
        elif (Dtype == 'float'):
            # convert missing vals to nans
            Dval = np.copy(Dnum)
            Dval[Dnum >= 10**9] = np.nan

    return Dval

###########################################################################
# MAIN
###########################################################################

# ObsList holds the list of message types for a given obs type. The format 
# for each element of the list is:
#
#  <obs_type> : [ <list_of_bufr_message_types>, <list_of_bufr_data_types>,
#                 <list_of_bufr_event_types> ]
#
ObsList = {
    # Aircraft with conventional obs
    # Specs are from GSI read_prepbufr.f90
    'Aircraft': [
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
MessageList = ObsList[ObsType][0]
DataList    = ObsList[ObsType][1]
EventList   = ObsList[ObsType][2]

# open files
bufr = ncepbufr.open(PrepbufrFname)

NumMsgs = 0
NumSelectedMsgs = 0
NumDataSubsets = 0
NumEventSubsets = 0
while (bufr.advance() == 0): 
    NumMsgs += 1

    # Select only the messages that belong to this observation type
    if (bufr.msg_type in MessageList):
        NumSelectedMsgs += 1

        # Load each subset and grab the data for each item in DataList
        # and EventList. The bufr.read_subset() method will return
        # a 2D array for non events, and a 3D array for events.
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
        while (bufr.load_subset() == 0):
            for Dname in DataList:
                NumDataSubsets += 1
                Dval = ReadBufrData(Dname, 'data', DataTypes[Dname])
                print("DEBUG: Read Data: ", Dname, Dval, Dval.shape)

            for Ename in EventList:
                NumEventSubsets += 1
                Eval = ReadBufrData(Dname, 'event', DataTypes[Dname])
                print("DEBUG: Event Data: ", Ename, Eval, Eval.shape)
        

print("{0:d} messages selected out of {1:d} messages in the file".format(
      NumSelectedMsgs, NumMsgs))
print("  {0:d} data subsets selcted".format(NumDataSubsets))
print("  {0:d} event subsets selcted".format(NumEventSubsets))


bufr.close()

