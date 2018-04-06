#!/usr/bin/env python

from __future__ import print_function
import ncepbufr
import numpy as np
import sys
import os
import re
import argparse
import netCDF4
from netCDF4 import Dataset
import struct
import datetime as dt

###########################################################################
# CONFIGURATION
###########################################################################

# Some handy constants. These become global variables in this script. Using the
# naming convention of all caps to remind us that these are not to be changed.

# MAX_STRING_LEN is good with 10 characters. This is the length of the long format
# for date and time. Most of the id labels are 6 or 8 characters.
MAX_STRING_LEN = 10

# MAX_EVENTS will usually be limited to 255 due to an array size in the Fortran
# interface. In practice, there are typically a handful of events (4 or 5) since
# the events are related to the steps that are gone through to convert a raw
# BUFR file to a prepBUFR file (at NCEP). It is generally accepted that 20 is
# a safe limit (instead of 255) for the max number of events, so set MaxEvents
# to 20 to help conserve file space.
MAX_EVENTS = 20

# MAX_LEVELS should be limited to 255 by a Fortran array size. This may need to
# change in the future since this number corresponds to the number of atmospheric
# levels in an observation.
MAX_LEVELS = 255

# BUFR file types
BFILE_UNDEF    = 0
BFILE_BUFR     = 1
BFILE_PREPBUFR = 2

# BUFR types
BTYPE_UNDEF  = 0
BTYPE_HEADER = 1
BTYPE_DATA   = 2
BTYPE_EVENT  = 3
BTYPE_REP    = 4

# Data types
DTYPE_UNDEF   = 0
DTYPE_STRING  = 1   # for CCITT IA5 units in the BUFR table
DTYPE_INTEGER = 2   # for CODE TABLE, FLAG TABLE units in the BUFR table
DTYPE_FLOAT   = 3   # for all other units in the BUFR table
DTYPE_UINT    = 4   # for dimension coordinates
DTYPE_DOUBLE  = 5   # temporary: for strings that are expected to be double
                    #            in downstream flows (GSI)

############################################################################
# CLASSES
############################################################################

# The BUFR format is extremely flexible, and different obs types have taken
# advantage of that fact. This has resulted in the requirement of utilizing
# different algorithms to extract obs data for different obs types. Ie, it's
# extremely difficult to force the different formats into a common algorithm.
# Using a base class with a simple extraction algorithm which can be overridden
# in a derived class seems to be a good way to handle this situation.
#
# For the extraction, it does appear that many obs types will place a header
# at the front of an BUFR subset which consists of a simple list of BUFR
# mnemonics. The header is followed by the obs data which can be a simple
# list of mnemonics, but typically is a more complex structure with 
# replications, sequences and events. The header extraction algorithm can
# (for now) belong in the base class, and the obs data extraction algorithms
# can belong in the derived classes (ie, specific to each obs type).
#
# Define the base class with a simple method that assumes all variables have a
# one-to-one corrspondence with a BUFR mnemonic. More complex examples can
# override the convert(), convert_header(), and/or convert_obs() methods
# and do whatever is necessary. The thought about breaking up convert() into
# two methods, convert_header() and convert_obs(), is that it seems that most
# obs types will be able to utilize the simple convert_header() algorithm.
#
# The format for an entry in the *_spec lists is:
#
#    [ nc_varname, mnemonic, data_type, dim_names, dim_sizes, created ] 
#
#        nc_varname: netcdf variable name
#        mnemonic:   BUFR mnemonic
#        data_type:  float, integer, string, ...
#        dim_names:  (list of dimension names)
#        dim_sizes:  (list of dimension sizes)
#        created:    flag: True  - nc variable has been created
#                          False - nc variable has not been created
#

################################# Base Observation Type ############################
class ObsType(object):
    ### initialize data elements ###
    def __init__(self):
        self.bufr_ftype = BFILE_UNDEF
        self.mtype_re = 'UnDef'
 
        # Keep this list of dimensions in sync with the if statment structure
        # in the init_dim_spec() method.
        self.nobs = -1
        self.nlevs = MAX_LEVELS
        self.nevents = MAX_EVENTS
        self.nstring = MAX_STRING_LEN
        self.nchans = -1

        self.time_units = ""
        self.int_spec = []
        self.evn_spec = []
        self.rep_spec = []
        self.seq_spec = []
        self.dim_spec = []
        self.misc_spec = [
            [ [ 'Time',     '', DTYPE_FLOAT,  ['nobs'],            [self.nobs]               ],
              [ 'msg_type', '', DTYPE_STRING, ['nobs', 'nstring'], [self.nobs, self.nstring] ],
              [ 'msg_date', '', DTYPE_UINT,   ['nobs'],            [self.nobs]               ] ]
            ]

    ### methods ###

    ###############################################################################
    # This method will set the time units. The time units value will be used to
    # calculate time offsets for observation values.
    def set_time_units(self, tunits):
        self.time_units = tunits

    ###############################################################################
    # This method will set the number of observations. This must be called
    # before attempting to create any netcdf variables since self.nobs
    # is also used to define the dimension sizes in all of the netcdf variables.
    def set_nobs(self, nobs):
        # update the data memeber
        self.nobs = nobs

        # update the dimension sizes in the specs
        #
        # each spec is a list of variable specs
        # each variable spec is a list with the fourth item being a list of
        #    dimension names and the fifth item being a list of dimension sizes
        #
        # for every place in the dimension name list where the name is 'nobs', replace
        # the corresponding size in the size list with self.nobs
        for slist in [ self.int_spec, self.evn_spec, self.rep_spec, self.seq_spec,
                      self.dim_spec, self.misc_spec ]:
            for sub_slist in slist:
                for var_spec in sub_slist:
                    for i in [ j for j,dname in enumerate(var_spec[3]) if dname == 'nobs']:
                        var_spec[4][i] = self.nobs

    ###############################################################################
    # This method will set the dimension specs (data memeber self.dim_spec). The
    # format for the dim_spec will match that of the other specs (eg, self.int_spec).
    def init_dim_spec(self):
        # Do a union on all of the dimension names.
        AllDimNames = set([])
        for slist in [ self.int_spec, self.evn_spec, self.rep_spec,
                       self.seq_spec, self.misc_spec ]:
            for sub_slist in slist:
                for var_spec in sub_slist:
                    # dimension names are in the list given by var_spec[3]
                    AllDimNames = AllDimNames | set(var_spec[3])

        # AllDimNames holds the list of unique dimension names.
        DimList = []
        for dname in AllDimNames:
            if (dname == 'nobs'):
                dsize = self.nobs
            elif (dname == 'nlevs'):
                dsize = self.nlevs
            elif (dname == 'nevents'):
                dsize = self.nevents
            elif (dname == 'nstring'):
                dsize = self.nstring
            elif (dname == 'nchans'):
                dsize = self.nchans
            else:
                print("ERROR: init_dim_spec: Unknown dimension name: {0:s}".format(dname))
                sys.exit(3)

            DimList.append([ dname, dname, DTYPE_UINT, [ dname ], [ dsize ] ])

        self.dim_spec = [ DimList ]

    ###############################################################################
    # This method will create dimensions and variables in the netcdf file
    # according to the obs type variable specs.
    def create_nc_datasets(self, fid):

        # Create dimensions first so that the variables can reference them.
        for sub_slist in self.dim_spec:
            for dspec in sub_slist:
                nc.createDimension(dspec[0], dspec[4][0])

        # Create variables including the coordinates for the dimensions
        for slist in [ self.dim_spec, self.int_spec, self.evn_spec,
                      self.rep_spec, self.seq_spec, self.misc_spec ]:
            for sub_slist in slist:
                for var_spec in sub_slist:
                    Vname    = var_spec[0]
                    Dtype    = var_spec[2]
                    DimNames = var_spec[3]
                    DimSizes = var_spec[4]

                    # Convert the data type code to a netCDF data type
                    if (Dtype == DTYPE_STRING):
                        Vtype = 'S1'
                    elif (Dtype == DTYPE_INTEGER):
                        Vtype = 'i4'
                    elif (Dtype == DTYPE_UINT):
                        Vtype = 'u4'
                    elif (Dtype == DTYPE_FLOAT):
                        Vtype = 'f4'
                    elif (Dtype == DTYPE_DOUBLE):
                        Vtype = 'f8'

                    # For the chunk sizes,
                    #   The first dimension is always nobs
                    #   For vars with a single dimension,
                    #       make the chunk spec match dim sizes
                    #   For vars with multiple dimensions,
                    #       make the chunk spec match dim sizes except use 1 for first entry
                    if (len(DimSizes) == 1):
                        ChunkSizes = DimSizes
                    else:
                        ChunkSizes = [ 1 ] + DimSizes[1:]

                    nc.createVariable(Vname, Vtype, DimNames, chunksizes=ChunkSizes,
                                      zlib=True, shuffle=True, complevel=6)

    ###############################################################################
    # This method will fill in the dimension variables with coordinate values.
    # For now, using dummy values which are 1..n where n is the variable size.
    def fill_coords(self, nc):
        for DimSpecs in self.dim_spec:
            for VarSpec in DimSpecs:
                Vname = VarSpec[0]
                Value = np.arange(VarSpec[4][0]) + 1
                nc[Vname][:] = Value

    ###############################################################################
    # This method will read in a list of bufr mnemonics and return a list of
    # the corresponding data values.
    def read_bufr_data(self, bufr, Mlists, Rflag=False, Sflag=False, Eflag=False):
        BufrValues = []

        # Mlists contains sub-lists of mnemonic names. Process each sub-list by
        # reading all mnemonics in that sub-list in one call to read_subset().
        for MnemonicList in Mlists:
            Mstring = " ".join(MnemonicList)
            BufrValues.append(bufr.read_subset(Mstring, events=Eflag, seq=Sflag, rep=Rflag))

        return BufrValues

    ###############################################################################
    # This method will convert bufr float data to the specified actual format.
    # BufrValues is a list of masked arrays, where each masked array contains
    # entries for all mnemonics in the sub-list of SpecList.
    def bufr_float_to_actual(self, SpecList, BufrValues, ActualValues):
        # Make a separate copy of the input dictionary
        OutVals = { key : value for key, value in ActualValues.items() }

        for SubSpecs, SubBvals in zip(SpecList, BufrValues):
            for VarSpec, Bval in zip(SubSpecs, SubBvals):
                # Convert according to the spec, and add to the dictionary.
                # Netcdf variable name is in VarSpec[0]
                # Data type is in VarSpec[2]
                OutVals[VarSpec[0]] = BufrFloatToActual(Bval, VarSpec[2])

        return OutVals

    ###############################################################################
    # This method will take the four input spec lists and read the mnemonics
    # from the bufr file. This routine will also convert the bufr values to
    # corresponding netcdf values. This method will return a dictionary keyed
    # by the netcdf variable name containing the associated values.
    def extract_bufr(self, bufr):
        ActualValues = {}
    
        # Read and convert the individual data mnemonics. The mnemonic value is the second
        # entry in the int_spec sublist elements.
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.int_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists) 
        ActualValues = self.bufr_float_to_actual(self.int_spec, BufrValues, ActualValues)

        # Read and convert the event mnemonics
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.evn_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists, Eflag=True) 
        ActualValues = self.bufr_float_to_actual(self.evn_spec, BufrValues, ActualValues)

        # Read and convert the replication mnemonics
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.rep_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists, Rflag=True) 
        ActualValues = self.bufr_float_to_actual(self.rep_spec, BufrValues, ActualValues)

        # Read and convert the event mnemonics
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.seq_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists, Sflag=True) 
        ActualValues = self.bufr_float_to_actual(self.seq_spec, BufrValues, ActualValues)

        return ActualValues

    ###############################################################################
    # This method will convert the BUFR data into netcdf data. This includes
    # reading BUFR and writing netcdf. This method represents a default that can
    # be used for (hopefully) many obs types. If an obs type requires a more complex
    # method, then this one can be overridden in a derived class. 
    #
    # The default method provides the following:
    #   Copy all BUFR mnemonic values in the variable specs to the output netcdf file
    #   Calculate a time offset from the reference time and store in addition to
    #     the BUFR mnemonic values
    def convert(self, bufr, nc):
        # Walk through the messages, selecting only those that match the regular
        # expression for this obs type.
        print("Converting BUFR to netcdf:")
        ObsNum = 0
        while ((bufr.advance() == 0) and (ObsNum < self.nobs)):
            # Select only the messages that belong to this observation type
            if (re.search(self.mtype_re, bufr.msg_type)):
                MsgType = np.ma.array(bufr.msg_type)
                MsgDate = np.ma.array([bufr.msg_date])
                while (bufr.load_subset() == 0):
                    # Write the message type and message date
                    WriteNcVar(nc, ObsNum, 'msg_type', MsgType)
                    WriteNcVar(nc, ObsNum, 'msg_date', MsgDate)

                    # Grab all of the mnemonics from the bufr file, and convert
                    # to netcdf ready format. ReadConvertBufr() a dictionary 
                    # keyed by the netcdf variable name and containing the associated
                    # data values.
                    ActualValues = self.extract_bufr(bufr)

                    # Write out the netcdf values
                    for Vname, Vdata in ActualValues.items():
                        # Skip the write if Vdata is empty
                        if (Vdata.size > 0):
                            WriteNcVar(nc, ObsNum, Vname, Vdata)

                    # Increment observation number and print out progress messages.
                    ObsNum += 1
                    if ((ObsNum % 100) == 0):
                        print("  Converted {0:d} observations".format(ObsNum))
            
        print("")
        print("  Total converted observations: ", ObsNum)
        print("")

################################# Aircraft Observation Type ############################
class AircraftObsType(ObsType):
    ### initialize data elements ###
    def __init__(self, bf_type):
        super().__init__()

        self.bufr_ftype = bf_type
        if (bf_type == BFILE_BUFR):
            self.mtype_re = '^NC004001'
            self.int_spec = [
                [ [ 'YEAR',   'YEAR',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MNTH',   'MNTH',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'DAYS',   'DAYS',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'HOUR',   'HOUR',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MINU',   'MINU',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ACID',   'ACID',   DTYPE_DOUBLE,  ['nobs'], [self.nobs] ],
                  [ 'CORN',   'CORN',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'CLAT',   'CLAT',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'CLON',   'CLON',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'FLVL',   'FLVL',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],

                [ [ 'TMDB',   'TMDB',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TMDP',   'TMDP',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'REHU',   'REHU',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'WSPD',   'WSPD',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'WDIR',   'WDIR',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'QMAT',   'QMAT',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'QMDD',   'QMDD',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'QMWN',   'QMWN',   DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                [ [ 'SEQNUM', 'SEQNUM', DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'BUHD',   'BUHD',   DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'BORG',   'BORG',   DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'BULTIM', 'BULTIM', DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'BBB',    'BBB',    DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'RPID',   'RPID',   DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ] ],
                ]
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []
        elif (bf_type == BFILE_PREPBUFR):
            self.mtype_re = 'AIRC[AF][RT]'
            self.int_spec = [
                [ [ 'SID',  'SID',  DTYPE_DOUBLE,  ['nobs'], [self.nobs] ],
                  [ 'ACID', 'ACID', DTYPE_DOUBLE,  ['nobs'], [self.nobs] ],
                  [ 'XOB',  'XOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'YOB',  'YOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'DHR',  'DHR',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TYP',  'TYP',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ELV',  'ELV',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'SAID', 'SAID', DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'T29',  'T29',  DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                [ [ 'POB',  'POB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'QOB',  'QOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TOB',  'TOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'ZOB',  'ZOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'UOB',  'UOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'VOB',  'VOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PWO',  'PWO',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'MXGS', 'MXGS', DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PRSS', 'PRSS', DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TDO',  'TDO',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PMO',  'PMO',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],

                [ [ 'PQM',  'PQM',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'QQM',  'QQM',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'TQM',  'TQM',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ZQM',  'ZQM',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'WQM',  'WQM',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'PWQ',  'PWQ',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'PMQ',  'PMQ',  DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                [ [ 'POE',  'POE',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'QOE',  'QOE',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TOE',  'TOE',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'WOE',  'WOE',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PWE',  'PWE',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],

                [ [ 'HOVI', 'HOVI', DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'CAT',  'CAT',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'XDR',  'XDR',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'YDR',  'YDR',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'HRDR', 'HRDR', DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'POAF', 'POAF', DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'IALR', 'IALR', DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],
                ]
            self.evn_spec = [
                [ [ 'TPC_bevn', 'TPC', DTYPE_INTEGER, ['nobs', 'nevents'], [self.nobs, self.nevents] ],
                  [ 'TOB_bevn', 'TOB', DTYPE_FLOAT,   ['nobs', 'nevents'], [self.nobs, self.nevents] ],
                  [ 'TQM_bevn', 'TQM', DTYPE_INTEGER, ['nobs', 'nevents'], [self.nobs, self.nevents] ] ],
                ]
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super().init_dim_spec()

    ### methods ###
    

################################# Radiosonde Observation Type ############################
class SondesObsType(ObsType):
    ### initialize data elements ###
    def __init__(self, bf_type):
        super().__init__()

        self.bufr_ftype = bf_type
        if (bf_type == BFILE_BUFR):
            self.mtype_re = 'UnDef'
            self.int_spec = []
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []
        elif (bf_type == BFILE_PREPBUFR):
            # Clara: THIS LIST IS NOT EXHAUSTIVE!!!!
            #        it is based on dumping a few messages, 
            #        then screening for vars read in by the gsi
            #          1. Header
            #          2. Obs types
            #          3. quality markers
            #          4. error ests.
            #          5. location info?
            #
            # Clara: PREPBUFR FILES INCLUDE (BUT NOT READ BY GSI): 
            #            'TSB',  'ITP',  'SQN','PROCN',  'RPT', 'TCOR', 'SIRC',
            #        EVENTS VARS? *PC, *RC, *FC , TVO
            #
            self.mtype_re = 'ADPUPA'
            self.int_spec = [
                [ [ 'SID',  'SID',  DTYPE_DOUBLE,  ['nobs'], [self.nobs] ],
                  [ 'XOB',  'XOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'YOB',  'YOB',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'DHR',  'DHR',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TYP',  'TYP',  DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ELV',  'ELV',  DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'T29',  'T29',  DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                [ [ 'POB',  'POB',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'QOB',  'QOB',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'TOB',  'TOB',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'ZOB',  'ZOB',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'UOB',  'UOB',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'VOB',  'VOB',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'PWO',  'PWO',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'TDO',  'TDO',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ] ],

                [ [ 'PQM',  'PQM',  DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'QQM',  'QQM',  DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'TQM',  'TQM',  DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'ZQM',  'ZQM',  DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'WQM',  'WQM',  DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'PWQ',  'PWQ',  DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'PMQ',  'PMQ',  DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ] ],

                [ [ 'POE',  'POE',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'QOE',  'QOE',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'TOE',  'TOE',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'WOE',  'WOE',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'PWE',  'PWE',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'XDR',  'XDR',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'YDR',  'YDR',  DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'HRDR', 'HRDR', DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ] ],

                ]
            self.evn_spec = [
                [ [ 'TPC_bevn', 'TPC', DTYPE_INTEGER, ['nobs', 'nlevs', 'nevents'], [self.nobs, self.nlevs, self.nevents] ],
                  [ 'TOB_bevn', 'TOB', DTYPE_FLOAT,   ['nobs', 'nlevs', 'nevents'], [self.nobs, self.nlevs, self.nevents] ],
                  [ 'TQM_bevn', 'TQM', DTYPE_INTEGER, ['nobs', 'nlevs', 'nevents'], [self.nobs, self.nlevs, self.nevents] ] ]
                ]
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super().init_dim_spec()

    ### methods ###
    

########################### Radiance (AMSU-A) Observation Type ############################
class AmsuaObsType(ObsType):
    ### initialize data elements ###
    def __init__(self, bf_type):
        super().__init__()

        self.nchans = 20  # This is unique to AMSU
        self.bufr_ftype = bf_type
        if (bf_type == BFILE_BUFR):

            self.mtype_re = '^NC021023'
            self.int_spec = [
                [ [ 'SAID',   'SAID',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'FOVN',   'FOVN',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'YEAR',   'YEAR',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MNTH',   'MNTH',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'DAYS',   'DAYS',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'HOUR',   'HOUR',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MINU',   'MINU',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'SECO',   'SECO',   DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'CLAT',   'CLAT',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'CLON',   'CLON',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'HOLS',   'HOLS',   DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],

                [ [ 'SAZA',   'SAZA',   DTYPE_FLOAT, ['nobs'], [self.nobs] ],
                  [ 'SOZA',   'SOZA',   DTYPE_FLOAT, ['nobs'], [self.nobs] ],
                  [ 'BEARAZ', 'BEARAZ', DTYPE_FLOAT, ['nobs'], [self.nobs] ],
                  [ 'SOLAZI', 'SOLAZI', DTYPE_FLOAT, ['nobs'], [self.nobs] ] ]

                ]
            self.evn_spec = []
            self.rep_spec = [
                [ [ 'CHNM', 'CHNM', DTYPE_INTEGER, ['nobs', 'nchans'], [self.nobs, self.nchans] ],
                  [ 'TMBR', 'TMBR', DTYPE_FLOAT,   ['nobs', 'nchans'], [self.nobs, self.nchans] ],
                  [ 'CSTC', 'CSTC', DTYPE_FLOAT,   ['nobs', 'nchans'], [self.nobs, self.nchans] ] ]

                ]
            self.seq_spec = []
        elif (bf_type == BFILE_PREPBUFR):
            self.mtype_re = 'UnDef'
            self.int_spec = []
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super().init_dim_spec()

    ### methods ###
    

###########################################################################
# SUBROUTINES
###########################################################################

def SplitDate(yyyymmddhh):
    # This routine will take an integer date yyyymmddhh and return the
    # datetime equivalent.
    DateString = str(yyyymmddhh)
    Dtime = dt.datetime(int(DateString[0:4]), int(DateString[4:6]),
                        int(DateString[6:8]), int(DateString[8:10]))

    return Dtime

def MakeDate(Dtime):
    # This routine will take in integers representing yyyy, mm, dd, hh and
    # return an integer date yyyymmddhh.

    DateString = "%0.4i"%(Dtime.year) + "%0.2i"%(Dtime.month) + "%0.2i"%(Dtime.day) + "%0.2i"%(Dtime.hour)

    return int(DateString)

def FindRefDate(StartDate):
    # This routine will return the next analysis time (0, 6, 12, 18Z) following
    # the date represented in StartDate. StartDate is an integer value of the form
    # yyyymmddhh.
    #
    # Use datetime structures so that addition will take into account carry
    # over into the next day, month, year when hours are added.
    #
    # Compare the hours field in the start date with the analysis times
    # 0, 6, 12, 18, 24 where 24 is 0Z of the next day. 24 is there to make
    # sure that when subtracting the start date hour value, at least one
    # of the entries in the result will be greater than zero. Then the
    # distance to the next analysis time will be the first entry that is
    # greater than zero in the result of the subtraction (HourDiffs).
    StartDtime = SplitDate(StartDate)
    HourDiffs = np.array([ 0, 6, 12, 18, 24 ]) - StartDtime.hour
    HourInc = int(HourDiffs[HourDiffs > 0][0])

    # Form the datetime compatible version of HourInc, which can then
    # be added to the start date.
    DtDelta = dt.timedelta(hours=HourInc) 
    RefDtime = StartDtime + DtDelta
    RefDate = MakeDate(RefDtime)

    return RefDate

def BfilePreprocess(BufrFname, MessageRe, MaxNumMsg):
    # This routine will read the BUFR file and figure out how many observations
    # will be read when recording data.
    #
    # It will also figure out the timestamp to use for the obs reference time.
    # The next analysis time (0, 6, 12, 18Z) past the earliest message date
    # found in the selected messages will be used for the reference time.
    #
    # The msg_date value is an integer in the form of YYYYMMDDHH. Because of
    # this format, the smallest integer is also the earliest date. So we
    # just need to find the minimum date value and then figure out the
    # next analysis time from that value.

    bufr = ncepbufr.open(BufrFname)

    # The number of observations will be equal to the total number of subsets
    # contained in the selected messages.
    TotalNumMsg = 0
    NumMsg = 0 
    NumObs = 0 
    EarliestDate = 9999999999 # A value about 8000 years from now
    LatestDate = 0
    while ( (bufr.advance() == 0) ): 
        # Select only the messages that belong to this observation type
        if (re.search(MessageRe, bufr.msg_type)):
            TotalNumMsg += 1

            # If MaxNumMsg is less than 1, then select all messages.
            # If MaxNumMsg is >= 1, then select no more than MaxNumMsg
            if ((MaxNumMsg < 1) or (NumMsg < MaxNumMsg)):
                # Attribute "subsets" contains the number of subsets
                # for the current message.
                NumMsg += 1
                NumObs += bufr._subsets()

                if (bufr.msg_date < EarliestDate):
                    EarliestDate = bufr.msg_date

    bufr.close()

    return [NumObs, NumMsg, TotalNumMsg, FindRefDate(EarliestDate)] 

def BufrFloatToActual(Bval, Dtype):
    # This routine will extract the value of a variable from the
    # output of read_subset(). read_subset() will return a floating point
    # number (for any type) or an empty list if the mnemonic didn't exist. For strings
    # (Dtype = DTYPE_STRING) read the floating point number as characters. Otherwise
    # convert to integer or leave alone.
    #
    # Keep Dtype values in sync with entries in the DATA_TYPES dictionary. For now,
    # these values are DTYPE_STRING, DTYPE_INTEGER, DTYPE_FLOAT, DTYPE_DOUBLE.

    # If the incoming Bval is empty, then return an empty masked array
    # value so that writing process can skip this value if Bval was empty.
    if (Bval.size == 0):
        # Bval is empty so return an empty Dval.
        Dval = np.ma.array([ ])
    else:
        # Bval is not empty. Convert the Bval data to the appropriate type, and
        # return another masked array with the proper data and mask.

        if (Dtype == DTYPE_STRING):
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
            ByteList = list(struct.unpack('8c', Bval))

            # replace chars < 1 and > 127 with blank space
            for j in range(len(ByteList)):
                ByteVal = struct.unpack('@B', ByteList[j])[0]
                if ( (ByteVal < 1) or (ByteVal > 127)):
                    ByteList[j] = b' '

            TempStr = bytes.join(b'', ByteList).decode('ascii') 
            Dval = np.ma.array(TempStr, mask=Bval.mask, dtype='S8')
        elif (Dtype == DTYPE_INTEGER):
            # convert to integer
            Dval = np.ma.array(Bval.data.astype(np.int32), mask=Bval.mask, dtype=np.int32)
        elif (Dtype == DTYPE_FLOAT):
            # copy floats
            Dval = np.ma.array(Bval.data.astype(np.float32), mask=Bval.mask, dtype=np.float32)
        elif (Dtype == DTYPE_DOUBLE):
            # copy doubles
            Dval = np.ma.array(Bval.data.astype(np.float64), mask=Bval.mask, dtype=np.float64)

    # Squeeze the array since read_subset can return size 1 dimensions (eg. nlevs).
    return Dval.squeeze()

def WriteNcVar(Fid, ObsNum, Vname, Vdata):
    # This routine will write into a variable in the output netCDF file

    # For the string data, convert to a numpy character array
    if ((Vdata.dtype.char == 'S') or (Vdata.dtype.char == 'U')):
        StrSpec = "S{0:d}".format(MAX_STRING_LEN)
        Value = netCDF4.stringtochar(Vdata.astype(StrSpec))
    else:
        Value = Vdata.copy()

    # At this point, the dimension sizes of the netcdf variable (NcVar) and Value
    # need to get aligned. For some dimensions, the NcVar dimension size will tend
    # to be larger than the Value dimension size (eg, nlevs). For other dimensions,
    # it will be the other way around (eg, nevents). The one thing that can be counted
    # on is that the list of dimensions will match between NcVar and Value, except
    # that NcVar will have nobs as an extra dimension, and nobs will be the first
    # in the dimension list. For example, if you have a multi-level event:
    #
    #    Value dimensions will be [ nlevs, nevents ]
    #    Ncvar dimensions will be [ nobs, nlevs, nevents ]
    #
    # This means that to reconcile the sizes of each dimension, we need to slice
    # out of the minimum sizes of the corresponding dimension of NcVar and Value.
    # Using the example above, for a multi-level event:
    #
    #    Value dimensions are [ 51, 255 ]
    #    NcVar dimensions are [ 1000, 255, 20 ]
    #
    #    nlevs size for Value is 51, for NcVar is 255 so use 51 for the slicing
    #    nevents size for Value is 255, for NcVar is 20 so use 20 for the slicing
    #
    #    The assignment then becomes
    #        NcVar[ObsNum, 0:51, 0:20] = Value[0:51, 0:20]
    #
    # Figure out how to do the slicing by looking at the number of dimensions at both
    # Value (masked array) and NcVar (netcdf array). A masked array that gets built
    # using a scalar data value will return 0 for the number of dimensions.
    NcVar = Fid[Vname]
    ValNdim = Value.ndim
    NcNdim = NcVar.ndim

    if (NcNdim == 1):
        # No need for slicing. Value is either a scalar or a
        # 1D array with a single element
        NcVar[ObsNum] = Value
    elif (NcNdim == 2):
        # Value has one dimension  (eg, [nlevs])
        # NcVar has two dimensions (eg, [nobs,nlevs])
        N1 = min(Value.shape[0], NcVar.shape[1])
        NcVar[ObsNum, 0:N1] = Value[0:N1]
    elif (NcNdim == 3):
        # Value has two dimensions   (eg, [nlevs,nevents])
        # NcVar has three dimensions (eg, [nobs,nlevs,nevents])
        N1 = min(Value.shape[0], NcVar.shape[1])
        N2 = min(Value.shape[1], NcVar.shape[2])
        NcVar[ObsNum,0:N1, 0:N2] = Value[0:N1, 0:N2]
    elif (NcNdim == 4):
        # Value has three dimensions (eg, [nlevs,nstring,nevents])
        # NcVar has four dimensions  (eg, [nobs,nlevs,nstring,nevents])
        N1 = min(Value.shape[0], NcVar.shape[1])
        N2 = min(Value.shape[1], NcVar.shape[2])
        N3 = min(Value.shape[2], NcVar.shape[3])
        NcVar[ObsNum, 0:N1, 0:N2, 0:N3] = Value[0:N1, 0:N2, 0:N3]

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("obs_type", help="observation type")
ap.add_argument("input_bufr", help="path to input BUFR file")
ap.add_argument("output_netcdf", help="path to output netCDF4 file")
ap.add_argument("-m", "--maxmsgs", type=int, default=-1,
                help="maximum number of messages to keep", metavar="<max_num_msgs>")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output netcdf file")
ap.add_argument("-p", "--prepbufr", action="store_true",
                help="input BUFR file is in prepBUFR format")

MyArgs = ap.parse_args()

ObsType = MyArgs.obs_type
BufrFname = MyArgs.input_bufr
NetcdfFname = MyArgs.output_netcdf
MaxNumMsg = MyArgs.maxmsgs
ClobberOfile = MyArgs.clobber
if (MyArgs.prepbufr):
    BfileType = BFILE_PREPBUFR
else:
    BfileType = BFILE_BUFR

# Check files
BadArgs = False
if (not os.path.isfile(BufrFname)): 
    print("ERROR: {0:s}: Specified input BUFR file does not exist: {1:s}".format(ScriptName, BufrFname))
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

# Check the observation type, and create an observation instance.
if (ObsType == 'Aircraft'):
    Obs = AircraftObsType(BfileType)
elif (ObsType == 'Sondes'):
    Obs = SondesObsType(BfileType)
elif (ObsType == 'Amsua'):
    Obs = AmsuaObsType(BfileType)
else:
    print("ERROR: {0:s}: Unknown observation type: {1:s}".format(ScriptName, ObsType))
    print("")
    BadArgs = True

if (not BadArgs):
    if (Obs.mtype_re == 'UnDef'):
        if (BfileType == BFILE_BUFR):
            print("ERROR: {0:s}: Observation type {1:s} for BUFR format is undefined".format(ScriptName, ObsType))
        elif (BfileType == BFILE_PREPBUFR):
            print("ERROR: {0:s}: Observation type {1:s} for prepBUFR format is undefined".format(ScriptName, ObsType))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

# Arguments are okay, and we've got an observation object instantiated. Note that
# we need to have the obs object instantiated before calling BfilePreprocess()
# routine below. This is so BfilePreprocess() can select messages in the
# same manner as the subsequent conversion.
print("Converting BUFR to netCDF")
print("  Observation Type: {0:s}".format(ObsType))
if (BfileType == BFILE_BUFR):
    print("  Input BUFR file (BUFR format): {0:s}".format(BufrFname))
elif (BfileType == BFILE_PREPBUFR):
    print("  Input BUFR file (prepBUFR format): {0:s}".format(BufrFname))
print("  Output netCDF file: {0:s}".format(NetcdfFname))
if (MaxNumMsg > 0):
    print("  Limiting nubmer of messages to record to {0:d} messages".format(MaxNumMsg))
print("")

# It turns out that using multiple unlimited dimensions in the netCDF file
# can be very detrimental to the file's size, and can also be detrimental
# to the runtime for creating the file.
#
# In order to mitigate this, we want to use fixed size dimensions instead.
# Each obs type object will have its associated dimension sizes defined as
# fixed sizes. The only missing part is how many observations (subsets) will
# be selected.
#
# This number of subsets needs to be determined from reading through all the
# selected messages. Fortunately, this is very fast.
#
# Make a pass through the BUFR file to determine the number of observations
# and the reference time.
#
# BfilePreprocess() will use the regular expression for selecting message
# types. NumObs will be set to the number of observations selected,
# NumMsgs will be set to the number of messages selected, and TotalMsgs
# will be set to the total number of messages that match Obs.mtype_re in the file.
[NumObs, NumMsgs, TotalMsgs, RefDate] = BfilePreprocess(BufrFname, Obs.mtype_re, MaxNumMsg)

print("  Total number of messages that match obs type {0:s}: {1:d}".format(ObsType, TotalMsgs))
print("  Number of messages selected: {0:d}".format(NumMsgs))
print("  Number of observations selected: {0:d}".format(NumObs))
print("  Reference date for observations: {0:d}".format(RefDate))
print("")

# We will use the netCDF4 method date2num to calculate the datetime offset from the
# reference date (RefDate). This method wants a datetime object holding the current
# date and time as the first argument, and a units string holding the reference time.
# The units string is in the form "seconds since YYYY-MM-DD HH:00 UTC" where YYYY, MM,
# DD and HH come from the reference date. Create the units string and add that to the
# Obs object.
RefDtime = SplitDate(RefDate)
TimeUnits = "seconds since %0.4i-%0.2i-%0.2i %0.2i:00 UTC" %(RefDtime.year,
            RefDtime.month, RefDtime.day, RefDtime.hour)
Obs.set_time_units(TimeUnits)

# Now that we have the number of observations we will be recording, set the dimension
# size in the obs object. Note the set_nobs() method needs to be called before creating
# netcdf variables.
Obs.set_nobs(NumObs)

# Create the dimensions and variables in the netCDF file in preparation for
# recording the selected observations.
# Aslo, store the reference date in the netcdf file as an attribute
nc = Dataset(NetcdfFname, 'w', format='NETCDF4')
nc.ref_date = RefDate
Obs.create_nc_datasets(nc)

# Fill in the dimension variables with the coordinate values. Just using dummy values
# for now which are 1..n where n is the size of the coorespoding dimension.
Obs.fill_coords(nc)

# Open the BUFR file and initialize the file object.
bufr = ncepbufr.open(BufrFname)

# Run the conversion
Obs.convert(bufr, nc)

# Clean up
bufr.close()

nc.sync()
nc.close()
