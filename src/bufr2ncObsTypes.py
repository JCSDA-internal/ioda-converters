#!/usr/bin/env python

from __future__ import print_function
import numpy as np
import sys
import re
import netCDF4
from netCDF4 import Dataset
import struct
import datetime as dt

import bufr2ncCommon as cm

############################################################################
# SUBROUTINES
############################################################################

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

        if (Dtype == cm.DTYPE_STRING):
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
        elif (Dtype == cm.DTYPE_INTEGER):
            # convert to integer
            Dval = np.ma.array(Bval.data.astype(np.int32), mask=Bval.mask, dtype=np.int32)
        elif (Dtype == cm.DTYPE_FLOAT):
            # copy floats
            Dval = np.ma.array(Bval.data.astype(np.float32), mask=Bval.mask, dtype=np.float32)
        elif (Dtype == cm.DTYPE_DOUBLE):
            # copy doubles
            Dval = np.ma.array(Bval.data.astype(np.float64), mask=Bval.mask, dtype=np.float64)

    # Squeeze the array since read_subset can return size 1 dimensions (eg. nlevs).
    return Dval.squeeze()

def WriteNcVar(Fid, ObsNum, Vname, Vdata):
    # This routine will write into a variable in the output netCDF file

    # For the string data, convert to a numpy character array
    if ((Vdata.dtype.char == 'S') or (Vdata.dtype.char == 'U')):
        StrSpec = "S{0:d}".format(cm.MAX_STRING_LEN)
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
        self.bufr_ftype = cm.BFILE_UNDEF
        self.mtype_re = 'UnDef'
 
        # Keep this list of dimensions in sync with the if statment structure
        # in the init_dim_spec() method.
        self.nobs = -1
        self.nlevs = cm.MAX_LEVELS
        self.nevents = cm.MAX_EVENTS
        self.nstring = cm.MAX_STRING_LEN
        self.nchans = -1

        self.time_units = ""
        self.int_spec = []
        self.evn_spec = []
        self.rep_spec = []
        self.seq_spec = []
        self.dim_spec = []
        self.misc_spec = [
            [ [ 'msg_type', '', cm.DTYPE_STRING, ['nobs', 'nstring'], [self.nobs, self.nstring] ],
              [ 'msg_date', '', cm.DTYPE_UINT,   ['nobs'],            [self.nobs]               ] ]
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
    # This method is a default routine for counting the number of observations
    # in the current BUFR message. The default number of observations is simply
    # the number of subsets in this message. The reason that this is a method in
    # the base class is so that derived classes (GpsroObsType, for example) can
    # override this method with a more complex algorithm.
    def msg_obs_count(self, bufr):
        return bufr._subsets()

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
        # Keep the following list of dimensions in sync with the __init__ method in
        # in the ObsType base class.
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

            DimList.append([ dname, dname, cm.DTYPE_UINT, [ dname ], [ dsize ] ])

        self.dim_spec = [ DimList ]

    ###############################################################################
    # This method will create dimensions and variables in the netcdf file
    # according to the obs type variable specs.
    def create_nc_datasets(self, nc):

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
                    if (Dtype == cm.DTYPE_STRING):
                        Vtype = 'S1'
                    elif (Dtype == cm.DTYPE_INTEGER):
                        Vtype = 'i4'
                    elif (Dtype == cm.DTYPE_UINT):
                        Vtype = 'u4'
                    elif (Dtype == cm.DTYPE_FLOAT):
                        Vtype = 'f4'
                    elif (Dtype == cm.DTYPE_DOUBLE):
                        Vtype = 'f8'

                    # Don't specify the chunk sizes. Since all of the dimensions
                    # are of fixed size, the built-in algorithm for calculating
                    # chunk sizes will do a good job.
                    nc.createVariable(Vname, Vtype, DimNames, zlib=True,
                                      shuffle=True, complevel=6)

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
    #
    # This method provides a defalut method that can be overridden by an obs type
    # requiring a more complex algorithm (Gpsro, eg.). ActualValues is a list
    # of dictionaries, and the defalut action is to create one item in that list.
    # This single dictionary will be filled in by simply walking through the
    # variables in the lists contained in int_spec, evn_spec, rep_spec and seq_spec,
    # reading the mnemonics out of the BUFR file, and loading in the results into
    # the single dictionary.
    def extract_bufr(self, bufr):
        # Initialize ActualValues to a list with one entry which is an empty dictionary.
        ActualValues = []
        ActualValues.append({})
    
        # Read and convert the individual data mnemonics. The mnemonic value is the second
        # entry in the int_spec sublist elements.
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.int_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists) 
        ActualValues[0] = self.bufr_float_to_actual(self.int_spec, BufrValues, ActualValues[0])

        # Read and convert the event mnemonics
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.evn_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists, Eflag=True) 
        ActualValues[0] = self.bufr_float_to_actual(self.evn_spec, BufrValues, ActualValues[0])

        # Read and convert the replication mnemonics
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.rep_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists, Rflag=True) 
        ActualValues[0] = self.bufr_float_to_actual(self.rep_spec, BufrValues, ActualValues[0])

        # Read and convert the sequence mnemonics
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.seq_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists, Sflag=True) 
        ActualValues[0] = self.bufr_float_to_actual(self.seq_spec, BufrValues, ActualValues[0])

        return ActualValues

    ###############################################################################
    # This method will calculate the offset time value from the BUFR mnemonic
    # values. The calculation depends on the type of BUFR file (raw BUFR or prepBUFR).
    # For raw BUFR, the absolute observation time comes from the mnemonics:
    #     YEAR  - year
    #     MNTH  - month
    #     DAYS  - day
    #     HOUR  - hour
    #     MINU  - minute
    #     SECO  - second
    #
    # For prepBUFR, the time relative to msg_date is held in DHR, and for multi-level
    # obs in HRDR.
    #     msg_date - Message date/time
    #     DHR      - Observation time minus cycle time
    #     HRDR     - Observation time minus cycle time on a level by level basis
    #                   (taking drift into account)
    #
    # The netCDF4 utility date2num is used to calculate the offset. This routine takes
    # in an absolute time stored in a datetime structure and the reference time stored
    # in a units string (ie: seconds from YYYY-MM-DD HH:MM). First calculate the
    # absolute time from the variable values, and then use date2num to get the time
    # offset.
    def calc_obs_time(self, ActualValues):
        if (self.bufr_ftype == cm.BFILE_PREPBUFR):
            # prepBUFR: use msg_date and DHR or HRDR

            # Calculate the offset given by msg_date alone.
            MsgDate = ActualValues['msg_date'].data[0]
            MsgDtime = cm.SplitDate(MsgDate)
            MdateOffset = netCDF4.date2num(MsgDtime, units=self.time_units)

            # Get the offset from either DHR or HRDR. 
            if (self.multi_level):
                OdateOffset = ActualValues['HRDR'].data
            else:
                OdateOffset = ActualValues['DHR'].data
                
            # Add in the MsgDate and ObsDate offsets together.
            Toffset = np.ma.array(OdateOffset + MdateOffset)
        else:
            # raw BUFR: use YEAR, MNTH, ...
            Year   = int(ActualValues['YEAR'].data)
            Month  = int(ActualValues['MNTH'].data)
            Day    = int(ActualValues['DAYS'].data)
            Hour   = int(ActualValues['HOUR'].data)
            Minute = int(ActualValues['MINU'].data)
            Second = int(ActualValues['SECO'].data)

            # Create datetime object with above data. Sometimes the SECO value is
            # outside the range 0..59 (which is what datetime requires). Use Year through
            # Minute to create the datetime object and add in Second via a
            # timedelta object.
            ObsDate = dt.datetime(Year, Month, Day, Hour, Minute) + dt.timedelta(seconds=Second)

            # Convert to offset relative to the time units.
            Toffset = np.ma.array(netCDF4.date2num(ObsDate, units=self.time_units))

        return Toffset

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
                    # Grab all of the mnemonics from the bufr file, and convert
                    # from the BUFR float representation to the actual data type
                    # (integer, float, string, double). ActualValues is a list of
                    # dictionaries where each dictionary represents one observation.
                    # A dictionary within the list is keyed by the netcdf variable
                    # name and contains the associated data value.
                    ActualValues = self.extract_bufr(bufr)

                    for i in range(len(ActualValues)):
                        # Put the message type and message date into the dictionary.
                        ActualValues[i]['msg_type'] = MsgType
                        ActualValues[i]['msg_date'] = MsgDate

                        # Calculate the value for the Time variable (which is an offset
                        # from the reference time). Add the Time value to the dictionary.
                        ActualValues[i]['Time'] = self.calc_obs_time(ActualValues[i])

                        # Write out the netcdf variables.
                        for Vname, Vdata in ActualValues[i].items():
                            # Skip the write if Vdata is empty
                            if (Vdata.size > 0):
                                WriteNcVar(nc, ObsNum, Vname, Vdata)

                        # Increment observation number and print out progress messages.
                        ObsNum += 1
                        if ((ObsNum % 100) == 0):
                            print("  Converted {0:d} observations".format(ObsNum))

        # If processing a prepBUFR file, record the virtual temperature
        # program code
        if (self.bufr_ftype == cm.BFILE_PREPBUFR):
            nc.virtmp_code = bufr.get_program_code('VIRTMP')

        print("")
        print("  Total converted observations: ", ObsNum)
        print("")

################################# Aircraft Observation Type ############################
class AircraftObsType(ObsType):
    ### initialize data elements ###
    def __init__(self, bf_type):
        super(AircraftObsType, self).__init__()

        self.bufr_ftype = bf_type
        self.multi_level = False
        # Add on the specs for the Time variable
        self.misc_spec[0].append([ 'Time', '', cm.DTYPE_FLOAT, ['nobs'], [self.nobs] ])
        if (bf_type == cm.BFILE_BUFR):
            self.mtype_re = '^NC004001'
            self.int_spec = [
                [ [ 'YEAR',   'YEAR',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MNTH',   'MNTH',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'DAYS',   'DAYS',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'HOUR',   'HOUR',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MINU',   'MINU',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ACID',   'ACID',   cm.DTYPE_DOUBLE,  ['nobs'], [self.nobs] ],
                  [ 'CORN',   'CORN',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'CLAT',   'CLAT',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'CLON',   'CLON',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'FLVL',   'FLVL',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],

                [ [ 'TMDB',   'TMDB',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TMDP',   'TMDP',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'REHU',   'REHU',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'WSPD',   'WSPD',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'WDIR',   'WDIR',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'QMAT',   'QMAT',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'QMDD',   'QMDD',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'QMWN',   'QMWN',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                [ [ 'SEQNUM', 'SEQNUM', cm.DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'BUHD',   'BUHD',   cm.DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'BORG',   'BORG',   cm.DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'BULTIM', 'BULTIM', cm.DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'BBB',    'BBB',    cm.DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ],
                  [ 'RPID',   'RPID',   cm.DTYPE_STRING,  ['nobs', 'nstring'], [self.nobs, self.nstring] ] ],
                ]
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []
        elif (bf_type == cm.BFILE_PREPBUFR):
            self.mtype_re = 'AIRC[AF][RT]'
            self.int_spec = [
                [ [ 'SID',  'SID',  cm.DTYPE_DOUBLE,  ['nobs'], [self.nobs] ],
                  [ 'ACID', 'ACID', cm.DTYPE_DOUBLE,  ['nobs'], [self.nobs] ],
                  [ 'XOB',  'XOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'YOB',  'YOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'DHR',  'DHR',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TYP',  'TYP',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ELV',  'ELV',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'SAID', 'SAID', cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'T29',  'T29',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                [ [ 'POB',  'POB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'QOB',  'QOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TOB',  'TOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'ZOB',  'ZOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'UOB',  'UOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'VOB',  'VOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PWO',  'PWO',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'MXGS', 'MXGS', cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PRSS', 'PRSS', cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TDO',  'TDO',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PMO',  'PMO',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],

                [ [ 'PQM',  'PQM',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'QQM',  'QQM',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'TQM',  'TQM',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ZQM',  'ZQM',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'WQM',  'WQM',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'PWQ',  'PWQ',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'PMQ',  'PMQ',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                [ [ 'POE',  'POE',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'QOE',  'QOE',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TOE',  'TOE',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'WOE',  'WOE',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PWE',  'PWE',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],

                [ [ 'HOVI', 'HOVI', cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'CAT',  'CAT',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'XDR',  'XDR',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'YDR',  'YDR',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'HRDR', 'HRDR', cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'POAF', 'POAF', cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'IALR', 'IALR', cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],
                ]
            self.evn_spec = [
                [ [ 'TPC_bevn', 'TPC', cm.DTYPE_INTEGER, ['nobs', 'nevents'], [self.nobs, self.nevents] ],
                  [ 'TOB_bevn', 'TOB', cm.DTYPE_FLOAT,   ['nobs', 'nevents'], [self.nobs, self.nevents] ],
                  [ 'TQM_bevn', 'TQM', cm.DTYPE_INTEGER, ['nobs', 'nevents'], [self.nobs, self.nevents] ] ],
                ]
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super(AircraftObsType, self).init_dim_spec()

    ### methods ###
    

################################# Radiosonde Observation Type ############################
class SondesObsType(ObsType):
    ### initialize data elements ###
    def __init__(self, bf_type):
        super(SondesObsType, self).__init__()

        self.bufr_ftype = bf_type
        self.multi_level = True
        # Add on the specs for the Time variable
        self.misc_spec[0].append([ 'Time', '', cm.DTYPE_FLOAT, ['nobs','nlevs'], [self.nobs,self.nlevs] ])
        if (bf_type == cm.BFILE_BUFR):
            self.mtype_re = 'UnDef'
            self.int_spec = []
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []
        elif (bf_type == cm.BFILE_PREPBUFR):
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
                [ [ 'SID',  'SID',  cm.DTYPE_DOUBLE,  ['nobs'], [self.nobs] ],
                  [ 'XOB',  'XOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'YOB',  'YOB',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'DHR',  'DHR',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'TYP',  'TYP',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ELV',  'ELV',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'T29',  'T29',  cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                [ [ 'POB',  'POB',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'QOB',  'QOB',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'TOB',  'TOB',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'ZOB',  'ZOB',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'UOB',  'UOB',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'VOB',  'VOB',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'PWO',  'PWO',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'TDO',  'TDO',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ] ],

                [ [ 'PQM',  'PQM',  cm.DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'QQM',  'QQM',  cm.DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'TQM',  'TQM',  cm.DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'ZQM',  'ZQM',  cm.DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'WQM',  'WQM',  cm.DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'PWQ',  'PWQ',  cm.DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'PMQ',  'PMQ',  cm.DTYPE_INTEGER, ['nobs', 'nlevs'], [self.nobs, self.nlevs] ] ],

                [ [ 'POE',  'POE',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'QOE',  'QOE',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'TOE',  'TOE',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'WOE',  'WOE',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'PWE',  'PWE',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'XDR',  'XDR',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'YDR',  'YDR',  cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ],
                  [ 'HRDR', 'HRDR', cm.DTYPE_FLOAT,   ['nobs', 'nlevs'], [self.nobs, self.nlevs] ] ],

                ]
            self.evn_spec = [
                [ [ 'TPC_bevn', 'TPC', cm.DTYPE_INTEGER, ['nobs', 'nlevs', 'nevents'], [self.nobs, self.nlevs, self.nevents] ],
                  [ 'TOB_bevn', 'TOB', cm.DTYPE_FLOAT,   ['nobs', 'nlevs', 'nevents'], [self.nobs, self.nlevs, self.nevents] ],
                  [ 'TQM_bevn', 'TQM', cm.DTYPE_INTEGER, ['nobs', 'nlevs', 'nevents'], [self.nobs, self.nlevs, self.nevents] ] ]
                ]
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super(SondesObsType, self).init_dim_spec()

    ### methods ###
    

########################### Radiance (AMSU-A) Observation Type ############################
class AmsuaObsType(ObsType):
    ### initialize data elements ###
    def __init__(self, bf_type):
        super(AmsuaObsType, self).__init__()

        self.nchans = 20  # This is unique to AMSU
        self.bufr_ftype = bf_type
        self.multi_level = False
        self.misc_spec[0].append([ 'Time', '', cm.DTYPE_FLOAT, ['nobs'], [self.nobs] ])
        if (bf_type == cm.BFILE_BUFR):

            self.mtype_re = '^NC021023'
            self.int_spec = [
                [ [ 'SAID',   'SAID',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'FOVN',   'FOVN',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'YEAR',   'YEAR',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MNTH',   'MNTH',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'DAYS',   'DAYS',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'HOUR',   'HOUR',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MINU',   'MINU',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'SECO',   'SECO',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'CLAT',   'CLAT',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'CLON',   'CLON',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'HOLS',   'HOLS',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ] ],

                [ [ 'SAZA',   'SAZA',   cm.DTYPE_FLOAT, ['nobs'], [self.nobs] ],
                  [ 'SOZA',   'SOZA',   cm.DTYPE_FLOAT, ['nobs'], [self.nobs] ],
                  [ 'BEARAZ', 'BEARAZ', cm.DTYPE_FLOAT, ['nobs'], [self.nobs] ],
                  [ 'SOLAZI', 'SOLAZI', cm.DTYPE_FLOAT, ['nobs'], [self.nobs] ] ]

                ]
            self.evn_spec = []
            self.rep_spec = [
                [ [ 'CHNM', 'CHNM', cm.DTYPE_INTEGER, ['nobs', 'nchans'], [self.nobs, self.nchans] ],
                  [ 'TMBR', 'TMBR', cm.DTYPE_FLOAT,   ['nobs', 'nchans'], [self.nobs, self.nchans] ],
                  [ 'CSTC', 'CSTC', cm.DTYPE_FLOAT,   ['nobs', 'nchans'], [self.nobs, self.nchans] ] ]

                ]
            self.seq_spec = []
        elif (bf_type == cm.BFILE_PREPBUFR):
            self.mtype_re = 'UnDef'
            self.int_spec = []
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super(AmsuaObsType, self).init_dim_spec()

    ### methods ###
    
########################### GPSRO Observation Type ############################
class GpsroObsType(ObsType):
    ### initialize data elements ###
    def __init__(self, bf_type):
        super(GpsroObsType, self).__init__()

        self.bufr_ftype = bf_type
        self.multi_level = False
        self.misc_dtype = cm.DTYPE_FLOAT
        self.misc_spec[0].append([ 'Time', '', cm.DTYPE_FLOAT, ['nobs'], [self.nobs] ])
        if (bf_type == cm.BFILE_BUFR):

            self.mtype_re = '^NC003010'
            self.int_spec = [
                [ [ 'SAID',   'SAID',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'YEAR',   'YEAR',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MNTH',   'MNTH',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'DAYS',   'DAYS',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'HOUR',   'HOUR',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'MINU',   'MINU',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'SECO',   'SECO',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'ELRC',   'ELRC',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PCCF',   'PCCF',   cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'PTID',   'PTID',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ],
                  [ 'GEODU',  'GEODU',  cm.DTYPE_FLOAT,   ['nobs'], [self.nobs] ],
                  [ 'QFRO',   'QFRO',   cm.DTYPE_INTEGER, ['nobs'], [self.nobs] ] ],

                ]
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []

            # These are the observation variables that will be extracted by the
            # convert method of this class (which overrides the base class convert
            # method).
            self.misc_spec[0].append([ 'CLATH',     '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'CLONH',     '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'HEIT',      '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'ARFR',      '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'ARFR_err',  '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'ARFR_pccf', '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'BNDA',      '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'BNDA_err',  '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'BNDA_pccf', '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'MEFR',      '', self.misc_dtype, ['nobs'], [self.nobs] ])
            self.misc_spec[0].append([ 'IMPP',      '', self.misc_dtype, ['nobs'], [self.nobs] ])

        elif (bf_type == cm.BFILE_PREPBUFR):
            self.mtype_re = 'UnDef'
            self.int_spec = []
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super(GpsroObsType, self).init_dim_spec()

    ### methods ###

    ########################################################################
    # This method will extract counts from the BUFR file which are useful
    # for determining how to walk through the bending angle and refractivity
    # observation data within the BUFR file.
    def extract_gpsro_obs_counts(self, bufr):
        # ROSEQ1 contains the bending angle observations
        # ROSEQ3 contains the refractivity observations
        #
        # Within each bending angle observation is another sequence, ROSEQ2,
        # which contains bending angle obs for a list of frequencies. Because
        # ROSEQ1 and ROSEQ2 are related, the length of the array returned 
        # from the mnemonic '{ROSEQ2}' will be equal to the value returned
        # from the mnemonic '(ROSEQ1)'. Therefore, the number of bending angle
        # observations can be determined from the length of the ROSEQ2 array.
        #
        # Mnemonic '{ROSEQ2}' returns the number of frequencies within each bend angle obs
        #                     this value is an array since the number of frequencies can
        #                     change on each bend angle obs
        # Mnemonic '(ROSEQ3)' returns the number of refractivity obs
        #
        NumBangleFreq = bufr.read_subset('{ROSEQ2}').astype(int).squeeze()
        NumRefrac     = bufr.read_subset('(ROSEQ3)').astype(int).squeeze()

        NumBangle = len(NumBangleFreq)

        return [ NumBangle, NumRefrac, NumBangleFreq ]

    ########################################################################
    # This method will count up the number of observations contained in a
    # single message of a GPSRO obs type.
    def msg_obs_count(self, bufr):
        ObsCount = 0

        # Visit all subsets and count up the number of obs contained in each
        # subset.
        SubsetCount = 1
        while (bufr.load_subset() == 0):
            [ NumBangle, NumRefrac, NumBangleFreq ] = self.extract_gpsro_obs_counts(bufr)

            if (NumBangle != NumRefrac):
                print("WARNING: Gpsro: Skip report due to mismatch in number of bending angle and refractivity observations")
                print("WARNING: Gpsro:   Message number: {0:d}, Subset number: {1:d}".format(bufr.msg_counter, SubsetCount))
                print("")
                continue
            else:
                # Have same number of bending angle and refractivity observations
                # Even though there can be multiple frequencies within a single
                # bending angle obs, we are only going to take the zero frequency
                # entry. That is, only one frequency per bending angle obs. Therefore,
                # the number of obs this subset is equal to the number of bend
                # angle obs.
                ObsCount += NumBangle

            SubsetCount += 1

        return int(ObsCount)

    ###############################################################################
    # This method will extract data from a subset of a Gpsro raw BUFR file and
    # load up the observations into the ActualValues data structure. ActualValues
    # is a list of dictionaries where each dictionary holds one observation.
    def extract_bufr(self, bufr):
        ActualValues = []
    
        # Read and convert the header data (list in int_spec)
        Mlists = [ [ Mlist[1] for Mlist in SubList] for SubList in self.int_spec ]
        BufrValues = self.read_bufr_data(bufr, Mlists) 
        HeaderVals = self.bufr_float_to_actual(self.int_spec, BufrValues, {})

        [ NumBangle, NumRefrac, NumBangleFreq ] = self.extract_gpsro_obs_counts(bufr)

        # Only record observations if the numbers of bend angle and refractivity obs match
        # The warnings about skipping mismatched number of obs have already been written
        # by the msg_obs_count() method.
        if (NumBangle == NumRefrac):
            # Grab the sequence data for each of bend angle and refractivity
            BangleBvals = bufr.read_subset('ROSEQ1', seq=True)
            RfracBvals = bufr.read_subset('ROSEQ3', seq=True)

            # Each replication of bend angle (and refrac) obs is a single observation.
            for irep in range(NumBangle):
                # Record the header values. It is important to use the copy() method
                # of the HeaderVals dictionary. The copy() method will create a new
                # dictionary with references to the elements in HeaderVals. This is
                # exactly what we want: A list of separate dictionaries where the
                # header elements are all references to the elements in HeaderVals, and
                # the other elements that get entered into each dictionary are
                # separated so they can be set to anything (ie, hold unique values).
                #
                # Note that "ActualValues.append(HeaderVals)" results in each entry
                # in the ActualVlaues list reference the same dictionary which is the
                # one created when HeaderVals was created. In this case, subsequent
                # loops keep overwriting the previous loop's values and you end up
                # with NumBangle references to the single dictionary. Ie, the entire
                # set of NumBangle observations all have the same values for all
                # keys.
                ActualValues.append(HeaderVals.copy())

                # The frequency related data from the inner sequence of ROSEQ1 got
                # expanded and recorded in the result array (BangleBvals) by adding
                # extra entries along the first dimension. The sequence ROSEQ1 looks
                # like:
                #
                #   ROSEQ1: CLATH CLONH BEARAZ {ROSEQ2} PCCF
                #   ROSEQ2: MEFR IMPP BNDA FOST BNDA_err FOST_err
                #
                # ROSEQ2 can repeat n times, then the expansion ROSEQ1 looks like:
                #
                #   CLATH CLONH BEARAZ ROSEQ2-1 PCCF                    (for n = 1)
                #   CLATH CLONH BEARAZ ROSEQ2-1 ROSEQ2-2 ROSEQ-3 PCCF   (for n = 3)
                #
                #         where ROSEQ2-n just means the nth replication of ROSEQ2
                #

                # Grab the BUFR values for the obs data
                # Latitude
                CLATH = np.ma.array(BangleBvals[0,irep], mask=BangleBvals.mask[0,irep])

                # Longitude
                CLONH = np.ma.array(BangleBvals[1,irep], mask=BangleBvals.mask[1,irep])

                # Height
                HEIT = np.ma.array(RfracBvals[0,irep], mask=RfracBvals.mask[0,irep])

                # Refractivity data
                ARFR = np.ma.array(RfracBvals[1,irep], mask=RfracBvals.mask[0,irep])
                ARFR_err = np.ma.array(RfracBvals[3,irep], mask=RfracBvals.mask[0,irep])
                ARFR_pccf = np.ma.array(RfracBvals[5,irep], mask=RfracBvals.mask[0,irep])

                # Bending Angle data
                # Locate the zero frequency sequence. Use missing data if the zero
                # frequency data is not available in the BUFR file.
                MEFR     = np.ma.array([0.0], mask=[True])
                IMPP     = np.ma.array([0.0], mask=[True])
                BNDA     = np.ma.array([0.0], mask=[True])
                BNDA_err = np.ma.array([0.0], mask=[True])
                for i in range(NumBangleFreq[irep]):
                    m = 6*(i+1)-3
                    if ((int(BangleBvals[m,irep]) == 0) and (not BangleBvals.mask[m,irep])):
                        # This replication has zero frequency which is not masked
                        # (that is, not marked as missing).
                        MEFR     = np.ma.array(BangleBvals[m,irep],
                                     mask=BangleBvals.mask[m,irep])    # mean frequency
                        IMPP     = np.ma.array(BangleBvals[m+1,irep],
                                     mask=BangleBvals.mask[m+1,irep])  # impact parameter
                        BNDA     = np.ma.array(BangleBvals[m+2,irep],
                                     mask=BangleBvals.mask[m+2,irep])  # bending angle
                        BNDA_err = np.ma.array(BangleBvals[m+4,irep],
                                     mask=BangleBvals.mask[m+4,irep])  # bending angle error
                        break

                # BNDA_pccf is at the end of the ROSEQ1 section, i.e. one after the
                # NumBangleFreq[irep] replications of the current ROSEQ2 section.
                m = 6*NumBangleFreq[irep] + 3
                BNDA_pccf = np.ma.array(BangleBvals[m,irep],
                              mask=BangleBvals.mask[m,irep])   # bending angle pccf

                # Convert and fill in the ActualValues dictionary
                ActualValues[irep]['CLATH'] = BufrFloatToActual(CLATH, self.misc_dtype)
                ActualValues[irep]['CLONH'] = BufrFloatToActual(CLONH, self.misc_dtype)
                ActualValues[irep]['HEIT']  = BufrFloatToActual(HEIT, self.misc_dtype)

                ActualValues[irep]['ARFR']      = BufrFloatToActual(ARFR, self.misc_dtype)
                ActualValues[irep]['ARFR_err']  = BufrFloatToActual(ARFR_err, self.misc_dtype)
                ActualValues[irep]['ARFR_pccf'] = BufrFloatToActual(ARFR_pccf, self.misc_dtype)

                ActualValues[irep]['MEFR']      = BufrFloatToActual(MEFR, self.misc_dtype)
                ActualValues[irep]['IMPP']      = BufrFloatToActual(IMPP, self.misc_dtype)
                ActualValues[irep]['BNDA']      = BufrFloatToActual(BNDA, self.misc_dtype)
                ActualValues[irep]['BNDA_err']  = BufrFloatToActual(BNDA_err, self.misc_dtype)
                ActualValues[irep]['BNDA_pccf'] = BufrFloatToActual(BNDA_pccf, self.misc_dtype)

        return ActualValues

