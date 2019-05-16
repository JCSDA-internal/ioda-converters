#!/usr/bin/env python

from __future__ import print_function
import numpy as np
import sys
import re
import netCDF4
import struct
import datetime as dt

import bufr2ncCommon as cm

############################################################################
# SUBROUTINES
############################################################################


def SplitMsgDate(yyyymmddhh):
    # This routine will take an integer date yyyymmddhh and return the
    # datetime equivalent.
    DateString = str(yyyymmddhh)
    Dtime = dt.datetime(int(DateString[0:4]), int(DateString[4:6]),
                        int(DateString[6:8]), int(DateString[8:10]))

    return Dtime


def MakeDate(Dtime):
    # This routine will take in a datetime object and return an integer date yyyymmdd.
    DateString = "%0.4i" % (Dtime.year) + \
        "%0.2i" % (Dtime.month) + "%0.2i" % (Dtime.day)

    return int(DateString)


def MakeTime(Dtime):
    # This routine will take in a datetime object and return an integer time hhmmss.
    TimeString = "%0.2i" % (Dtime.hour) + \
        "%0.2i" % (Dtime.minute) + "%0.2i" % (Dtime.second)

    return int(TimeString)


def MakeEpochTime(Dtime):
    # This routine will take in a datetime object and return time since the
    # Epoch (Jan 1, 1970).
    EpochDtime = dt.datetime(1970, 1, 1)
    EpochTime = (Dtime - EpochDtime).total_seconds()

    return EpochTime


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
        Dval = np.ma.array([])
    else:
        # Bval is not empty. Convert the Bval data to the appropriate type, and
        # return another masked array with the proper data and mask.

        if (Dtype == cm.DTYPE_STRING):
            # TODO This is not generic enough to accomodate all the strings.
            #
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
                if ((ByteVal < 1) or (ByteVal > 127)):
                    ByteList[j] = b' '

            TempStr = bytes.join(b'', ByteList).decode('ascii')
            Dval = np.ma.array(TempStr, mask=Bval.mask, dtype='S8')
        elif (Dtype == cm.DTYPE_INTEGER):
            # convert to integer
            Dval = np.ma.array(Bval.data.astype(np.int32),
                               mask=Bval.mask, dtype=np.int32)
        elif (Dtype == cm.DTYPE_FLOAT):
            # copy floats
            Dval = np.ma.array(Bval.data.astype(np.float32),
                               mask=Bval.mask, dtype=np.float32)
        elif (Dtype == cm.DTYPE_DOUBLE):
            # copy doubles
            Dval = np.ma.array(Bval.data.astype(np.float64),
                               mask=Bval.mask, dtype=np.float64)

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
    # that NcVar will have nlevs as an extra dimension, and nlocs will be the first
    # in the dimension list. For example, if you have a multi-level event:
    #
    #    Value dimensions will be [ nlevs, nevents ]
    #    Ncvar dimensions will be [ nlocs, nlevs, nevents ]
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
    #
    # It is possible to get single level values from a multiple level
    # obs type (such as sondes). In this case the levels dimension will
    # be squeezed away from Value. Handle this case by inserting Value
    # into the first element of the nlevs dimension. Note that there is
    # an assumption that nlevs is the first dimension of Value and
    # the second dimension of NcVar.

    NcVar = Fid[Vname]
    ValNdim = Value.ndim
    NcNdim = NcVar.ndim

    if (NcNdim == 1):
        # No need for slicing. Value is either a scalar or a
        # 1D array with a single element
        NcVar[ObsNum] = Value
    elif (NcNdim == 2):
        if (ValNdim == 0):
            # Value is a scalar (representing a single level value)
            # NcVar has two dimensions (eg, [nlocs,nlevs])
            NcVar[ObsNum, 0] = Value
        else:
            # Value has one dimension  (eg, [nlevs])
            # NcVar has two dimensions (eg, [nlocs,nlevs])
            N1 = min(Value.shape[0], NcVar.shape[1])
            NcVar[ObsNum, 0:N1] = Value[0:N1]
    elif (NcNdim == 3):
        if (ValNdim == 1):
            # Value has one dimension and is single level   (eg, [nevents])
            # NcVar has three dimensions (eg, [nlocs,nlevs,nevents])
            N2 = min(Value.shape[0], NcVar.shape[2])
            NcVar[ObsNum, 0, 0:N2] = Value[0:N2]
        else:
            # Value has two dimensions   (eg, [nlevs,nevents])
            # NcVar has three dimensions (eg, [nlocs,nlevs,nevents])
            N1 = min(Value.shape[0], NcVar.shape[1])
            N2 = min(Value.shape[1], NcVar.shape[2])
            NcVar[ObsNum, 0:N1, 0:N2] = Value[0:N1, 0:N2]
    elif (NcNdim == 4):
        if (ValNdim == 2):
            # Value has two dimensions and is single level (eg, [nstring,nevents])
            # NcVar has four dimensions  (eg, [nlocs,nlevs,nstring,nevents])
            N2 = min(Value.shape[0], NcVar.shape[2])
            N3 = min(Value.shape[1], NcVar.shape[3])
            NcVar[ObsNum, 0, 0:N2, 0:N3] = Value[0:N2, 0:N3]
        else:
            # Value has three dimensions (eg, [nlevs,nstring,nevents])
            # NcVar has four dimensions  (eg, [nlocs,nlevs,nstring,nevents])
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
# override the convert() method or its sub-methods.
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

# ############################### Base Observation Type ############################


class ObsType(object):
    # # initialize data elements ###
    def __init__(self):
        self.bufr_ftype = cm.BFILE_UNDEF

        # Variables for message selector
        self.mtype_re = 'UnDef'
        self.max_num_msg = 0
        self.thin_interval = 1
        self.num_msg_selected = 0
        self.num_msg_mtype = 0

        # Keep this list of dimensions in sync with the if statment structure
        # in the init_dim_spec() method.
        self.nlocs = -1
        self.nlevs = cm.MAX_LEVELS
        self.nevents = cm.MAX_EVENTS
        self.nstring = cm.MAX_STRING_LEN
        self.nchans = -1
        self.nrecs = 1
        self.nvars = -1

        self.int_spec = []
        self.evn_spec = []
        self.rep_spec = []
        self.seq_spec = []
        self.dim_spec = []
        self.misc_spec = [
            [['msg_type@MetaData', '', cm.DTYPE_STRING, ['nlocs', 'nstring'], [self.nlocs, self.nstring]],
             ['msg_date@MetaData', '', cm.DTYPE_UINT, ['nlocs'], [self.nlocs]]]
        ]

    # # methods ###

    ###############################################################################
    # This method will set the number of observations. This must be called
    # before attempting to create any netcdf variables since self.nlocs
    # is also used to define the dimension sizes in all of the netcdf variables.
    def set_nlocs(self, nlocs):
        # update the data memeber
        self.nlocs = nlocs

        # update the dimension sizes in the specs
        #
        # each spec is a list of variable specs
        # each variable spec is a list with the fourth item being a list of
        #    dimension names and the fifth item being a list of dimension sizes
        #
        # for every place in the dimension name list where the name is 'nlocs', replace
        # the corresponding size in the size list with self.nlocs
        for slist in [self.int_spec, self.evn_spec, self.rep_spec, self.seq_spec,
                      self.dim_spec, self.misc_spec]:
            for sub_slist in slist:
                for var_spec in sub_slist:
                    for i in [j for j, dname in enumerate(var_spec[3]) if dname == 'nlocs']:
                        var_spec[4][i] = self.nlocs

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
        for slist in [self.int_spec, self.evn_spec, self.rep_spec,
                      self.seq_spec, self.misc_spec]:
            for sub_slist in slist:
                for var_spec in sub_slist:
                    # dimension names are in the list given by var_spec[3]
                    AllDimNames = AllDimNames | set(var_spec[3])

        # AllDimNames holds the list of unique dimension names.
        # Keep the following list of dimensions in sync with the __init__ method in
        # in the ObsType base class.
        DimList = []
        for dname in AllDimNames:
            if (dname == 'nlocs'):
                dsize = self.nlocs
            elif (dname == 'nlevs'):
                dsize = self.nlevs
            elif (dname == 'nevents'):
                dsize = self.nevents
            elif (dname == 'nstring'):
                dsize = self.nstring
            elif (dname == 'nchans'):
                dsize = self.nchans
            elif (dname == 'nrecs'):
                dsize = self.nrecs
            elif (dname == 'nvars'):
                dsize = self.nvars
            else:
                print(
                    "ERROR: init_dim_spec: Unknown dimension name: {0:s}".format(dname))
                sys.exit(3)
            DimList.append([dname, dname, cm.DTYPE_UINT, [dname], [dsize]])

        self.dim_spec = [DimList]

    ###############################################################################
    # This method will create dimensions and variables in the netcdf file
    # according to the obs type variable specs.
    def create_nc_datasets(self, nc):

        # Create dimensions first so that the variables can reference them.
        nc.createDimension('nrecs', self.nrecs)   # placeholder for now
        nc.createDimension('nvars', self.nvars)
        for sub_slist in self.dim_spec:
            for dspec in sub_slist:
                nc.createDimension(dspec[0], dspec[4][0])

        # Create variables including the coordinates for the dimensions
        for slist in [self.dim_spec, self.int_spec, self.evn_spec,
                      self.rep_spec, self.seq_spec, self.misc_spec]:
            for sub_slist in slist:
                for var_spec in sub_slist:
                    Vname = var_spec[0]
                    Dtype = var_spec[2]
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
            BufrValues.append(bufr.read_subset(
                Mstring, events=Eflag, seq=Sflag, rep=Rflag))

        return BufrValues

    ###############################################################################
    # This method will convert bufr float data to the specified actual format.
    # BufrValues is a list of masked arrays, where each masked array contains
    # entries for all mnemonics in the sub-list of SpecList.
    def bufr_float_to_actual(self, SpecList, BufrValues, ActualValues):
        # Make a separate copy of the input dictionary
        OutVals = {key: value for key, value in ActualValues.items()}
        OutValsBufr = {key: value for key, value in ActualValues.items()}
        for SubSpecs, SubBvals in zip(SpecList, BufrValues):
            for VarSpec, Bval in zip(SubSpecs, SubBvals):
                # Convert according to the spec, and add to the dictionary.
                # Netcdf variable name is in VarSpec[0]
                # Data type is in VarSpec[2]

                OutVals[VarSpec[0]] = BufrFloatToActual(Bval, VarSpec[2])
                OutValsBufr[VarSpec[1]] = BufrFloatToActual(Bval, VarSpec[2])

        return [OutVals, OutValsBufr]

    ###############################################################################
    # This method will convert bufr float data to the specified actual format.
    # BufrValues is a list of masked arrays, where each masked array contains
    # entries for all mnemonics in the sub-list of SpecList.
    def bufr_float_to_actual_bufr(self, SpecList, BufrValues, ActualValues, ActualValuesBufr):
        # Make a separate copy of the input dictionary
        OutVals = {key: value for key, value in ActualValues.items()}
        OutValsBufr = {key: value for key, value in ActualValuesBufr.items()}
        for SubSpecs, SubBvals in zip(SpecList, BufrValues):
            for VarSpec, Bval in zip(SubSpecs, SubBvals):
                # Convert according to the spec, and add to the dictionary.
                # Netcdf variable name is in VarSpec[0]
                # Data type is in VarSpec[2]

                OutVals[VarSpec[0]] = BufrFloatToActual(Bval, VarSpec[2])
                OutValsBufr[VarSpec[1]] = BufrFloatToActual(Bval, VarSpec[2])

        return [OutVals, OutValsBufr]

    ###############################################################################
    # This method will take the four input spec lists and read the mnemonics
    # from the bufr file. This routine will also convert the bufr values to
    # corresponding netcdf values. This method will return a dictionary keyed
    # by the netcdf variable name containing the associated values.
    #
    # This method provides a defalut method that can be overridden by an obs type
    # requiring a more complex algorithm (Gpsro, eg.). ActualValues is a list
    # of dictionaries, and the default action is to create one item in that list.
    # This single dictionary will be filled in by simply walking through the
    # variables in the lists contained in int_spec, evn_spec, rep_spec and seq_spec,
    # reading the mnemonics out of the BUFR file, and loading in the results into
    # the single dictionary.
    def extract_bufr(self, bufr):
        # Initialize ActualValues to a list with one entry which is an empty dictionary.
        ActualValues = []
        ActualValues.append({})
        ActualValuesBufr = []
        ActualValuesBufr.append({})

        # Read and convert the individual data mnemonics. The mnemonic value is the second
        # entry in the int_spec sublist elements.
        Mlists = [[Mlist[1] for Mlist in SubList] for SubList in self.int_spec]
        BufrValues = self.read_bufr_data(bufr, Mlists)
        [ActualValues[0], ActualValuesBufr[0]] = self.bufr_float_to_actual_bufr(
            self.int_spec, BufrValues,
            ActualValues[0], ActualValuesBufr[0])

        # Read and convert the event mnemonics
        Mlists = [[Mlist[1] for Mlist in SubList] for SubList in self.evn_spec]
        BufrValues = self.read_bufr_data(bufr, Mlists, Eflag=True)
        [ActualValues[0], ActualValuesBufr[0]] = self.bufr_float_to_actual_bufr(
            self.evn_spec, BufrValues,
            ActualValues[0], ActualValuesBufr[0])

        # Read and convert the replication mnemonics
        Mlists = [[Mlist[1] for Mlist in SubList] for SubList in self.rep_spec]
        BufrValues = self.read_bufr_data(bufr, Mlists, Rflag=True)
        # [ActualValues[0], ActualValuesBufr[0]] = self.bufr_float_to_actual(self.rep_spec, BufrValues, ActualValues[0])
        [ActualValues[0], ActualValuesBufr[0]] = self.bufr_float_to_actual_bufr(
            self.rep_spec, BufrValues,
            ActualValues[0], ActualValuesBufr[0])

        # Read and convert the sequence mnemonics
        Mlists = [[Mlist[1] for Mlist in SubList] for SubList in self.seq_spec]
        BufrValues = self.read_bufr_data(bufr, Mlists, Sflag=True)
        [ActualValues[0], ActualValuesBufr[0]] = self.bufr_float_to_actual_bufr(
            self.seq_spec, BufrValues,
            ActualValues[0], ActualValuesBufr[0])

        return [ActualValues, ActualValuesBufr]

    ###############################################################################
    # This method will calculate the absolute date and time values from the BUFR
    # mnemonic values. The calculation depends on the type of BUFR file (raw BUFR
    # or prepBUFR). For raw BUFR, the absolute observation time comes from the
    # mnemonics:
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
    def calc_obs_date_time(self, ActualValues):
        if (self.bufr_ftype == cm.BFILE_PREPBUFR):
            # prepBUFR: use msg_date and DHR or HRDR

            # Form the absolute time by adding the message date with the
            # offset time.
            MsgDate = ActualValues['msg_date@MetaData'].data[0]
            MsgDtime = SplitMsgDate(MsgDate)

            # Get the offset from either DHR or HRDR. These values are
            # in hours. Use DHR by default. If HRDR is available with
            # a multilevel obs type, then use it.

            # Data is missing if corresponding mask value is True, so need
            # to verify that all mask values are False before using HRDR.
            HrdrAvailable = not np.any(ActualValues['HRDR@MetaData'].mask)

            OdateOffset = ActualValues['DHR@MetaData'].data
            if (self.multi_level):
                if (HrdrAvailable):
                    OdateOffset = ActualValues['HRDR@MetaData'].data
                else:
                    # Have a multi-level obs type, but using a single DHR value.
                    # Replicate the DHR value into a vector with the size of
                    # the multi-level obs type.
                    OdateOffset = np.repeat(
                        OdateOffset, ActualValues['HRDR@MetaData'].size)

            # At this point it is possible that OdateOffset is a scalar value
            # (indicated by size == 1). If so, then cast OdateOffest into a vector
            # of size one for the following loop.
            # The datetime routines don't accept arrays as arguments so
            # need to calculate absolute times one element at a time.
            Nlevs = OdateOffset.size
            if (Nlevs == 1):
                OdateOffset = np.array([OdateOffset])
            AbsDate = np.empty(Nlevs, dtype=np.int)
            AbsTime = np.empty(Nlevs, dtype=np.int)
            EpochTime = np.empty(Nlevs, dtype=np.int)
            for i in range(Nlevs):
                OffsetDtime = dt.timedelta(hours=float(OdateOffset[i]))
                AbsDtime = MsgDtime + OffsetDtime

                AbsDate[i] = MakeDate(AbsDtime)
                AbsTime[i] = MakeTime(AbsDtime)
                EpochTime[i] = MakeEpochTime(AbsDtime)

        else:
            # raw BUFR: use YEAR, MNTH, ...
            Year = int(ActualValues['YEAR'].data)
            Month = int(ActualValues['MNTH'].data)
            Day = int(ActualValues['DAYS'].data)
            Hour = int(ActualValues['HOUR'].data)
            Minute = int(ActualValues['MINU'].data)
            if ('SECO' in ActualValues):
                Second = int(ActualValues['SECO'].data)
            else:
                Second = int(0)

            # Create datetime object with above data. Sometimes the SECO value is
            # outside the range 0..59 (which is what datetime requires). Use Year through
            # Minute to create the datetime object and add in Second via a
            # timedelta object.
            ObsDtime = dt.datetime(
                Year, Month, Day, Hour, Minute) + dt.timedelta(seconds=Second)

            # Form as arrays so that their size can be checked.
            AbsDate = np.array([MakeDate(ObsDtime)])
            AbsTime = np.array([MakeTime(ObsDtime)])
            EpochTime = np.array([MakeEpochTime(ObsDtime)])

        return [AbsDate, AbsTime, EpochTime]

    ###############################################################################
    # This method will calculate the lat and lon values from the BUFR
    # mnemonic values. The calculation depends on the type of BUFR file (raw BUFR
    # or prepBUFR). For raw BUFR, the lat and lon vlaues come from the
    # mnemonics:
    #     CLAT  - latitude (coarse resolution)
    #     CLON  - longitude (coarse resolution)
    #                  OR
    #     CLATH  - latitude (high resolution)
    #     CLONH  - longitude (high resolution)
    #
    # For prepBUFR, the lat lon values come from the mnemonics:
    #     XOB   - For single level obs
    #     YOB   - For single level obs
    #                   OR
    #     XDR   - For multi-level obs (drift)
    #     YDR   - For multi-level obs (drift)
    #
    def calc_obs_lat_lon(self, ActualValues):
        if (self.bufr_ftype == cm.BFILE_PREPBUFR):
            # If multi-level obs, then use XDR, YDR. Otherwise, use XOB, YOB.
            if (self.multi_level):
                Lon = ActualValues['XDR@MetaData'].data
                Lat = ActualValues['YDR@MetaData'].data
            else:
                Lon = ActualValues['XOB@MetaData'].data
                Lat = ActualValues['YOB@MetaData'].data
        else:
            # Try CLATH and CLONH first.
            if ('CLATH@MetaData' in ActualValues):
                Lon = ActualValues['CLONH'].data
                Lat = ActualValues['CLATH'].data
            else:
                Lon = ActualValues['CLON'].data
                Lat = ActualValues['CLAT'].data

        return [Lat, Lon]

    ###############################################################################
    # This method will start the message selector. This selector method will
    # apply a few filters for selecting messages. These filters require
    # internal message counters that this method will reset.
    def start_msg_selector(self):
        self.num_msg_selected = 0
        self.num_msg_mtype = 0

    ###############################################################################
    # This method is the message selector. It will apply selection filters
    # to the input BUFR messages. This isn't a clean as it could be, but time
    # constraints are at work here!
    def select_next_msg(self, bufr):
        got_a_msg = False
        # Grab the next message
        while (bufr.advance() == 0):
            # Skip this message if not the desired type
            if (re.search(self.mtype_re, bufr.msg_type)):
                # Keep count of the messages that match the desired type, which is
                # needed to do the selection filtering.
                self.num_msg_mtype += 1

                # Apply the filtering. Default is to take all messages
                Select = True

                # If the max_num_msg parameter is greater than zero, then use it to limit
                # the number of messages that are selected.
                if (self.max_num_msg > 0):
                    Select = (self.num_msg_selected < self.max_num_msg)

                # If the thinning interval is greater than 1, then use it to further select
                # every n-th message.
                if (self.thin_interval > 1):
                    Select = Select and (
                        (self.num_msg_mtype % self.thin_interval) == 0)

                # If Select is true, the current message has been selected. Keep
                # track of how many messages have been selected, plus break out of
                # the loop and return.
                if (Select):
                    self.num_msg_selected += 1
                    got_a_msg = True
                    break

        return got_a_msg

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
        self.start_msg_selector()

        while (self.select_next_msg(bufr)):
            MsgType = np.ma.array(bufr.msg_type)
            MsgDate = np.ma.array([bufr.msg_date])
            while (bufr.load_subset() == 0):
                # Grab all of the mnemonics from the bufr file, and convert
                # from the BUFR float representation to the actual data type
                # (integer, float, string, double). ActualValues is a list of
                # dictionaries where each dictionary represents one observation.
                # A dictionary within the list is keyed by the netcdf variable
                # name and contains the associated data value.
                [ActualValues, ActualValuesBufr] = self.extract_bufr(bufr)

                for i in range(len(ActualValues)):
                    # Put the message type and message date into the dictionary.
                    ActualValues[i]['msg_type@MetaData'] = MsgType
                    ActualValues[i]['msg_date@MetaData'] = MsgDate

                    ActualValuesBufr[i]['msg_type@MetaData'] = MsgType
                    ActualValuesBufr[i]['msg_date@MetaData'] = MsgDate

                    # Calculate the value for the Time variable (which is an offset
                    # from the reference time). Add the Time value to the dictionary.
                    [ActualValues[i]['ObsDate@MetaData'], ActualValues[i]['ObsTime@MetaData'],
                        ActualValues[i]['time@MetaData']] = self.calc_obs_date_time(ActualValuesBufr[i])

                    _t1 = (ActualValues[i]['time@MetaData']).astype(np.float)
                    _t2 = np.array(dt.datetime.strptime(str(nc.date_time), '%Y%m%d%H'))
                    ActualValues[i]['time@MetaData'] = (_t1 - _t2) / 3600

                    # Calculate the value of lat and lon and add to the dictionary.
                    [ActualValues[i]['latitude@MetaData'], ActualValues[i]
                        ['longitude@MetaData']] = self.calc_obs_lat_lon(ActualValuesBufr[i])

                    # Write out the netcdf variables.
                    for Vname, Vdata in ActualValues[i].items():

                        # Skip the write if Vdata is empty
                        if Vdata.size:
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

# ############################### Aircraft Observation Type ############################


class AircraftObsType(ObsType):
    # # initialize data elements ###
    def __init__(self, bf_type):
        super(AircraftObsType, self).__init__()

        self.bufr_ftype = bf_type
        self.multi_level = False

        self.nvars = 4

        # Put the time and date vars in the subclasses so that their dimensions can
        # vary ( [nlocs], [nlocs,nlevs] ).
        self.misc_spec[0].append(
            ['ObsTime@MetaData', '', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['ObsDate@MetaData', '', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['time@MetaData', '', cm.DTYPE_DOUBLE, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['latitude@MetaData', '', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['longitude@MetaData', '', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]])

        if (bf_type == cm.BFILE_BUFR):
            self.mtype_re = '^NC00400[14]'
            self.int_spec = [
                [['year@MetaData', 'YEAR', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['mnth@MetaData', 'MNTH', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['days@MetaData', 'DAYS', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['hour@MetaData', 'HOUR', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['mInu@MetaData', 'MINU', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['ACID@MetaData', 'ACID', cm.DTYPE_DOUBLE,
                     ['nlocs'], [self.nlocs]],
                 ['CORN@MetaData', 'CORN', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['CLAT@MetaData', 'CLAT', cm.DTYPE_FLOAT,
                     ['nlocs'], [self.nlocs]],
                 ['CLON@MetaData', 'CLON', cm.DTYPE_FLOAT,
                     ['nlocs'], [self.nlocs]],
                 ['FLVL@MetaData', 'FLVL', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]]],

                [['air_temperature@ObsValue', 'TMDB', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['relative_humidity@ObsValue', 'REHU',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['wind_speed@ObsValue', 'WSPD',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['wind_to_direction@ObsValue', 'WDIR',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['air_temperature@PreQC', 'QMAT',
                     cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['relative_humidity@PreQC', 'QMDD',
                     cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['wind@PreQC', 'QMWN', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]]],

                [['SEQNUM@MetaData', 'SEQNUM', cm.DTYPE_STRING, ['nlocs', 'nstring'], [self.nlocs, self.nstring]],
                 ['BUHD@MetaData', 'BUHD', cm.DTYPE_STRING, [
                     'nlocs', 'nstring'], [self.nlocs, self.nstring]],
                 ['BORG@MetaData', 'BORG', cm.DTYPE_STRING, [
                     'nlocs', 'nstring'], [self.nlocs, self.nstring]],
                 ['BULTIM@MetaData', 'BULTIM', cm.DTYPE_STRING, [
                     'nlocs', 'nstring'], [self.nlocs, self.nstring]],
                 ['BBB@MetaData', 'BBB', cm.DTYPE_STRING, [
                     'nlocs', 'nstring'], [self.nlocs, self.nstring]],
                 ['RPID@MetaData', 'RPID', cm.DTYPE_STRING, ['nlocs', 'nstring'], [self.nlocs, self.nstring]]],
            ]
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []
        elif (bf_type == cm.BFILE_PREPBUFR):
            self.mtype_re = 'AIRC[AF][RT]'
            self.int_spec = [
                [['SID@MetaData', 'SID', cm.DTYPE_DOUBLE, ['nlocs'], [self.nlocs]],
                 ['ACID@MetaData', 'ACID', cm.DTYPE_DOUBLE, ['nlocs'], [self.nlocs]],
                 ['XOB@MetaData', 'XOB', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['YOB@MetaData', 'YOB', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['DHR@MetaData', 'DHR', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['TYP@MetaData', 'TYP', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['ELV@MetaData', 'ELV', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['SAID@MetaData', 'SAID', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['T29@MetaData', 'T29', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]]],

                [['air_pressure@MetaData', 'POB', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['height@MetaData', 'ZOB',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['specific_humidity@ObsValue', 'QOB',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['air_temperature@ObsValue', 'TOB',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['eastward_wind@ObsValue', 'UOB',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['northward_wind@ObsValue', 'VOB', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]]],

                [['air_pressure@PreQC', 'PQM', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['specific_humidity@PreQC', 'QQM',
                     cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['air_temperature@PreQC', 'TQM',
                     cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['WQM@PreQC', 'WQM', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]]],

                [['air_pressure@ObsError', 'POE', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['specific_humidity@ObsError', 'QOE',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['air_temperature@ObsError', 'TOE', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]]],

                [['HOVI@MetaData', 'HOVI', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['CAT@MetaData', 'CAT', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['XDR@MetaData', 'XDR', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['YDR@MetaData', 'YDR', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['HRDR@MetaData', 'HRDR', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['POAF@MetaData', 'POAF', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['IALR@MetaData', 'IALR', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]]],
            ]
            self.evn_spec = [
                [['TPC_bevn@MetaData', 'TPC', cm.DTYPE_INTEGER, ['nlocs', 'nevents'], [self.nlocs, self.nevents]],
                 ['TOB_bevn@MetaData', 'TOB', cm.DTYPE_FLOAT, [
                     'nlocs', 'nevents'], [self.nlocs, self.nevents]],
                 ['TQM_bevn@MetaData', 'TQM', cm.DTYPE_INTEGER, ['nlocs', 'nevents'], [self.nlocs, self.nevents]]],
            ]
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super(AircraftObsType, self).init_dim_spec()

    # # methods ###


# ############################### Radiosonde Observation Type ############################
class SondesObsType(ObsType):
    # # initialize data elements ###
    def __init__(self, bf_type):
        super(SondesObsType, self).__init__()

        self.bufr_ftype = bf_type
        self.multi_level = True

        self.nvars = 4

        # Put the time and date vars in the subclasses so that their dimensions can
        # vary ( [nlocs], [nlocs,nlevs] ).
        self.misc_spec[0].append(['ObsTime@MetaData', '', cm.DTYPE_INTEGER, [
                                 'nlocs', 'nlevs'], [self.nlocs, self.nlevs]])
        self.misc_spec[0].append(['ObsDate@MetaData', '', cm.DTYPE_INTEGER, [
                                 'nlocs', 'nlevs'], [self.nlocs, self.nlevs]])
        self.misc_spec[0].append(['time@MetaData', '', cm.DTYPE_DOUBLE, [
                                 'nlocs', 'nlevs'], [self.nlocs, self.nlevs]])
        self.misc_spec[0].append(['latitude@MetaData', '', cm.DTYPE_FLOAT, [
                                 'nlocs', 'nlevs'], [self.nlocs, self.nlevs]])
        self.misc_spec[0].append(['longitude@MetaData', '', cm.DTYPE_FLOAT, [
                                 'nlocs', 'nlevs'], [self.nlocs, self.nlevs]])

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
            #            'TSB', 'ITP', 'SQN','PROCN', 'RPT', 'TCOR', 'SIRC',
            #        EVENTS VARS? *PC, *RC, *FC , TVO
            #
            self.mtype_re = 'ADPUPA'
            self.int_spec = [
                [['SID@MetaData', 'SID', cm.DTYPE_DOUBLE, ['nlocs'], [self.nlocs]],
                 ['XOB@MetaData', 'XOB', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['YOB@MetaData', 'YOB', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['DHR@MetaData', 'DHR', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['TYP@MetaData', 'TYP', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['ELV@MetaData', 'ELV', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['T29@MetaData', 'T29', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]]],

                [['air_pressure@MetaData', 'POB', cm.DTYPE_FLOAT, ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['height@MetaData', 'ZOB', cm.DTYPE_FLOAT,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['specific_humidity@ObsValue', 'QOB', cm.DTYPE_FLOAT,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['air_temperature@ObsValue', 'TOB', cm.DTYPE_FLOAT,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['eastward_wind@ObsValue', 'UOB', cm.DTYPE_FLOAT,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['northward_wind@ObsValue', 'VOB', cm.DTYPE_FLOAT, ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]]],

                [['air_pressure@PreQC', 'PQM', cm.DTYPE_INTEGER, ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['specific_humidity@PreQC', 'QQM', cm.DTYPE_INTEGER,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['air_temperature@PreQC', 'TQM', cm.DTYPE_INTEGER,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['WQM@PreQC', 'WQM', cm.DTYPE_INTEGER, ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]]],

                [['air_pressure@ObsError', 'POE', cm.DTYPE_FLOAT, ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['specific_humidity@ObsError', 'QOE', cm.DTYPE_FLOAT,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['air_temperature@ObsError', 'TOE', cm.DTYPE_FLOAT,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['XDR@MetaData', 'XDR', cm.DTYPE_FLOAT,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['YDR@MetaData', 'YDR', cm.DTYPE_FLOAT,
                     ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]],
                 ['HRDR@MetaData', 'HRDR', cm.DTYPE_FLOAT, ['nlocs', 'nlevs'], [self.nlocs, self.nlevs]]],

            ]
            self.evn_spec = [
                [['TPC_bevn@MetaData', 'TPC', cm.DTYPE_INTEGER, ['nlocs', 'nlevs', 'nevents'], [self.nlocs, self.nlevs, self.nevents]],
                 ['TOB_bevn@MetaData', 'TOB', cm.DTYPE_FLOAT, [
                     'nlocs', 'nlevs', 'nevents'], [self.nlocs, self.nlevs, self.nevents]],
                 ['TQM_bevn@MetaData', 'TQM', cm.DTYPE_INTEGER, ['nlocs', 'nlevs', 'nevents'], [self.nlocs, self.nlevs, self.nevents]]]
            ]
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super(SondesObsType, self).init_dim_spec()

    # # methods ###


# ######################### Radiance (AMSU-A) Observation Type ############################
class AmsuaObsType(ObsType):
    # # initialize data elements ###
    def __init__(self, bf_type):
        super(AmsuaObsType, self).__init__()

        self.nchans = 20  # This is unique to AMSU
        self.bufr_ftype = bf_type
        self.multi_level = False
        self.nvars = self.nchans

        # Put the time and date vars in the subclasses so that their dimensions can
        # vary ( [nlocs], [nlocs,nlevs] ).
        self.misc_spec[0].append(
            ['ObsTime@MetaData', '', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['ObsDate@MetaData', '', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['time@MetaData', '', cm.DTYPE_DOUBLE, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['latitude@MetaData', '', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['longitude@MetaData', '', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]])

        if (bf_type == cm.BFILE_BUFR):

            self.mtype_re = '^NC021023'
            self.int_spec = [
                [['SAID@MetaData', 'SAID', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['FOVN@MetaData', 'FOVN', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['YEAR@MetaData', 'YEAR', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['MNTH@MetaData', 'MNTH', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['DAYS@MetaData', 'DAYS', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['HOUR@MetaData', 'HOUR', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['MINU@MetaData', 'MINU', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['SECO@MetaData', 'SECO', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['CLAT@MetaData', 'CLAT', cm.DTYPE_FLOAT,
                     ['nlocs'], [self.nlocs]],
                 ['CLON@MetaData', 'CLON', cm.DTYPE_FLOAT,
                     ['nlocs'], [self.nlocs]],
                 ['HOLS@MetaData', 'HOLS', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]]],

                [['sensor_zenith_angle@MetaData', 'SAZA', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['solar_zenith_angle@MetaData', 'SOZA',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['sensor_azimuth_angle@MetaData', 'BEARAZ',
                     cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]],
                 ['solar_azimuth_angle@MetaData', 'SOLAZI', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]]]

            ]
            self.evn_spec = []
            self.rep_spec = [
                [['channel_number@MetaData', 'CHNM', cm.DTYPE_INTEGER, ['nlocs', 'nchans'], [self.nlocs, self.nchans]],
                 ['brightness_temperature@ObsValue', 'TMBR', cm.DTYPE_FLOAT,
                     ['nlocs', 'nchans'], [self.nlocs, self.nchans]],
                 ['CSTC@MetaData', 'CSTC', cm.DTYPE_FLOAT, ['nlocs', 'nchans'], [self.nlocs, self.nchans]]]

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

    # # methods ###

# ######################### GPSRO Observation Type ############################


class GpsroObsType(ObsType):
    # # initialize data elements ###
    def __init__(self, bf_type):
        super(GpsroObsType, self).__init__()

        # For nc variable 'profile_number'
        self.subset_num = 0

        self.bufr_ftype = bf_type
        self.multi_level = False
        self.misc_dtype = cm.DTYPE_FLOAT

        self.nvars = 2

        # Put the time and date vars in the subclasses so that their dimensions can
        # vary ( [nlocs], [nlocs,nlevs] ).
        self.misc_spec[0].append(
            ['ObsTime', '', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['ObsDate', '', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['time', '', cm.DTYPE_DOUBLE, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['latitude', '', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]])
        self.misc_spec[0].append(
            ['longitude', '', cm.DTYPE_FLOAT, ['nlocs'], [self.nlocs]])

        if (bf_type == cm.BFILE_BUFR):

            self.mtype_re = '^NC003010'
            self.int_spec = [
                [['SAID@MetaData', 'SAID', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]],
                 ['YEAR@MetaData', 'YEAR', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['MNTH@MetaData', 'MNTH', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['DAYS@MetaData', 'DAYS', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['HOUR@MetaData', 'HOUR', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['MINU@MetaData', 'MINU', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['SECO@MetaData', 'SECO', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['ELRC@MetaData', 'ELRC', cm.DTYPE_FLOAT,
                     ['nlocs'], [self.nlocs]],
                 ['PCCF@MetaData', 'PCCF', cm.DTYPE_FLOAT,
                     ['nlocs'], [self.nlocs]],
                 ['PTID@MetaData', 'PTID', cm.DTYPE_INTEGER,
                     ['nlocs'], [self.nlocs]],
                 ['GEODU@MetaData', 'GEODU', cm.DTYPE_FLOAT,
                     ['nlocs'], [self.nlocs]],
                 ['QFRO@MetaData', 'QFRO', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]]],

            ]
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []

            # These are the observation variables that will be extracted by the
            # convert method of this class (which overrides the base class convert
            # method).
            self.misc_spec[0].append(['CLATH@MetaData',
                                      '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(['CLONH@MetaData',
                                      '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(['height@MetaData',
                                      '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(
                ['atmospheric_refractivity@ObsValue', '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(
                ['atmospheric_refractivity@ObsError', '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(
                ['atmospheric_refractivity@PreQC', '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(['bending_angle@ObsValue',
                                      '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(
                ['bending_angle@ObsError', '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(
                ['bending_angle@PreQc', '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(['mean_frequency@MetaData',
                                      '', self.misc_dtype, ['nlocs'], [self.nlocs]])
            self.misc_spec[0].append(['impact_parameter@MetaData',
                                      '', self.misc_dtype, ['nlocs'], [self.nlocs]])

            # Subset count
            self.misc_spec[0].append(
                ['profile_number@MetaData', '', cm.DTYPE_INTEGER, ['nlocs'], [self.nlocs]])

        elif (bf_type == cm.BFILE_PREPBUFR):
            self.mtype_re = 'UnDef'
            self.int_spec = []
            self.evn_spec = []
            self.rep_spec = []
            self.seq_spec = []

        # Set the dimension specs.
        super(GpsroObsType, self).init_dim_spec()

    # # methods ###

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
        # from the mnemonic '{ROSEQ2}' should be equal to the value returned
        # from the mnemonic '(ROSEQ1)'.
        #
        # Mnemonic '(ROSEQ1)' returns the number of bending angle obs
        # Mnemonic '(ROSEQ3)' returns the number of refractivity obs
        #
        # Mnemonic '{ROSEQ2}' returns the number of frequencies within each bend angle obs
        #                     this value is an array since the number of frequencies can
        #                     change on each bend angle obs
        #
        BangleFreqCnts = bufr.read_subset('{ROSEQ2}').astype(int).squeeze()

        NumBangle = bufr.read_subset('(ROSEQ1)').astype(int).squeeze()
        NumRefrac = bufr.read_subset('(ROSEQ3)').astype(int).squeeze()

        return [NumBangle, NumRefrac, BangleFreqCnts]

    ########################################################################
    # This method will count up the number of observations contained in a
    # single message of a GPSRO obs type.
    def msg_obs_count(self, bufr):
        ObsCount = 0

        # Visit all subsets and count up the number of obs contained in each
        # subset.
        SubsetCount = 1
        while (bufr.load_subset() == 0):
            # Read and convert the header data (list in int_spec)
            Mlists = [[Mlist[1] for Mlist in SubList]
                      for SubList in self.int_spec]
            BufrValues = self.read_bufr_data(bufr, Mlists)
            HeaderVals = self.bufr_float_to_actual(
                self.int_spec, BufrValues, {})

            [NumBangle, NumRefrac,
                BangleFreqCnts] = self.extract_gpsro_obs_counts(bufr)

            if (self.select_this_subset(bufr, HeaderVals['SAID'], HeaderVals['PTID'],
                                        HeaderVals['QFRO'], NumBangle, BangleFreqCnts, Verbose=True)):
                # Have same number of bending angle and refractivity observations
                # Even though there can be multiple frequencies within a single
                # bending angle obs, we are only going to take the zero frequency
                # entry. That is, only one frequency per bending angle obs. Therefore,
                # the number of obs this subset is equal to the number of bend
                # angle obs.
                ObsCount += NumBangle
            else:
                print("WARNING: Gpsro: Skipping report: Message number: {0:d}, Subset number: {1:d}".format(
                    bufr.msg_counter, SubsetCount))
                print("")
                continue

            SubsetCount += 1

        return int(ObsCount)

    ###############################################################################
    # This method will return true if the subset is acceptable, false otherwise.
    def select_this_subset(self, bufr, Said, Ptid, Qfro, NumBangle, BangleFreqCnts,
                           Verbose=False):
        Select = True

        # Sanity check: make sure length of bending angle frequency counts vector
        # matches the number of bending angle obs.
        if (NumBangle != BangleFreqCnts.size):
            Select = False
            if (Verbose):
                print(
                    "WARNING: Gpsro: Skip report due to mismatch in ROSEQ1 and ROSEQ2 occurence")

        # For certain satellite ids, skip this subset if the non-nominal flags
        # are set for bending angle (flag value == 5). This is based on GRAS SAF specs.
        #
        # Note, (flag value == 6) indicates that reflectivity data is non-nominal. Since
        # we are preferring bending angle over reflectivity, don't skip if 6 appears in
        # the list. Only skip if 5 (bend angle non-nominal) appears in the list.
        if (Said in [3, 4, 421, 440, 821]):
            Ibits = bufr.get_flag_table_bits('QFRO', Qfro)
            if (5 in Ibits):
                Select = False
                if (Verbose):
                    print("WARNING: Gpsro: Skip report due to bad profile for bending angle, said = {0:d}, ptid = {1:d}".format(
                        Said, Ptid))

        return Select

    ###############################################################################
    # This method will extract data from a subset of a Gpsro raw BUFR file and
    # load up the observations into the ActualValues data structure. ActualValues
    # is a list of dictionaries where each dictionary holds one observation.
    def extract_bufr(self, bufr):
        ActualValues = []

        # Read and convert the header data (list in int_spec)
        Mlists = [[Mlist[1] for Mlist in SubList] for SubList in self.int_spec]
        BufrValues = self.read_bufr_data(bufr, Mlists)
        HeaderVals = self.bufr_float_to_actual(self.int_spec, BufrValues, {})

        [NumBangle, NumRefrac,
            BangleFreqCnts] = self.extract_gpsro_obs_counts(bufr)

        # Run this subset through a selection filter. The warnings about skipping
        # subsets have already been written by the msg_obs_count() method.
        if (self.select_this_subset(bufr, HeaderVals['SAID'], HeaderVals['PTID'],
                                    HeaderVals['QFRO'], NumBangle, BangleFreqCnts)):
            # Update subset number.
            self.subset_num += 1

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
                #   CLATH CLONH BEARAZ ROSEQ2-1 ROSEQ2-2 ROSEQ2-3 PCCF  (for n = 3)
                #
                #         where ROSEQ2-n just means the nth replication of ROSEQ2
                #
                # Each subset contains bending angle data, and refractivity data may
                # or may not be present. The bending angle data is preferred over the
                # refractivity data so record every subset, and check to see if
                # refractivity data exists. If so, record them; if not, fill in
                # ActualValue slots with "missing" data.

                # Grab the BUFR values for the obs data
                # Latitude
                CLATH = np.ma.array(
                    BangleBvals[0, irep], mask=BangleBvals.mask[0, irep])

                # Longitude
                CLONH = np.ma.array(
                    BangleBvals[1, irep], mask=BangleBvals.mask[1, irep])

                # Height + Refractivity data
                # Typically, the number of refractivity obs is either zero or a match
                # with the number of bending angle obs. Just in case a subset shows
                # up with a different numbers of refractivity and bending angle obs, where
                # the refractivity number is greater than zero, allow for recording those.
                if (irep < NumRefrac):
                    HEIT = np.ma.array(
                        RfracBvals[0, irep], mask=RfracBvals.mask[0, irep])

                    # Refractivity data
                    ARFR = np.ma.array(
                        RfracBvals[1, irep], mask=RfracBvals.mask[0, irep])
                    ARFR_err = np.ma.array(
                        RfracBvals[3, irep], mask=RfracBvals.mask[0, irep])
                    ARFR_pccf = np.ma.array(
                        RfracBvals[5, irep], mask=RfracBvals.mask[0, irep])
                else:
                    HEIT = np.ma.array([0.0], mask=[True])
                    ARFR = np.ma.array([0.0], mask=[True])
                    ARFR_err = np.ma.array([0.0], mask=[True])
                    ARFR_pccf = np.ma.array([0.0], mask=[True])

                # Bending Angle data
                # Locate the zero frequency sequence. Use missing data if the zero
                # frequency data is not available in the BUFR file. Note that the
                # code below does not break out of the for loop upon finding a
                # zero frequency. The Fortran code that served as a model for
                # for this script (read_gps.f90 from GSI repo) did not break out
                # as well. This shouldn't be a performance problem since the typical
                # numbers of frequencies is either 1 or 3.
                MEFR = np.ma.array([0.0], mask=[True])
                IMPP = np.ma.array([0.0], mask=[True])
                BNDA = np.ma.array([0.0], mask=[True])
                BNDA_err = np.ma.array([0.0], mask=[True])
                for i in range(BangleFreqCnts[irep]):
                    m = 6*(i+1)-3
                    if ((int(BangleBvals[m, irep]) == 0) and (not BangleBvals.mask[m, irep])):
                        # This replication has zero frequency which is not masked
                        # (that is, not marked as missing).
                        MEFR = np.ma.array(BangleBvals[m, irep],
                                           mask=BangleBvals.mask[m, irep])    # mean frequency
                        IMPP = np.ma.array(BangleBvals[m+1, irep],
                                           mask=BangleBvals.mask[m+1, irep])  # impact parameter
                        BNDA = np.ma.array(BangleBvals[m+2, irep],
                                           mask=BangleBvals.mask[m+2, irep])  # bending angle
                        BNDA_err = np.ma.array(BangleBvals[m+4, irep],
                                               mask=BangleBvals.mask[m+4, irep])  # bending angle error

                # BNDA_pccf is at the end of the ROSEQ1 section, i.e. one after the
                # BangleFreqCnts[irep] replications of the current ROSEQ2 section.
                m = 6*BangleFreqCnts[irep] + 3
                BNDA_pccf = np.ma.array(BangleBvals[m, irep],
                                        mask=BangleBvals.mask[m, irep])   # bending angle pccf

                # Convert and fill in the ActualValues dictionary
                ActualValues[irep]['CLATH@MetaData'] = BufrFloatToActual(
                    CLATH, self.misc_dtype)
                ActualValues[irep]['CLONH@MetaData'] = BufrFloatToActual(
                    CLONH, self.misc_dtype)
                ActualValues[irep]['height@MetaData'] = BufrFloatToActual(
                    HEIT, self.misc_dtype)

                ActualValues[irep]['atmospheric_refractivity@ObsValue'] = BufrFloatToActual(
                    ARFR, self.misc_dtype)
                ActualValues[irep]['atmospheric_refractivity@ObsError'] = BufrFloatToActual(
                    ARFR_err, self.misc_dtype)
                ActualValues[irep]['atmospheric_refractivity@PreQC'] = BufrFloatToActual(
                    ARFR_pccf, self.misc_dtype)

                ActualValues[irep]['mean_frequency@MetaData'] = BufrFloatToActual(
                    MEFR, self.misc_dtype)
                ActualValues[irep]['impact_parameter@MetaData'] = BufrFloatToActual(
                    IMPP, self.misc_dtype)
                ActualValues[irep]['bending_angle@ObsValue'] = BufrFloatToActual(
                    BNDA, self.misc_dtype)
                ActualValues[irep]['bending_angle@ObsError'] = BufrFloatToActual(
                    BNDA_err, self.misc_dtype)
                ActualValues[irep]['bending_angle@ObsPreQC'] = BufrFloatToActual(
                    BNDA_pccf, self.misc_dtype)

                ActualValues[irep]['profile_number@MetaData'] = np.ma.array([
                                                                            self.subset_num])

        else:
            # This subset has been rejected, so empty out the ActualValues list so that
            # nothing will get recorded into the output netcdf file.
            ActualValues = []

        return ActualValues
