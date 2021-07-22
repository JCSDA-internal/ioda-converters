#!/usr/bin/env python3

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


def BufrFloatToActual(Bval, Dtype):
    # This routine will extract the value of a variable from the
    # output of read_subset(). read_subset() will return a floating point
    # number (for any type) or an empty list if the mnemonic didn't exist. For
    # strings(Dtype = DTYPE_STRING) read the floating point number as
    # characters. Otherwise convert to integer or leave alone.
    #
    # Keep Dtype values in sync with entries in the DATA_TYPES dictionary. For
    # now, these values are DTYPE_STRING, DTYPE_INTEGER, DTYPE_FLOAT,
    # DTYPE_DOUBLE.

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
        else:
            Dval = np.ma.array(Bval.data.astype(np.float32),
                               mask=Bval.mask, dtype=np.float32)

    # Squeeze the array since read_subset can return size 1 dimensions (eg.
    # nlevs).
    return Dval.squeeze()


def WriteNcVar(Fid, ObsNum, Vname, Vdata, isProfile=False):
    # This routine will write into a variable in the output netCDF file

    # For the string data, convert to a numpy character array
    if ((Vdata.dtype.char == 'S') or (Vdata.dtype.char == 'U')):
        StrSpec = "S{0:d}".format(cm.MAX_STRING_LEN)
        Value = netCDF4.stringtochar(Vdata.astype(StrSpec))
    else:
        Value = Vdata.copy()

    # At this point, the dimension sizes of the netcdf variable (NcVar) and
    # Value need to get aligned. For some dimensions, the NcVar dimension size
    # will tend to be larger than the Value dimension size (eg, nlevs). For
    # other dimensions, it will be the other way around (eg, nevents). The one
    # thing that can be counted on is that the list of dimensions will match
    # between NcVar and Value, except that NcVar will have nlevs as an extra
    # dimension, and nlocs will be the first in the dimension list.
    # For example, if you have a multi-level event:
    #
    #    Value dimensions will be [ nlevs, nevents ]
    #    Ncvar dimensions will be [ nlocs, nlevs, nevents ]
    #
    # This means that to reconcile the sizes of each dimension, we need to
    # slice out of the minimum sizes of the corresponding dimension of NcVar
    # and Value.
    # Using the example above, for a multi-level event:
    #
    #    Value dimensions are [ 51, 255 ]
    #    NcVar dimensions are [ 1000, 255, 20 ]
    #
    #    nlevs size for Value is 51, for NcVar is 255 so use 51 for the slicing
    #    nevents size for Value is 255, for NcVar is 20 so use 20 for the
    #    slicing. The assignment then becomes
    #
    #        NcVar[ObsNum, 0:51, 0:20] = Value[0:51, 0:20]
    #
    # Figure out how to do the slicing by looking at the number of dimensions
    # at both Value (masked array) and NcVar (netcdf array). A masked array
    # that gets built using a scalar data value will return 0 for the number
    # of dimensions.
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
        if isProfile:
            if Value.shape == ():
                NcVar[ObsNum] = Value
            else:
                NcVar[ObsNum:ObsNum+Value.shape[0]] = Value
        else:
            NcVar[ObsNum] = Value
    elif (NcNdim == 2):
        if (ValNdim == 0):
            # Value is a scalar (representing a single level value)
            # NcVar has two dimensions (eg, [nlocs,nlevs])
            NcVar[ObsNum, 0] = Value
        else:
            # Value has one dimension  (eg, [nlevs])
            # NcVar has two dimensions (eg, [nlocs,nlevs])
            if isProfile:
                N1 = NcVar.shape[1]
                if len(Value.shape) == 1:
                    NcVar[ObsNum, 0:N1] = Value[0:N1]
                else:
                    for i in range(Value.shape[0]):
                        NcVar[(ObsNum+i), 0:N1] = Value[0:N1]
            else:
                N1 = min(Value.shape[0], NcVar.shape[1])
                # N1 = Value.shape[0]
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
            # Value has two dimensions and is single level (eg,
            # [nstring,nevents])
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

# ############################### Base Observation Type ##################


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

    ##########################################################################
    # This method will set the number of observations. This must be called
    # before attempting to create any netcdf variables since self.nlocs
    # is also used to define the dimension sizes in all of the netcdf
    # variables.
    def set_nlocs(self, nlocs):
        # update the data memeber
        self.nlocs = nlocs

        # update the dimension sizes in the specs
        #
        # each spec is a list of variable specs
        # each variable spec is a list with the fourth item being a list of
        #    dimension names and the fifth item being a list of dimension sizes
        #
        # for every place in the dimension name list where the name is 'nlocs',
        # replace the corresponding size in the size list with self.nlocs
        for slist in [self.int_spec, self.evn_spec, self.rep_spec, self.seq_spec,
                      self.dim_spec, self.misc_spec]:
            for sub_slist in slist:
                for var_spec in sub_slist:
                    for i in [j for j, dname in enumerate(
                            var_spec[3]) if dname == 'nlocs']:
                        var_spec[4][i] = self.nlocs

    ##########################################################################
    # This method is a default routine for counting the number of observations
    # in the current BUFR message. The default number of observations is simply
    # the number of subsets in this message. The reason that this is a method
    # in the base class is so that derived classes (GpsroObsType, for example)
    # can override this method with a more complex algorithm.
    def msg_obs_count(self, bufr):
        return bufr._subsets()

    ##########################################################################
    # This method will set the dimension specs (data memeber self.dim_spec).
    # The format for the dim_spec will match that of the other specs (eg,
    # self.int_spec).
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
        # Keep the following list of dimensions in sync with the
        # __init__ method in the ObsType base class.
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

    ##########################################################################
    # This method will create dimensions and variables in the netcdf file
    # according to the obs type variable specs.
    def create_nc_datasets(self, nc, isProfile=False):

        # Create dimensions first so that the variables can reference them.
        nc.createDimension('nrecs', self.nrecs)   # placeholder for now
        nc.createDimension('nvars', self.nvars)
        for sub_slist in self.dim_spec:
            for dspec in sub_slist:
                if isProfile:
                    if dspec[0] == "nlocs":
                        nc.createDimension(dspec[0], 0)
                    else:
                        nc.createDimension(dspec[0], dspec[4][0])
                else:
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
                    # Don't specify the chunk size. Since all of the dimensions
                    # are of fixed size, the built-in algorithm for calculating
                    # chunk sizes will do a good job.
                    nc.createVariable(Vname, Vtype, DimNames, zlib=True,
                                      shuffle=True, complevel=6)

    ##########################################################################
    # This method will fill in the dimension variables with coordinate values.
    # For now, using dummy values which are 1..n where n is the variable size.
    def fill_coords(self, nc):
        for DimSpecs in self.dim_spec:
            for VarSpec in DimSpecs:
                Vname = VarSpec[0]
                Value = np.arange(VarSpec[4][0]) + 1
                nc[Vname][:] = Value

    ##########################################################################
    # This method will read in a list of bufr mnemonics and return a list of
    # the corresponding data values.
    def read_bufr_data(self, bufr, Mlists, Rflag=False,
                       Sflag=False, Eflag=False):
        BufrValues = []

        # Mlists contains sub-lists of mnemonic names. Process each sub-list by
        # reading all mnemonics in that sub-list in one call to read_subset().
        for MnemonicList in Mlists:
            Mstring = " ".join(MnemonicList)
            BufrValues.append(bufr.read_subset(
                Mstring, events=Eflag, seq=Sflag, rep=Rflag))

        return BufrValues

    ##########################################################################
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
                if (VarSpec[1] != 'RRSTG'):
                    OutVals[VarSpec[0]] = BufrFloatToActual(Bval, VarSpec[2])
                    OutValsBufr[VarSpec[1]] = BufrFloatToActual(
                        Bval, VarSpec[2])

        return [OutVals, OutValsBufr]

    ##########################################################################
    # This method will convert bufr float data to the specified actual format.
    # BufrValues is a list of masked arrays, where each masked array contains
    # entries for all mnemonics in the sub-list of SpecList.
    def bufr_float_to_actual_bufr(
            self, SpecList, BufrValues, ActualValues, ActualValuesBufr):
        # Make a separate copy of the input dictionary
        OutVals = {key: value for key, value in ActualValues.items()}
        OutValsBufr = {key: value for key, value in ActualValuesBufr.items()}
        for SubSpecs, SubBvals in zip(SpecList, BufrValues):
            for VarSpec, Bval in zip(SubSpecs, SubBvals):
                # Convert according to the spec, and add to the dictionary.
                # Netcdf variable name is in VarSpec[0]
                # Data type is in VarSpec[2]
                if (VarSpec[1] != 'RRSTG'):
                    OutVals[VarSpec[0]] = BufrFloatToActual(Bval, VarSpec[2])
                    OutValsBufr[VarSpec[1]] = BufrFloatToActual(
                        Bval, VarSpec[2])
        return [OutVals, OutValsBufr]

    ##########################################################################
    # This method will take the four input spec lists and read the mnemonics
    # from the bufr file. This routine will also convert the bufr values to
    # corresponding netcdf values. This method will return a dictionary keyed
    # by the netcdf variable name containing the associated values.
    #
    # This method provides a defalut method that can be overridden by an obs
    # type requiring a more complex algorithm (Gpsro, eg.). ActualValues is a
    # list of dictionaries, and the default action is to create one item in
    # that list.
    # This single dictionary will be filled in by simply walking through the
    # variables in the lists contained in int_spec, evn_spec, rep_spec and
    # seq_spec, reading the mnemonics out of the BUFR file, and loading in the
    # results into the single dictionary.
    def extract_bufr(self, bufr):
        # Initialize ActualValues to a list with one entry which is an empty
        # dictionary.
        ActualValues = []
        ActualValues.append({})
        ActualValuesBufr = []
        ActualValuesBufr.append({})

        # Read and convert the individual data mnemonics. The mnemonic value
        # is the second entry in the int_spec sublist elements.
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

    ##########################################################################
    # This method will calculate the absolute date and time values from the
    # BUFR mnemonic values. The calculation depends on the type of BUFR file
    # (raw BUFR or prepBUFR). For raw BUFR, the absolute observation time comes
    # from the mnemonics:
    #     YEAR  - year
    #     MNTH  - month
    #     DAYS  - day
    #     HOUR  - hour
    #     MINU  - minute
    #     SECO  - second
    #
    # For prepBUFR, the time relative to msg_date is held in DHR, and for
    # multi-level obs in HRDR.
    #     msg_date - Message date/time
    #     DHR      - Observation time minus cycle time
    #     HRDR     - Observation time minus cycle time on a level by level
    #                   basis (taking drift into account)
    #
    def calc_obs_date_time(self, ActualValues):
        # raw BUFR: use YEAR, MNTH, ...
        Year = int(ActualValues['YEAR'].data)
        Month = int(ActualValues['MNTH'].data)
        Day = int(ActualValues['DAYS'].data)
        Hour = int(ActualValues['HOUR'].data)
        Minute = int(ActualValues['MINU'].data)
        if ('SECO' in ActualValues):
            try:
                Second = int(ActualValues['SECO'].data)
            except Exception:
                Second = int(0)
        else:
            Second = int(0)

        # Create datetime object with above data. Sometimes the SECO value
        # is outside the range 0..59 (which is what datetime requires).
        # Use Year through Minute to create the datetime object and add in
        # Second via a timedelta object.
        ObsDtime = dt.datetime(
            Year, Month, Day, Hour, Minute) + dt.timedelta(seconds=Second)
        DateTime = np.array(ObsDtime.strftime("%Y-%m-%dT%H:%M:%SZ"))
        return [DateTime]

    ##########################################################################
    # This method will start the message selector. This selector method will
    # apply a few filters for selecting messages. These filters require
    # internal message counters that this method will reset.

    def start_msg_selector(self):
        self.num_msg_selected = 0
        self.num_msg_mtype = 0

    ##########################################################################
    # This method is the message selector. It will apply selection filters
    # to the input BUFR messages. This isn't a clean as it could be, but time
    # constraints are at work here!
    def select_next_msg(self, bufr):
        got_a_msg = False
        # Grab the next message
        while (bufr.advance() == 0):
            # Skip this message if not the desired type
            if (re.search(self.mtype_re, bufr.msg_type)):
                # Keep count of the messages that match the desired type, which
                # is needed to do the selection filtering.
                self.num_msg_mtype += 1

                # Apply the filtering. Default is to take all messages
                Select = True

                # If the max_num_msg parameter is greater than zero, then use
                # it to limit the number of messages that are selected.
                if (self.max_num_msg > 0):
                    Select = (self.num_msg_selected < self.max_num_msg)

                # If the thinning interval is greater than 1, then use it to
                # further select every n-th message.
                if (self.thin_interval > 1):
                    Select = Select and (
                        (self.num_msg_mtype % self.thin_interval) == 0)

                # If Select is true, the current message has been selected.
                # Keep track of how many messages have been selected, plus
                # break out of the loop and return.
                if (Select):
                    self.num_msg_selected += 1
                    got_a_msg = True
                    break

        return got_a_msg

    ##########################################################################
    # This method will convert the BUFR data into netcdf data. This includes
    # reading BUFR and writing netcdf. This method represents a default that
    # can be used for (hopefully) many obs types. If an obs type requires a
    # more complex method, then this one can be overridden in a derived class.
    #
    # The default method provides the following:
    #     Copy all BUFR mnemonic values in the variable specs to the output
    #         netcdf file.
    #     Calculate a time offset from the reference time and store in addition
    #         to the BUFR mnemonic values
    def convert(self, bufr, nc, isProfile=False):
        # Walk through the messages, selecting only those match the regular
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
                # dictionaries where each dictionary represents one
                # observation. A dictionary within the list is keyed by the
                # netcdf variable name and contains the associated data value.
                [ActualValues, ActualValuesBufr] = self.extract_bufr(bufr)
                if isProfile:
                    maxLength = 1
                    for k in ActualValues[0].keys():
                        if len(ActualValues[0][k].shape) >= 1 and \
                           ActualValues[0][k].shape[0] > maxLength:
                            maxLength = ActualValues[0][k].shape[0]

                for i in range(len(ActualValues)):
                    # Put the message type, message date and datetime
                    # into the dictionary.
                    ActualValues[i]['msg_type@MetaData'] = MsgType
                    ActualValues[i]['msg_date@MetaData'] = MsgDate
                    [ActualValues[i]['datetime@MetaData']] = self.calc_obs_date_time(ActualValuesBufr[i])

                    # Write out the netcdf variables.

                    for Vname, Vdata in ActualValues[i].items():
                        if isProfile:
                            if Vdata.shape == ():
                                try:
                                    Vdata = np.ma.array(maxLength*[Vdata],
                                                        dtype=Vdata.dtype)
                                except Exception:
                                    pass
                                Vdata = Vdata.squeeze()
                        # Skip the write if Vdata is empty
                        if Vdata.size:
                            WriteNcVar(nc, ObsNum, Vname, Vdata, isProfile)

                    # Increment observation number and print out progress
                    # messages.
                    if isProfile:
                        ObsNum += maxLength
                    else:
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
