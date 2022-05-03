#!/usr/bin/env python3

import datetime as dt
import math
import sys
from collections import OrderedDict

import numpy as np
import netCDF4
from netCDF4 import Dataset

###################################################################################
# SUBROUTINES
###################################################################################


def WriteNcVar(Gfid, Ofid, OutDest, Vname, Vdtype, Vdims, Vvals):
    ###############################################################
    # This method will write out the variable into the appropriate
    # netcdf files.
    #
    if (OutDest == 'G' or OutDest == 'B'):
        # write to geo file
        Ovar = Gfid.createVariable(Vname, Vdtype, Vdims)
        Ovar[...] = Vvals[...]

    if (OutDest == 'O' or OutDest == 'B'):
        # write to obs file
        Ovar = Ofid.createVariable(Vname, Vdtype, Vdims)
        Ovar[...] = Vvals[...]


def CharVectorToString(CharVec):
    # Need to do this differently given the Python version (2 vs 3)
    #
    # sys.version_info[0] yeilds the Python major version number.
    if (sys.version_info[0] == 2):
        String = CharVec.tostring().rstrip('\x00')
    else:
        String = str(CharVec.tostring(), 'utf-8').rstrip('\x00')

    return String


###################################################################################
# CLASSES
###################################################################################

class NcWriter(object):
    # Constructor
    def __init__(self, NcFname, LocKeyList, TestKeyList=None):

        # Variable names of items in the record key

        # Variable names of items in the location key
        self._loc_key_list = LocKeyList

        # Variable names of items in the TestReference key
        self._test_key_list = TestKeyList

        # Names assigned to obs values, error estimates and qc marks
        self._oval_name = "ObsValue"
        self._oerr_name = "ObsError"
        self._oqc_name = "PreQC"

        # Names assigned to obs bias terms and predoctirs related to observations
        self._obiasterm_name = "GsiObsBiasTerm"
        self._obiaspred_name = "GsitObsBiasPredictor"

        # Names assigned to dimensions
        self._nlocs_dim_name = 'nlocs'
        self._nvars_dim_name = 'nvars'
        self._nstr_dim_name = 'nstring'
        self._ndatetime_dim_name = 'ndatetime'

        # Dimension sizes
        self._nlocs = 0
        self._nvars = 0
        self._nstring = 50
        self._ndatetime = 20

        # default fill values
        self._defaultF4 = np.float32(netCDF4.default_fillvals['f4'])
        self._defaultI4 = np.int32(netCDF4.default_fillvals['i4'])

        # Names assigned to record number (location metadata)
        self._rec_num_name = "record_number"

        # Names assigned to variable metadata
        self._var_list_name = "variable_names"

        # Names for metadata groups
        self._loc_md_name = "MetaData"
        self._var_md_name = "VarMetaData"
        self._test_md_name = "TestReference"

        # Reference date time
        self._ref_date_time = dt.datetime(1, 1, 1, 0)

        # Open the netcdf file for writing
        self._fid = Dataset(NcFname, 'w', format='NETCDF4')

    # Destructor
    def __del__(self):
        # Close the netcdf file
        self._fid.close()

    # Methods

    def OvalName(self):
        return self._oval_name

    def ObiastermName(self):
        return self._obiasterm_name

    def ObiaspredName(self):
        return self._obiaspred_name

    def OerrName(self):
        return self._oerr_name

    def OqcName(self):
        return self._oqc_name

    def NumpyToNcDtype(self, NumpyDtype):
        ############################################################
        # This method converts the numpy data type to the
        # corresponding netcdf datatype

        if (NumpyDtype == np.dtype('float64')):
            NcDtype = 'f4'    # convert double to float
        elif (NumpyDtype == np.dtype('float32')):
            NcDtype = 'f4'
        elif (NumpyDtype == np.dtype('int64')):
            NcDtype = 'i4'    # convert long to int
        elif (NumpyDtype == np.dtype('int32')):
            NcDtype = 'i4'
        elif (NumpyDtype == np.dtype('int16')):
            NcDtype = 'i4'
        elif (NumpyDtype == np.dtype('int8')):
            NcDtype = 'i4'
        elif (NumpyDtype == np.dtype('S1')):
            NcDtype = 'c'
        elif (NumpyDtype == np.dtype('U1')):
            NcDtype = 'c'
        elif (NumpyDtype == np.dtype('S32')):
            NcDtype = 'c'
        else:
            print("ERROR: Unrecognized numpy data type: ", NumpyDtype)
            exit(-2)

        return NcDtype

    def NumpyToIodaDtype(self, NumpyDtype):
        ############################################################
        # This method converts the numpy data type to the
        # corresponding ioda datatype

        if (NumpyDtype == np.dtype('float64')):
            IodaDtype = 'float'    # convert double to float
        elif (NumpyDtype == np.dtype('float32')):
            IodaDtype = 'float'
        elif (NumpyDtype == np.dtype('int64')):
            IodaDtype = 'integer'    # convert long to int
        elif (NumpyDtype == np.dtype('int32')):
            IodaDtype = 'integer'
        elif (NumpyDtype == np.dtype('int16')):
            IodaDtype = 'integer'
        elif (NumpyDtype == np.dtype('int8')):
            IodaDtype = 'integer'
        elif (NumpyDtype == np.dtype('S1')):
            IodaDtype = 'string'
        else:
            print("ERROR: Unrecognized numpy data type: ", NumpyDtype)
            exit(-2)

        return IodaDtype

    def ConvertToTimeOffset(self, datestr_vec):
        """
        Convert from date strings in ioda reference format "YYYY-mm-ddTHH:MM:SSZ" to decimal hour offsets from reference datetime.

        Input: datestr_vec is an array of numpy chararrays, representing the raw bytes of the date string
        Output: numpy dtype:'f4' array of time offsets in decimal hours from the reference date.
        """
        offset = np.zeros((self._nlocs), dtype='f4')
        ref_offset = math.floor(self._ref_date_time.hour*3600. + self._ref_date_time.minute*60. + self._ref_date_time.second)/3600.  # decimal hours
        date_offset_cache = {}  # Cache a map from bytes[0:10] to the corresponding date's offset from reference date
        for i in range(len(datestr_vec)):
            date_bytes = datestr_vec[i].tobytes()
            date_offset = date_offset_cache.get(date_bytes[0:10])
            if date_offset is None:
                # There are at most 2 distinct dates per cycle for any window length < 24hrs
                delta = dt.date(int(date_bytes[0:4]), int(date_bytes[5:7]), int(date_bytes[8:10])) - self._ref_date_time.date()
                date_offset = delta.total_seconds()/3600.
                date_offset_cache[date_bytes[0:10]] = date_offset
            t_offset = (int(date_bytes[11:13])*3600. + int(date_bytes[14:16])*60. + int(date_bytes[17:19]))/3600.
            offset[i] = date_offset + (t_offset - ref_offset)
        return offset

    def CreateNcVector(self, Nsize, Dtype):
        ############################################################
        # This method will create a numpy array (vector) of an
        # appropriate type for writing into the output netcdf file.

        if (Dtype == "integer"):
            Vector = np.zeros((Nsize), dtype='i4')
        elif (Dtype == "float"):
            Vector = np.zeros((Nsize), dtype='f4')
        elif (Dtype == "string"):
            Vector = np.chararray((Nsize, self._nstring))
        elif (Dtype == "datetime"):
            Vector = np.chararray((Nsize, self._ndatetime))
        else:
            print("ERROR: Unrecognized vector data type: ", Dtype)
            exit(-3)

        return Vector

    def FillNcVector(self, Values, Dtype):
        ############################################################
        # This method will fill a numpy array (vector) that will
        # be used for writing into the output netcdf file.

        if (Dtype == "integer"):
            Vector = Values
        elif (Dtype == "float"):
            Vector = Values
        elif (Dtype == "string"):
            StringType = "S{0:d}".format(self._nstring)
            s = np.array(Values, StringType)
            try:
                Vector = netCDF4.stringtochar(s)
            except (UnicodeEncodeError, UnicodeDecodeError) as e:
                Vector = netCDF4.stringtochar(s, encoding='bytes')
        elif (Dtype == "datetime"):
            StringType = "S{0:d}".format(self._ndatetime)
            Vector = netCDF4.stringtochar(np.array(Values, StringType))
        else:
            print("ERROR: Unrecognized vector data type: ", Dtype)
            exit(-3)

        return Vector

    def WriteNcAttr(self, AttrData):
        ############################################################
        # This method will dump out the netcdf global attribute data
        # contained in the dictionary AttrData.

        self._fid.setncattr(self._nvars_dim_name, np.int32(self._nvars))
        self._fid.setncattr(self._nlocs_dim_name, np.int32(self._nlocs))

        for Aname, Aval in AttrData.items():
            if (Aname == "date_time_string"):
                # Save the datetime object in this object and convert
                # to integer representation for the netcdf file
                self._ref_date_time = dt.datetime.strptime(Aval, "%Y-%m-%dT%H:%M:%SZ")
                refDateTime = self._ref_date_time.year * 1000000
                refDateTime += self._ref_date_time.month * 10000
                refDateTime += self._ref_date_time.day * 100
                refDateTime += self._ref_date_time.hour
                self._fid.setncattr("date_time", np.int32(refDateTime))
            else:
                self._fid.setncattr(Aname, Aval)

    def WriteNcObsVars(self, ObsVars, VarMdata, VarUnits):
        ############################################################
        # This method will create dimensions and variables in the
        # output netcdf file for the obs variables.

        # Dimensions used by any group can be placed at in the top
        # level (root) group. This is convenient if we decide to
        # rearrange the group structure.
        self._fid.createDimension(self._nvars_dim_name, self._nvars)
        self._fid.createDimension(self._nlocs_dim_name, self._nlocs)
        self._fid.createDimension(self._nstr_dim_name, self._nstring)
        self._fid.createDimension(self._ndatetime_dim_name, self._ndatetime)

        for VarKey, Vvals in ObsVars.items():
            (Vname, Gname) = VarKey
            NcVname = "{0:s}@{1:s}".format(Vname, Gname)

            self._fid.createVariable(NcVname, self.NumpyToNcDtype(Vvals.dtype), (self._nlocs_dim_name))
            self._fid[NcVname][:] = Vvals
            # add units
            if Gname in ['ObsValue', 'ObsError', 'GsiHofX', 'GsiHofXBc',
                         'GsiHofXClr', 'GsiFinalObsError', 'GsiBc', 'GsiBcConst',
                         'GsiBcScanAng', 'GsiAdjustObsError', 'GsiFinalObsError', 'GsiObsBias']:
                try:
                    self._fid[NcVname].setncattr_string("units", VarUnits[Vname])
                except KeyError:
                    pass

    def WriteNcMetadata(self, MdataGroup, DimName, Mdata, VarUnits):
        ############################################################
        # This method will create variables in the output netcdf
        # file for the given metadata group.

        if (MdataGroup == self._loc_md_name):
            # Will be adding the time offset to the location metadata
            ToffsetName = "time@{0:s}".format(MdataGroup)
            self._fid.createVariable(ToffsetName, "f4", (DimName))

        for Vname, Vvals in Mdata.items():
            NcVname = "{0:s}@{1:s}".format(Vname, MdataGroup)
            NcDtype = self.NumpyToNcDtype(Vvals.dtype)
            if (NcDtype == 'c'):
                if (NcVname == "datetime@MetaData"):
                    self._fid.createVariable(NcVname, NcDtype, (DimName, self._ndatetime_dim_name))
                else:
                    self._fid.createVariable(NcVname, NcDtype, (DimName, self._nstr_dim_name))
            else:
                self._fid.createVariable(NcVname, NcDtype, (DimName))

            self._fid[NcVname][:] = Vvals
            # add units
            try:
                self._fid[NcVname].setncattr_string("units", VarUnits[Vname])
            except KeyError:
                pass

            # If we are writing out the datetime string, then we also
            # need to write out the time offset (from the reference date_time
            # attribute value).
            if (NcVname == "datetime@MetaData"):
                ToffsetValues = self.ConvertToTimeOffset(Vvals)
                self._fid[ToffsetName][:] = ToffsetValues
                # self._fid[ToffsetName].setncattr_string("Units", "hours since "+self._ref_date_time.strftime("%Y-%m-%d %H:%M:%S")+" UTC")

    def ExtractObsData(self, ObsData):
        ############################################################
        # This method will extract information from the ObsData
        # dictionary and reformat it into a more amenable form for
        # writing into the output file.

        self._nvars = 0
        self._nlocs = 0

        VarNames = set()

        # Walk through the structure and get counts so arrays
        # can be preallocated, and variable numbers can be assigned
        ObsVarList = []
        ObsVarExamples = []
        VMName = []
        VMData = {}
        nrecs = 0
        for RecKey, RecDict in ObsData.items():
            nrecs += 1
            for LocKey, LocDict in RecDict.items():
                self._nlocs += 1
                for VarKey, VarVal in LocDict.items():
                    if (VarKey[1] == self._oval_name):
                        VarNames.add(VarKey[0])
                    if (VarKey not in ObsVarList):
                        ObsVarList.append(VarKey)
                        ObsVarExamples.append(VarVal)
        VarList = sorted(list(VarNames))
        self._nvars = len(VarList)

        # Preallocate arrays and fill them up with data from the dictionary
        ObsVars = OrderedDict()
        for o in range(len(ObsVarList)):
            VarType = type(ObsVarExamples[o])
            if (VarType in [float, np.float32, np.float64]):
                defaultval = self._defaultF4
            elif (VarType in [int, np.int64, np.int32, np.int8]):
                defaultval = self._defaultI4    # convert long to int
            elif (VarType in [str, np.str_]):
                defaultval = ''
            elif (VarType in [np.ma.core.MaskedConstant]):
                # If we happened to pick an invalid value (inf, nan, etc.) from
                # a masked array, then the type is a MaskedConstant, and that implies
                # floating point values.
                defaultval = self._defaultF4
            else:
                print('Warning: VarType', VarType, ' not in float, int, str for:', ObsVarList[o])
                continue
            ObsVars[ObsVarList[o]] = np.full((self._nlocs), defaultval, dtype=defaultval.dtype)

        LocMdata = OrderedDict()
        for i in range(len(self._loc_key_list)):
            (LocVname, LocVtype) = self._loc_key_list[i]
            # if datetime was specified as as a "string" override to
            # define it as a "datetime"
            if LocVname == "datetime":
                LocVtype = "datetime"
            LocMdata[LocVname] = self.CreateNcVector(self._nlocs, LocVtype)
        LocMdata[self._rec_num_name] = self.CreateNcVector(self._nlocs, "integer")

        VarMdata = OrderedDict()
        VarMdata[self._var_list_name] = self.CreateNcVector(self._nvars, "string")
        VarMdata[self._var_list_name] = self.FillNcVector(VarList, "string")

        RecNum = 0
        LocNum = 0
        for RecKey, RecDict in ObsData.items():
            RecNum += 1

            # Exract record metadata encoded in the keys
            for LocKey, LocDict in RecDict.items():
                LocNum += 1

                # Extract the locations metadata encoded in the keys
                for i in range(len(self._loc_key_list)):
                    (LocVname, LocVtype) = self._loc_key_list[i]

                    # if datetime was specified as as a "string" override to
                    # define it as a "datetime"
                    if LocVname == "datetime":
                        LocVtype = "datetime"

                    LocMdata[LocVname][LocNum-1] = self.FillNcVector(LocKey[i], LocVtype)
                LocMdata[self._rec_num_name][LocNum-1] = RecNum

                for VarKey, VarVal in LocDict.items():
                    if (type(VarVal) in [np.ma.core.MaskedConstant]):
                        VarVal = self._defaultF4
                    ObsVars[VarKey][LocNum-1] = VarVal

        return (ObsVars, LocMdata, VarMdata)

    def BuildNetcdf(self, ObsVars, LocMdata, VarMdata, AttrData, VarUnits={}, TestData=None):
        ############################################################
        # This method will create an output netcdf file, and dump
        # the contents of the ObsData dictionary into that file.
        #
        # The structure of the output file is:
        #
        #  global attributes:
        #     nvars
        #     nlocs
        #     contents of AttrData dictionary
        #
        #  group, variable structure:
        #     /ObsValue
        #         2D float array, nvars X nlocs
        #         Observation values
        #     /ObsError
        #         2D float array, nvars X nlocs
        #         Observation errors
        #     /PreQC
        #         2D integer array, nvars X nlocs
        #         Observation QC marks
        #     /VarMetaData/
        #         Group holding 1D vectors of various data types, nvars
        #         channel_number
        #         channel_frequency
        #         etc.
        #     /LocMetaData/
        #         Group holding 1D vectors of various data types, nlocs
        #         latitude
        #         longitude
        #         datetime
        #     /VarUnits/
        #         Dictionary holding string of units for each variable/metadata item
        #         Key: variable, Value: unit_string
        #         Example: VarUnits['air_temperature'] = 'K'
        #     /TestData/
        #         Group holding 1D vectors of various data types, nlocs
        #         cloud_liquid_water_column_retrieved_from_observations for example

        # Write out the global attributes followed by the different
        # data sections: Obs variables, Record metadata, locations metadata
        # and variable metadata.
        self.WriteNcAttr(AttrData)
        self.WriteNcObsVars(ObsVars, VarMdata, VarUnits)

        self.WriteNcMetadata(self._loc_md_name, self._nlocs_dim_name, LocMdata, VarUnits)
        self.WriteNcMetadata(self._var_md_name, self._nvars_dim_name, VarMdata, VarUnits)
        if TestData is not None:
            self.WriteNcMetadata(self._test_md_name, self._nlocs_dim_name, TestData, VarUnits)
