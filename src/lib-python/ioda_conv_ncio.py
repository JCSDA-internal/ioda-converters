#!/usr/bin/env python

from __future__ import print_function
import numpy as np
import netCDF4
from netCDF4 import Dataset

###################################################################################
# SUBROUTINES (soon to be obsoleted)
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


###################################################################################
# CLASSES
###################################################################################


class NcWriter(object):
    #### Constructor ####
    def __init__(self, NcFname, RecKeyList, LocKeyList):

        # Variable names of items in the record key
        self._rec_key_list = RecKeyList

        # Variable names of items in the location key
        self._loc_key_list = LocKeyList

        # Output format
        self._out_nc_version = 1   # 1 - all variables form a list of vectors in the top
                                  #     level group
                                  # 2 - groups for meta data, 2D tables for obs data, etc.

        self._out_nc_1_descrip = "Each variable is a vector, all variables in top-level group"
        self._out_nc_2_descrip = "Obs data are 2D arrays, metatdata are vectors in subgroups"

        # Names assigned to obs values, error estimates and qc marks
        self._oval_name = "ObsValue"
        self._oerr_name = "ObsError"
        self._oqc_name  = "PreQc"

        # Names assigned to dimensions
        self._nrecs_dim_name = 'nrecs'
        self._nlocs_dim_name = 'nlocs'
        self._nvars_dim_name = 'nvars'
        self._nstr_dim_name  = 'nstring'

        # Names assigned to record number (location metadata)
        self._rec_num_name = "record_number"

        # Names assigned to variable metadata
        self._var_list_name = "variable_names"

        # Names for metadata groups
        self._rec_md_name = "RecMetaData"
        self._loc_md_name = "LocMetaData"
        self._var_md_name = "VarMetaData"

        # Misc
        self._max_str_len = 20

        # Open the netcdf file for writing
        self._fid = Dataset(NcFname, 'w', format='NETCDF4')

    #### Destructor ####
    def __del__(self):
        # Close the netcdf file
        self._fid.close()

    #### Methods ####

    def OvalName(self):
        return self._oval_name

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
        elif (NumpyDtype == np.dtype('S1')):
            NcDtype = 'c'
        else:
            print("ERROR: Unrecognized numpy data type: ", NumpyDtype)
            exit(-2)

        return NcDtype

    def CreateNcVector(self, Nsize, Dtype):
        ############################################################
        # This method will create a numpy array (vector) of an
        # appropriate type for writing into the output netcdf file.

        if (Dtype == "integer"):
            Vector = np.zeros((Nsize), dtype='i4')
        elif (Dtype == "float"):
            Vector = np.zeros((Nsize), dtype='f4')
        elif (Dtype == "string"):
            Vector = np.chararray((Nsize, self._max_str_len))
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
            StringType = "S{0:d}".format(self._max_str_len)
            Vector = netCDF4.stringtochar(np.array(Values, StringType))
        else:
            print("ERROR: Unrecognized vector data type: ", Dtype)
            exit(-3)

        return Vector

    def WriteNcAttr(self, AttrData, Nrecs, Nvars, Nlocs):
        ############################################################
        # This method will dump out the netcdf global attribute data
        # contained in the dictionary AttrData.

        self._fid.setncattr(self._nrecs_dim_name, Nrecs)
        self._fid.setncattr(self._nvars_dim_name, Nvars)
        self._fid.setncattr(self._nlocs_dim_name, Nlocs)
        for Aname, Aval in AttrData.items():
            self._fid.setncattr(Aname, Aval)

    def WriteNcObsVars(self, Nvars, Nlocs, Nrecs, ObsVars, VarMdata):
        ############################################################
        # This method will create dimensions and variables in the
        # output netcdf file for the obs variables.

        # Dimensions used by any group can be placed at in the top
        # level (root) group. This is convenient if we decide to
        # rearrange the group structure.
        self._fid.createDimension(self._nvars_dim_name, Nvars)
        self._fid.createDimension(self._nlocs_dim_name, Nlocs)
        self._fid.createDimension(self._nrecs_dim_name, Nrecs)
        self._fid.createDimension(self._nstr_dim_name, self._max_str_len)

        if (self._out_nc_version == 1):
            for Gname, Vvals in ObsVars.items():
                for i in range(Nvars):
                    # The rstrip trims off the null bytes that correspond to '' chars
                    # in the character array.
                    Vname = VarMdata['variable_names'][i,:].tostring().rstrip('\x00')
                    NcVname = "{0:s}@{1:s}".format(Vname, Gname)
                    NcVvals = Vvals[i,:]

                    self._fid.createVariable(NcVname, self.NumpyToNcDtype(NcVvals.dtype), (self._nlocs_dim_name))
                    self._fid[NcVname][:] = NcVvals

        elif (self._out_nc_version == 2):
            for Vname, Vvals in ObsVars.items():
                self._fid.createVariable(Vname, self.NumpyToNcDtype(Vvals.dtype), (self._nvars_dim_name, self._nlocs_dim_name))
                self._fid[Vname][:] = Vvals

    def WriteNcMetadata(self, MdataGroup, DimName, Mdata):
        ############################################################
        # This method will create variables in the output netcdf
        # file for the given metadata group.

        if (self._out_nc_version == 1):
            if (MdataGroup == self._loc_md_name):
                Gname = "MetaData"
            else:
                Gname = MdataGroup

            for Vname, Vvals in Mdata.items():
                NcVname = "{0:s}@{1:s}".format(Vname, Gname)
                NcDtype = self.NumpyToNcDtype(Vvals.dtype)
                if (NcDtype == 'c'):
                    self._fid.createVariable(NcVname, NcDtype, (DimName, self._nstr_dim_name))
                else:
                    self._fid.createVariable(NcVname, NcDtype, (DimName))

                self._fid[NcVname][:] = Vvals

        elif (self._out_nc_version == 2):
            MdGroup = self._fid.createGroup(MdataGroup)

            for Vname, Vvals in Mdata.items():
                NcDtype = self.NumpyToNcDtype(Vvals.dtype)
                if (NcDtype == 'c'):
                    MdGroup.createVariable(Vname, NcDtype, (DimName, self._nstr_dim_name))
                else:
                    MdGroup.createVariable(Vname, NcDtype, (DimName))

                MdGroup[Vname][:] = Vvals

    def ExtractObsData(self, ObsData):
        ############################################################
        # This method will extract information from the ObsData
        # dictionary and reformat it into a more amenable form for
        # writing into the output file.

        Nrecs = 0
        Nvars = 0
        Nlocs = 0

        VarNames = set()

        # Walk through the structure and get counts so arrays
        # can be preallocated, and variable numbers can be assigned
        for RecKey, RecDict in ObsData.items():
            Nrecs += 1
            for LocKey, LocDict in RecDict.items():
                Nlocs += 1
                for VarKey, VarVal in LocDict.items():
                    if (VarKey[1] == self._oval_name):
                        VarNames.add(VarKey[0])

        VarList = list(VarNames)
        Nvars = len(VarList)

        VarMap = { }
        for i in range(Nvars):
            VarMap[VarList[i]] = i

        # Preallocate arrays and fill them up with data from the dictionary
        ObsVars = {
            self._oval_name  : np.zeros((Nvars, Nlocs), dtype='f4'),
            self._oerr_name  : np.zeros((Nvars, Nlocs), dtype='f4'),
            self._oqc_name   : np.zeros((Nvars, Nlocs), dtype='i4'),
            }

        LocMdata = { }
        for i in range(len(self._loc_key_list)):
            (LocVname, LocVtype) = self._loc_key_list[i]
            LocMdata[LocVname] = self.CreateNcVector(Nlocs, LocVtype)
        LocMdata[self._rec_num_name] = self.CreateNcVector(Nlocs, "integer")

        VarMdata = { }
        VarMdata[self._var_list_name] = self.CreateNcVector(Nvars, "string")

        RecMdata = { }
        for i in range(len(self._rec_key_list)):
            (RecVname, RecVtype) = self._rec_key_list[i]
            RecMdata[RecVname] = self.CreateNcVector(Nrecs, RecVtype)

        VarMdata[self._var_list_name] = self.FillNcVector(VarList, "string")

        RecNum = 0
        LocNum = 0
        for RecKey, RecDict in ObsData.items():
            RecNum += 1

            # Exract record metadata encoded in the keys
            for i in range(len(self._rec_key_list)):
                (RecVname, RecVtype) = self._rec_key_list[i]
                RecMdata[RecVname][RecNum-1] = self.FillNcVector(RecKey[i], RecVtype)

            for LocKey, LocDict in RecDict.items():
                LocNum += 1

                # Extract the locations metadata encoded in the keys
                for i in range(len(self._loc_key_list)):
                    (LocVname, LocVtype) = self._loc_key_list[i]
                    LocMdata[LocVname][LocNum-1] = self.FillNcVector(LocKey[i], LocVtype)
                LocMdata[self._rec_num_name][LocNum-1] = RecNum

                for VarKey, VarVal in LocDict.items():
                    VarNum = VarMap[VarKey[0]]
                    if (VarKey[1] == self._oval_name):
                        ObsVars[self._oval_name][VarNum, LocNum-1] = VarVal
                    elif (VarKey[1] == self._oerr_name):
                        ObsVars[self._oerr_name][VarNum, LocNum-1] = VarVal
                    elif (VarKey[1] == self._oqc_name):
                        ObsVars[self._oqc_name][VarNum, LocNum-1] = VarVal

        return (Nrecs, Nvars, Nlocs, ObsVars, RecMdata, LocMdata, VarMdata)

    def BuildNetcdf(self, ObsData, AttrData):
        ############################################################
        # This method will create an output netcdf file, and dump
        # the contents of the ObsData dictionary into that file.
        #
        # The structure of the output file is:
        #
        #  global attributes:
        #     nrecs
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
        #     /PreQc
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
        #         date_time
        #     /RecMetaData/
        #         Group holding 1D vectors of various data types, nrecs
        #         station_id
        #         analysis_date_time

        # Check the output format setting
        if (self._out_nc_version == 1):
           print("BuildNetcdf: Writing output netcdf version 1")
           print("  ", self._out_nc_1_descrip)
        elif (self._out_nc_version == 2):
           print("BuildNetcdf: Writing output netcdf version 2")
           print("  ", self._out_nc_2_descrip)
        else:
           print("ERROR: Undefined version number output netcdf file: ", self._out_nc_version)
           print("ERROR:   Recognized version numbers:")
           print("ERROR:     1:", self._out_nc_1_descrip)
           print("ERROR:     2:", self._out_nc_2_descrip)
           exit(-1)
        print("")

        # The structure of the input ObsData dictionary is:
        #     ObsData[Record][Location][Variable]
        #
        # Each entry has a single value; either a float, integer or string.
        # We want to write the data into the file as arrays (nvars X nlocs)
        # or vectors (nlocs) so we need to reformat ObsData to make it
        # convenient to write the file.
        (Nrecs, Nvars, Nlocs, ObsVars, RecMdata, LocMdata, VarMdata) = self.ExtractObsData(ObsData)

        # Write out the global attributes followed by the different
        # data sections: Obs variables, Record metadata, locations metadata
        # and variable metadata.
        self.WriteNcAttr(AttrData, Nrecs, Nvars, Nlocs)

        self.WriteNcObsVars(Nvars, Nlocs, Nrecs, ObsVars, VarMdata)

        self.WriteNcMetadata(self._rec_md_name, self._nrecs_dim_name, RecMdata)
        self.WriteNcMetadata(self._loc_md_name, self._nlocs_dim_name, LocMdata)
        self.WriteNcMetadata(self._var_md_name, self._nvars_dim_name, VarMdata)
