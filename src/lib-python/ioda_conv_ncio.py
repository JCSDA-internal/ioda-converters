#!/usr/bin/env python

from __future__ import print_function
import numpy as np
import netCDF4
from netCDF4 import Dataset

###################################################################################
# Global Variables
###################################################################################

# Output format
OUT_NC_VERSION = 1   # 1 - all variables form a list of vectors in the top
                     #     level group
                     # 2 - groups for meta data, 2D tables for obs data, etc.

OUT_NC_1_DESCRIP = "Each variable is a vector, all variables in top-level group"
OUT_NC_2_DESCRIP = "Obs data are 2D arrays, metatdata are vectors in subgroups"

# Names assigned to obs values, error estimates and qc marks
OVAL_NAME = "ObsValue"
OERR_NAME = "ObsError"
OQC_NAME  = "PreQc"

# Names assigned to record metadata
REC_ID_NAME = "station_id"
REC_DT_NAME = "analysis_date_time"

# Names assigned to dimensions
NRECS_DIM_NAME = 'nrecs'
NLOCS_DIM_NAME = 'nlocs'
NVARS_DIM_NAME = 'nvars'
NSTR_DIM_NAME  = 'nstring'

# Names assigned to location metadata
LON_NAME     = "longitude"
LAT_NAME     = "latitude"
P_NAME       = "air_pressure"
DT_NAME      = "date_time"
REC_NUM_NAME = "record_number"

# Names assigned to variable metadata
VLIST_NAME = "variable_names"

# Names for metadata groups
REC_MD_NAME = "RecMetaData"
LOC_MD_NAME = "LocMetaData"
VAR_MD_NAME = "VarMetaData"

# Misc
MAX_STR_LEN = 20

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
# SUBROUTINES
###################################################################################

def WriteNcAttr(Fid, AttrData, Nrecs, Nvars, Nlocs):
    ############################################################
    # This method will dump out the netcdf global attribute data
    # contained in the dictionary AttrData.

    Fid.setncattr(NRECS_DIM_NAME, Nrecs)
    Fid.setncattr(NVARS_DIM_NAME, Nvars)
    Fid.setncattr(NLOCS_DIM_NAME, Nlocs)
    for Aname, Aval in AttrData.items():
        Fid.setncattr(Aname, Aval)

def NumpyToNcDtype(NumpyDtype):
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

def WriteNcObsVars(Fid, Nvars, Nlocs, Nrecs, ObsVars, VarMdata):
    ############################################################
    # This method will create dimensions and variables in the
    # output netcdf file for the obs variables.

    # Dimensions used by any group can be placed at in the top
    # level (root) group. This is convenient if we decide to
    # rearrange the group structure.
    Fid.createDimension(NVARS_DIM_NAME, Nvars)
    Fid.createDimension(NLOCS_DIM_NAME, Nlocs)
    Fid.createDimension(NRECS_DIM_NAME, Nrecs)
    Fid.createDimension(NSTR_DIM_NAME, MAX_STR_LEN)

    if (OUT_NC_VERSION == 1):
        for Gname, Vvals in ObsVars.items():
            for i in range(Nvars):
                # The rstrip trims off the null bytes that correspond to '' chars
                # in the character array.
                Vname = VarMdata['variable_names'][i,:].tostring().rstrip('\x00')
                NcVname = "{0:s}@{1:s}".format(Vname, Gname)
                NcVvals = Vvals[i,:]

                Fid.createVariable(NcVname, NumpyToNcDtype(NcVvals.dtype), (NLOCS_DIM_NAME))
                Fid[NcVname][:] = NcVvals

    elif (OUT_NC_VERSION == 2):
        for Vname, Vvals in ObsVars.items():
            Fid.createVariable(Vname, NumpyToNcDtype(Vvals.dtype), (NVARS_DIM_NAME, NLOCS_DIM_NAME))
            Fid[Vname][:] = Vvals

def WriteNcMetadata(Fid, MdataGroup, DimName, Mdata):
    ############################################################
    # This method will create variables in the output netcdf
    # file for the given metadata group.

    if (OUT_NC_VERSION == 1):
        if (MdataGroup == LOC_MD_NAME):
            Gname = "MetaData"
        else:
            Gname = MdataGroup

        for Vname, Vvals in Mdata.items():
            NcVname = "{0:s}@{1:s}".format(Vname, Gname)
            NcDtype = NumpyToNcDtype(Vvals.dtype)
            if (NcDtype == 'c'):
                Fid.createVariable(NcVname, NcDtype, (DimName, NSTR_DIM_NAME))
            else:
                Fid.createVariable(NcVname, NcDtype, (DimName))
       
            Fid[NcVname][:] = Vvals

    elif (OUT_NC_VERSION == 2):
        MdGroup = Fid.createGroup(MdataGroup)

        for Vname, Vvals in Mdata.items():
            NcDtype = NumpyToNcDtype(Vvals.dtype)
            if (NcDtype == 'c'):
                MdGroup.createVariable(Vname, NcDtype, (DimName, NSTR_DIM_NAME))
            else:
                MdGroup.createVariable(Vname, NcDtype, (DimName))
       
            MdGroup[Vname][:] = Vvals

def ExtractObsData(ObsData):
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
                if (VarKey[1] == OVAL_NAME):
                    VarNames.add(VarKey[0])

    VarList = list(VarNames)
    Nvars = len(VarList)

    VarMap = { }
    for i in range(Nvars):
        VarMap[VarList[i]] = i

    # Preallocate arrays and fill them up with data from the dictionary
    ObsVars = {
        OVAL_NAME  : np.zeros((Nvars, Nlocs), dtype='f4'),
        OERR_NAME  : np.zeros((Nvars, Nlocs), dtype='f4'),
        OQC_NAME   : np.zeros((Nvars, Nlocs), dtype='i4'),
        }

    LocMdata = {
        LON_NAME     : np.zeros((Nlocs), dtype='f4'),
        LAT_NAME     : np.zeros((Nlocs), dtype='f4'),
        P_NAME       : np.zeros((Nlocs), dtype='f4'),
        DT_NAME      : np.chararray((Nlocs, MAX_STR_LEN)),
        REC_NUM_NAME : np.zeros((Nlocs), dtype='i4')
        }

    VarMdata = {
        VLIST_NAME : np.chararray((Nvars, MAX_STR_LEN)),
        }

    RecMdata = {
        REC_ID_NAME : np.chararray((Nrecs, MAX_STR_LEN)),
        REC_DT_NAME : np.chararray((Nrecs, MAX_STR_LEN))
        }


    StringType = "S{0:d}".format(MAX_STR_LEN)
    VarMdata[VLIST_NAME] = netCDF4.stringtochar(np.array(VarList, StringType))

    RecNum = 0
    LocNum = 0
    for RecKey, RecDict in ObsData.items():
        RecNum += 1

        # Exract record metadata encoded in the keys
        RecMdata[REC_ID_NAME][RecNum-1] = netCDF4.stringtochar(np.array(RecKey[0], StringType))
        RecMdata[REC_DT_NAME][RecNum-1] = netCDF4.stringtochar(np.array(RecKey[1], StringType))

        for LocKey, LocDict in RecDict.items():
            LocNum += 1

            # Extract the locations metadata encoded in the keys
            LocMdata[LAT_NAME][LocNum-1] = LocKey[0]
            LocMdata[LON_NAME][LocNum-1] = LocKey[1]
            LocMdata[P_NAME][LocNum-1]   = LocKey[2]
            LocMdata[DT_NAME][LocNum-1]  = netCDF4.stringtochar(np.array(LocKey[3], StringType))

            LocMdata[REC_NUM_NAME][LocNum-1]  = RecNum

            for VarKey, VarVal in LocDict.items():
                VarNum = VarMap[VarKey[0]]
                if (VarKey[1] == OVAL_NAME):
                    ObsVars[OVAL_NAME][VarNum, LocNum-1] = VarVal
                elif (VarKey[1] == OERR_NAME):
                    ObsVars[OERR_NAME][VarNum, LocNum-1] = VarVal
                elif (VarKey[1] == OQC_NAME):
                    ObsVars[OQC_NAME][VarNum, LocNum-1] = VarVal

    return (Nrecs, Nvars, Nlocs, ObsVars, RecMdata, LocMdata, VarMdata)

def BuildNetcdf(NcFname, ObsData, AttrData):
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
    if (OUT_NC_VERSION == 1):
       print("BuildNetcdf: Writing output netcdf version 1")
       print("  ", OUT_NC_1_DESCRIP)
    elif (OUT_NC_VERSION == 2):
       print("BuildNetcdf: Writing output netcdf version 2")
       print("  ", OUT_NC_2_DESCRIP)
    else:
       print("ERROR: Undefined version number output netcdf file: ", OUT_NC_VERSION)
       print("ERROR:   Recognized version numbers:")
       print("ERROR:     1:", OUT_NC_1_DESCRIP) 
       print("ERROR:     2:", OUT_NC_2_DESCRIP)
       exit(-1)
    print("")

    # The structure of the input ObsData dictionary is:
    #     ObsData[Record][Location][Variable]
    #
    # Each entry has a single value; either a float, integer or string.
    # We want to write the data into the file as arrays (nvars X nlocs)
    # or vectors (nlocs) so we need to reformat ObsData to make it
    # convenient to write the file.
    (Nrecs, Nvars, Nlocs, ObsVars, RecMdata, LocMdata, VarMdata) = ExtractObsData(ObsData)

    # Open the output file, write out the global attributes and then
    # the different data sections: Obs variables, Record metadata,
    # locations metadata and variable metadata.
    Fid = Dataset(NcFname, 'w', format='NETCDF4')

    WriteNcAttr(Fid, AttrData, Nrecs, Nvars, Nlocs)

    WriteNcObsVars(Fid, Nvars, Nlocs, Nrecs, ObsVars, VarMdata)

    WriteNcMetadata(Fid, REC_MD_NAME, NRECS_DIM_NAME, RecMdata)
    WriteNcMetadata(Fid, LOC_MD_NAME, NLOCS_DIM_NAME, LocMdata)
    WriteNcMetadata(Fid, VAR_MD_NAME, NVARS_DIM_NAME, VarMdata)

    Fid.close()
