#!/usr/bin/env python
import datetime as dt
import ioda_obs_space as ioda_os
import numpy as np
from collections import OrderedDict

# define vars
_metagroup = 'MetaData'
# Names assigned to obs values, error estimates and qc marks
_oval_name = "ObsValue"
_oerr_name = "ObsError"
_oqc_name = "PreQC"
# Names assigned to obs bias terms and predictors related to observations
_obiasterm_name = "GsiObsBiasTerm"
_obiaspred_name = "GsitObsBiasPredictor"


def MetaDataName():
    return _metagroup


def OvalName():
    return _oval_name


def ObiastermName():
    return _obiasterm_name


def ObiaspredName():
    return _obiaspred_name


def OerrName():
    return _oerr_name


def OqcName():
    return _oqc_name


def get_default_fill_val(mydtype):
    dtype_tmp = np.array([], dtype=mydtype)
    NumpyDtype = dtype_tmp.dtype
    if (NumpyDtype == np.dtype('float64')):
        fillval = 9.969209968386869e+36
    elif (NumpyDtype == np.dtype('float32')):
        fillval = 9.969209968386869e+36
    elif (NumpyDtype == np.dtype('int64')):
        fillval = -9223372036854775806
    elif (NumpyDtype == np.dtype('int32')):
        fillval = -2147483647
    elif (NumpyDtype == np.dtype('int16')):
        fillval = -32767
    elif (NumpyDtype == np.dtype('int8')):
        fillval = -127
    elif (NumpyDtype == np.dtype('S1')):
        fillval = '\x00'
    elif (NumpyDtype == np.dtype('U1')):
        fillval = '\x00'
    elif (NumpyDtype == np.dtype('object')):
        fillval = '\x00'
    else:
        print("ERROR: Unrecognized data type", NumpyDtype)
        exit(-2)
    return fillval


_default_units = {
    'latitude': 'degrees_north',
    'longitude': 'degrees_east',
}

_defaultF4 = 9.969209968386869e+36


class IodaWriter(object):
    # Constructor
    def __init__(self, Fname, LocKeyList, DimDict, TestKeyList=None):
        # note: loc_key_list does nothing
        self._loc_key_list = LocKeyList
        self._dim_dict = DimDict
        self._test_key_list = TestKeyList
        # open IODA obs backend
        self.obsspace = ioda_os.ObsSpace(Fname, mode='w', dim_dict=DimDict)

    def WriteGeoVars(self, GeoVars, GeoVarDims, GeoVarAttrs):
        # this method will write out geovals using IODA
        for VarName, Vvals in GeoVars.items():
            # get variable and group names
            # create variable
            if VarName in GeoVarDims.keys():
                dims = GeoVarDims[VarName]
            else:
                # assume it is just nlocs
                dims = ['Location']
            fillval = get_default_fill_val(Vvals.dtype)
            # get fill value
            if VarName in GeoVarAttrs.keys():
                if '_FillValue' in GeoVarAttrs[VarName].keys():
                    fillval = GeoVarAttrs[VarName]['_FillValue']
            self.obsspace.create_var(VarName,
                                     dtype=Vvals.dtype,
                                     dim_list=dims,
                                     fillval=fillval,
                                     )
            # write the data to the file
            tmpVar = self.obsspace.Variable(VarName)
            tmpVar.write_data(Vvals)

    def WriteObsVars(self, ObsVars, VarDims, VarAttrs):
        # this method will create variables in the ouput obs group and
        # fill them with the provided data and metadata
        for VarKey, Vvals in ObsVars.items():
            # get variable and group names
            (Vname, Gname) = VarKey
            VarName = "{0:s}/{1:s}".format(Gname, Vname)
            # create variable
            if VarKey in VarDims.keys():
                dims = VarDims[VarKey]
            elif Vname in VarDims.keys():
                dims = VarDims[Vname]
            else:
                # assume it is just nlocs
                dims = ['Location']
            fillval = get_default_fill_val(Vvals.dtype)
            # get fill value
            if VarKey in VarAttrs.keys():
                if '_FillValue' in VarAttrs[VarKey].keys():
                    fillval = VarAttrs[VarKey]['_FillValue']
            self.obsspace.create_var(VarName,
                                     dtype=Vvals.dtype,
                                     dim_list=dims,
                                     fillval=fillval,
                                     )
            # write the data to the file
            tmpVar = self.obsspace.Variable(VarName)
            tmpVar.write_data(Vvals)

            # add some default metadata if necessary
            if Vname in _default_units.keys():
                if 'units' not in VarAttrs[VarKey].keys():
                    VarAttrs[VarKey]['units'] = _default_units[Vname]

            # add var metadata
            try:
                for MetaVar, MetaVal in VarAttrs[VarKey].items():
                    if MetaVar not in ['_FillValue']:
                        tmpVar.write_attr(MetaVar, MetaVal)
            except KeyError:
                pass  # no metadata for this variable

    def VerifyDateTime(self, ObsVars):
        # this method will check if the variable
        # MetaData/dateTime is a string or datetime object
        # if string, convert to datetime object
        VarKey = ('dateTime', 'MetaData')
        if VarKey not in ObsVars.keys():
            raise KeyError("Required variable 'MetaData/dateTime' does not exist.")
        dtvar = ObsVars[VarKey]
        # check if the array is type 'object' or not
        # otherwise we will assume it is an integer and already set up
        if (dtvar.dtype == np.dtype('object')):
            # object is used for strings or datetime objects
            if (isinstance(dtvar[0], str)):
                # convert ISO date strings to datetime objects
                newdtvar = [dt.datetime.strptime(x, "%Y-%m-%dT%H:%M:%SZ") for x in dtvar]
                ObsVars[VarKey] = np.array(newdtvar, dtype=object)

        return ObsVars

    def WriteGlobalAttrs(self, GlobalAttrs):
        # this method will create global attributes from GlobalAttrs dictionary
        for AttrKey, AttrVal in GlobalAttrs.items():
            self.obsspace.write_attr(AttrKey, AttrVal)

    def BuildIoda(self, ObsVars, VarDims, VarAttrs, GlobalAttrs,
                  TestData=None, geovals=False):
        # check and fix dateTime if necessary
        ObsVars = self.VerifyDateTime(ObsVars)
        if geovals:
            self.WriteGeoVars(ObsVars, VarDims, VarAttrs)
        else:
            self.WriteObsVars(ObsVars, VarDims, VarAttrs)
        self.WriteGlobalAttrs(GlobalAttrs)


def ExtractObsData(ObsData, loc_key_list):
    ############################################################
    # This method will extract information from the ObsData
    # dictionary and reformat it into a more amenable form for
    # writing into the output file.

    _nlocs = 0

    VarNames = set()

    # Walk through the structure and get counts so arrays
    # can be preallocated, and variable numbers can be assigned
    ObsVarList = []
    ObsVarExamples = []
    for LocKey, LocDict in ObsData.items():
        _nlocs += 1
        for VarKey, VarVal in LocDict.items():
            if (VarKey[1] == _oval_name):
                VarNames.add(VarKey[0])
            if (VarKey not in ObsVarList):
                ObsVarList.append(VarKey)
                ObsVarExamples.append(VarVal)
        # Extract the locations metadata encoded in the keys
        for i in range(len(loc_key_list)):
            (LocVname, LocVtype) = loc_key_list[i]
            locvar = (LocVname, 'MetaData')
            if (locvar not in ObsVarList):
                ObsVarList.append(locvar)
                ObsVarExamples.append(LocKey[i])

    # Preallocate arrays and fill them up with data from the dictionary
    ObsVars = OrderedDict()
    for o in range(len(ObsVarList)):
        VarType = type(ObsVarExamples[o])
        if (VarType in [float, np.float32, np.float64]):
            defaultval = get_default_fill_val(np.float32)
            defaultvaltype = np.float32
        elif (VarType in [int, np.int64, np.int32, np.int8]):
            defaultval = get_default_fill_val(np.int32)
            defaultvaltype = np.int32
        elif (VarType in [str, np.str_]):
            defaultval = get_default_fill_val(np.str)
            defaultvaltype = np.object_
        elif (VarType in [np.ma.core.MaskedConstant]):
            # If we happened to pick an invalid value (inf, nan, etc.) from
            # a masked array, then the type is a MaskedConstant, and that implies
            # floating point values.
            defaultval = get_default_fill_val(np.float32)
            defaultvaltype = np.float32
        else:
            print('Warning: VarType', VarType, ' not in float, int, str for:', ObsVarList[o])
            continue
        ObsVars[ObsVarList[o]] = np.full((_nlocs), defaultval, dtype=defaultvaltype)

    LocNum = 0
    # Exract record metadata encoded in the keys
    for LocKey, LocDict in ObsData.items():
        LocNum += 1

        # Extract the locations metadata encoded in the keys
        for i in range(len(loc_key_list)):
            (LocVname, LocVtype) = loc_key_list[i]
            ObsVars[(LocVname, 'MetaData')][LocNum-1] = LocKey[i]

        for VarKey, VarVal in LocDict.items():
            if (type(VarVal) in [np.ma.core.MaskedConstant]):
                VarVal = _defaultF4
            ObsVars[VarKey][LocNum-1] = VarVal

    return ObsVars, _nlocs
