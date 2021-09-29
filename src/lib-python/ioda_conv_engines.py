#!/usr/bin/env python
import ioda
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

# list of groups to not assign the standard variable unit to
_no_units = [_oqc_name]


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


class IodaWriter(object):
    # Constructor
    def __init__(self, Fname, LocKeyList, DimDict, TestKeyList=None):
        self._loc_key_list = LocKeyList
        self._dim_dict = DimDict
        self._test_key_list = TestKeyList
        # open IODA obs backend
        self.obsspace = ioda.ObsSpace(Fname, mode='w', dim_dict=DimDict)

    def WriteObsVars(self, ObsVars, VarDims, VarAttrs, VarUnits):
        # this method will create variables in the ouput obs group and
        # fill them with the provided data and metadata
        for VarKey, Vvals in ObsVars.items():
            # get variable and group names
            (Vname, Gname) = VarKey
            VarName = "{0:s}/{1:s}".format(Gname, Vname)
            # create variable
            if Vname in VarDims.keys():
                dims = VarDims[Vname]
            else:
                # assume it is just nlocs
                dims = ['nlocs']
            self.obsspace.create_var(VarName,
                                     dtype=Vvals.dtype,
                                     dim_list=dims,
                                     )
            # write the data to the file
            tmpVar = self.obsspace.Variable(VarName)
            tmpVar.write_data(Vvals)
            # add var metadata
            try:
                for MetaVar, MetaVal in VarAttrs[Vname].items():
                    tmpVar.write_attr(MetaVar, MetaVal)
            except KeyError:
                pass  # no metadata for this variable
            # add var units if exists
            if Gname not in _no_units:
                try:
                    UnitStr = VarUnits[Vname]
                    tmpVar.write_attr("units", UnitStr)
                except (KeyError, NameError):
                    # add error message here later, eventually all need units!
                    pass

    def WriteGlobalAttrs(self, GlobalAttrs):
        # this method will create global attributes from GlobalAttrs dictionary
        for AttrKey, AttrVal in GlobalAttrs.items():
            self.obsspace.write_attr(AttrKey, AttrVal)

    def BuildIoda(self, ObsVars, VarDims, VarAttrs, GlobalAttrs,
                  VarUnits={}, TestData=None):
        self.WriteObsVars(ObsVars, VarDims, VarAttrs, VarUnits)
        self.WriteGlobalAttrs(GlobalAttrs)

def ExtractObsData(ObsData):
    ############################################################
    # This method will extract information from the ObsData
    # dictionary and reformat it into a more amenable form for
    # writing into the output file.

    _nlocs = 0
    _defaultF4 = 9e9
    _defaultI4 = 99999999

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
            _nlocs += 1
            for VarKey, VarVal in LocDict.items():
                if (VarKey[1] == _oval_name):
                    VarNames.add(VarKey[0])
                if (VarKey not in ObsVarList):
                    ObsVarList.append(VarKey)
                    ObsVarExamples.append(VarVal)
    VarList = sorted(list(VarNames))

    # Preallocate arrays and fill them up with data from the dictionary
    ObsVars = OrderedDict()
    for o in range(len(ObsVarList)):
        VarType = type(ObsVarExamples[o])
        if (VarType in [float, np.float32, np.float64]):
            defaultval = _defaultF4
            defaultvaltype = np.float32
        elif (VarType in [int, np.int64, np.int32, np.int8]):
            defaultval = _defaultI4    # convert long to int
            defaultvaltype = np.int32
        elif (VarType in [str, np.str_]):
            defaultval = ''
            defaultvaltype = np.str_
        elif (VarType in [np.ma.core.MaskedConstant]):
            # If we happened to pick an invalid value (inf, nan, etc.) from
            # a masked array, then the type is a MaskedConstant, and that implies
            # floating point values.
            defaultval = _defaultF4
            defaultvaltype = np.float32
        else:
            print('Warning: VarType', VarType, ' not in float, int, str for:', ObsVarList[o])
            continue
        ObsVars[ObsVarList[o]] = np.full((_nlocs), defaultval, dtype=defaultvaltype)

    RecNum = 0
    LocNum = 0
    for RecKey, RecDict in ObsData.items():
        RecNum += 1

        # Exract record metadata encoded in the keys
        for LocKey, LocDict in RecDict.items():
            LocNum += 1

            for VarKey, VarVal in LocDict.items():
                if (type(VarVal) in [np.ma.core.MaskedConstant]):
                    VarVal = _defaultF4
                ObsVars[VarKey][LocNum-1] = VarVal

    return ObsVars, _nlocs
