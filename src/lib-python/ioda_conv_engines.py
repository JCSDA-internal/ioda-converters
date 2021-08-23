#!/usr/bin/env python
import ioda
import numpy as np

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

    def WriteObsVars(self, ObsVars, VarDims, VarMdata, VarUnits):
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
                for MetaVar, MetaVal in VarMdata[Vname].items():
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

    def WriteGlobalAttrs(self, AttrData):
        # this method will create global attributes from AttrData dictionary
        for AttrKey, AttrVal in AttrData.items():
            self.obsspace.write_attr(AttrKey, AttrVal)

    def BuildIoda(self, ObsVars, VarDims, VarMdata, AttrData,
                  VarUnits={}, TestData=None):
        self.WriteObsVars(ObsVars, VarDims, VarMdata, VarUnits)
        self.WriteGlobalAttrs(AttrData)
