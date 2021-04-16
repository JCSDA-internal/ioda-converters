#!/usr/bin/env python
import ioda
import numpy as np


class IodaWriter(object):
    # Constructor
    def __init__(self, Fname, LocKeyList, DimDict, TestKeyList=None):
        self._loc_key_list = LocKeyList
        self._dim_dict = DimDict
        self._test_key_list = TestKeyList
        # open IODA obs backend
        self._fid = ioda.Engines.HH.createFile(
                    name = Fname,
                    mode = ioda.Engines.BackendCreateModes.Truncate_If_Exists)
        # create list of dims in obs group
        self._dim_list = []
        for key, value in self._dim_dict.items():
            if key == 'nlocs':
                self._dim_list.append(ioda.NewDimensionScale.int32('nlocs',
                                      value, ioda.Unlimited, value))
            else:
                self._dim_list.append(ioda.NewDimensionScale.int32(key,
                                      value, value, value))
        # create obs group
        self._og = ioda.ObsGroup.generate(self._fid, self._dim_list)

        # store dims as a dictionary
        self._dims = {}
        for key, value in self._dim_dict.items():
            self._dims[key] = self._og.vars.open(key)

        # defining default var parameters
        self._p1 = ioda.VariableCreationParameters()
        self._p1.compressWithGZIP()
        self._p1.setFillValue.float(-999)

    def NumpyToIodaDtype(self, NumpyDtype):
        ############################################################
        # This method converts the numpy data type to the
        # corresponding ioda datatype

        if (NumpyDtype == np.dtype('float64')):
            IodaDtype = ioda.Types.double    # convert double to float
        elif (NumpyDtype == np.dtype('float32')):
            IodaDtype = ioda.Types.float
        elif (NumpyDtype == np.dtype('int64')):
            IodaDtype = ioda.Types.int64    # convert long to int
        elif (NumpyDtype == np.dtype('int32')):
            IodaDtype = ioda.Types.int32
        elif (NumpyDtype == np.dtype('int16')):
            IodaDtype = ioda.Types.int16
        elif (NumpyDtype == np.dtype('int8')):
            IodaDtype = ioda.Types.int16
        elif (NumpyDtype == np.dtype('S1')):
            IodaDtype = ioda.Types.str
        else:
            print("ERROR: Unrecognized numpy data type: ", NumpyDtype)
            exit(-2)

        return IodaDtype

    def WriteVar(self, vObj, data):
        # helper function to call the right IODA function depending
        # on datatype and shape
        datatype = self.NumpyToIodaDtype(data.dtype)
        if datatype == ioda.Types.float:
            vObj.writeNPArray.float(data)
        elif datatype == ioda.Types.double:
            vObj.writeNPArray.double(data)
        # add other elif here TODO

    def WriteAttr(self, vObj, attrName, attrVal):
        # get type of variable
        try:
            attrType = self.NumpyToIodaDtype(attrVal.dtype)
            if attrType == ioda.Types.float:
                vObj.atts.create(attrName, attrType,
                                 len(attrVal)).writeDatum.float(attrVal)
            elif attrType == ioda.Types.double:
                vObj.atts.create(attrName, attrType,
                                 len(attrVal)).writeDatum.double(attrVal)
            # add other elif here TODO
        except AttributeError: # if string
            if (type(attrVal) == str):
                attrType = ioda.Types.str
                vObj.atts.create(attrName, attrType,
                                [1]).writeDatum.str(attrVal)

    def WriteObsVars(self, ObsVars, VarDims, VarMdata, VarUnits):
        # this method will create variables in the ouput obs group and
        # fill them with the provided data and metadata
        for VarKey, Vvals in ObsVars.items():
            # get variable and group names
            (Vname, Gname) = VarKey
            VarName = "{0:s}/{1:s}".format(Gname, Vname)
            # get dimensions of variable as a list
            dims = VarDims[Vname]
            # now get a list of the actual dimension vars
            dimsVar = [self._dims[d] for d in dims]
            # get type of variable
            typeVar = self.NumpyToIodaDtype(Vvals.dtype)
            # create variable in obs group
            Var = self._fid.vars.create(VarName, typeVar,
                                        scales=dimsVar, params=self._p1)
            # need to write depending on type
            self.WriteVar(Var, Vvals)
            # add var metadata
            try:
                for MetaVar, MetaVal in VarMdata[Vname].items():
                    self.WriteAttr(Var, MetaVar, MetaVal)
            except KeyError:
                pass # no metadata for this variable
            # add var units if exists
            try:
                UnitStr = VarUnits[Vname]
                Var.atts.create("units", ioda.Types.str,
                                [1]).writeDatum.str(UnitStr)
            except KeyError:
                # add error message here later, eventually all need units!
                pass

    def BuildIoda(self, ObsVars, VarDims, LocMdata,
                  VarMdata, AttrData, VarUnits={}, TestData=None):
        self.WriteObsVars(ObsVars, VarDims, VarMdata, VarUnits)



