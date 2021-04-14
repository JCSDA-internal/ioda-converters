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

    def BuildIoda(self, ObsVars, LocMdata, VarMdata, AttrData, VarUnits={}, TestData=None):



