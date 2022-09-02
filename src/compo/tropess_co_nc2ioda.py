#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

# TODO:
#      - Add flags
#      - Unit conversion
#      - add obsvar, obserr, etc.
#      - a better way to add metadata

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import os
from pathlib import Path

import xarray as xr
import dask
import math
from numpy import log as ln

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string"),
]

obsvars = {
    'x_l_1': 'carbon_monoxide_in_l_1',
    'x_l_5': 'carbon_monoxide_in_l_5'

}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars))
}

DimDict = {
}

VarDims = {
    'x': ['nlocs']
}
    
# constants
vmr2mol_m2 = 3.405 # 1/(MWAir * grav)[(mole.s2)/(kg.m)]
hPa2Pa = 1E2
    
class tropess(object):
    def __init__(self, filenames):
        self.filenames = filenames
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        # set up variable names for IODA
        for key, item in obsvars.items():
            self.varDict[item]['valKey'] = item, iconv.OvalName()
            self.varDict[item]['errKey'] = item, iconv.OerrName()
            self.varDict[item]['qcKey'] = item, iconv.OqcName()
            self.varAttrs[item, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[item, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[item, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[item, iconv.OvalName()]['units'] = 'mol m-2'
            self.varAttrs[item, iconv.OerrName()]['units'] = 'mol m-2'
            self.varAttrs[item, iconv.OqcName()]['units'] = 'unitless'

        # loop through input filenames
        first = True
        for filename in self.filenames:

            ds_cris = xr.open_mfdataset(filename, parallel=False)

            # Read global attributes
            AttrData['sensor'] = ds_cris.attrs['Instrument']
            AttrData['platform'] = ds_cris.attrs['Platform']

            # Read lat lon
            lats = ds_cris['latitude']
            lons = ds_cris['longitude']

            # Read time and convert to ioda time format
            # note that datetime_utc variable in the intput is buggy 
            # and has +10sec offset when compared time_tai93.
            times = ds_cris['time'].dt.strftime("%Y-%m-%dT%H:%M:%SZ").values
            
            # Read pressure
            pressure = ds_cris['pressure'] # hPa (nlocsxnlevs)
            nlocs = pressure.shape[0]
            nlevs = pressure.shape[1]
            # add p=0 to the top of the atmopshere
            top_pressure = np.zeros((nlocs,1))
            full_pressure = np.concatenate((pressure, top_pressure), axis=1)
            
            # Read other variables
            x = ds_cris['x'] # dry_atmosphere_volume_mixing_ratio_of_carbon_monoxide (nlocsxnlevs)

            # open observation_ops group
            ds_cris_observation_ops= xr.open_mfdataset(filename, group='observation_ops', parallel=False)
    
            # A priori state, as volume mixing ratio (VMR) relative to dry air , unit = 1
            xa = ds_cris_observation_ops['xa'] # (nlocsxnlevs)
            

            # logarithmic_averaging_kernel
            averaging_kernel = ds_cris_observation_ops['averaging_kernel'] # (nlocsxnlevsxnlevs)
     
            # logarithmic_observation_error
            log_obs_error = ds_cris_observation_ops['observation_error']  # (nlocsxnlevsxnlevs)
            
            # The estimated state for target 0 based on x[0], xa[0], 
            # and avg_kernel[0], as volume mixing ratio (VMR) relative to dry air
            test_x = ds_cris_observation_ops['x_test']

            # --- Unit conversion from VMR to mole/m2
            x_mole_m2 = np.zeros((nlocs,nlevs))
            xa_mole_m2 = np.zeros((nlocs,nlevs))
            obs_error_mole_m2 = np.zeros((nlocs,nlevs,nlevs))
            qa = np.zeros((nlocs,nlevs))
            ap_mole_m2 = np.zeros((nlocs,nlevs))
            

            
            # --- Calculations
            # Calculate apriori term
            user_levels = [1,5]
            lev=0
            ap = np.zeros((nlocs,len(user_levels)))
            for user_level in user_levels:
                ap [:,lev] = 0
                for j in range(nlevs):                    
                    ap[:,lev] = np.exp(ln(xa[:,lev]) - averaging_kernel[:,user_level,j]*ln(xa[:,j])) + ap[:,lev]
                lev=lev+1

            for i in range(nlevs):
                delP_pa = hPa2Pa*(full_pressure[:,i] - full_pressure[:,i+1])
                x_mole_m2[:,i] = x[:,i]*delP_pa*vmr2mol_m2
                xa_mole_m2[:,i] = xa[:,i]*delP_pa*vmr2mol_m2
                obs_error_mole_m2[:,i,i] = np.exp(log_obs_error[:,i,i])*delP_pa*vmr2mol_m2 ##?? this is 3D
                
            lev=0
            for user_level in user_levels:
                delP_pa = hPa2Pa*(full_pressure[:,user_level] - full_pressure[:,user_level+1])
                print("delP is = ")
                print(delP_pa)
                print(ap[:,lev])
                ap_mole_m2[:,lev] = ap[:,lev]*delP_pa*vmr2mol_m2
                lev=lev+1

                
            # ---- Write Metadata and data 
            
            #if first:
            # add metadata variables
            self.outdata[('datetime', 'MetaData')] = times
            self.outdata[('latitude', 'MetaData')] = lats
            self.outdata[('longitude', 'MetaData')] = lons

            lev=0
            for user_level in user_levels: ## over user_levels or retrieval levels
                for j in range(nlevs):
                    varname_ak = ('averaging_kernel_l'+str(user_level)+'_level_'+str(i), 'RtrvlAncData')
                    self.outdata[varname_ak] = averaging_kernel[:,user_level,j]
                    self.varAttrs[varname_ak]['coordinates'] = 'longitude latitude'
                    self.varAttrs[varname_ak]['units'] = ''
                
                    varname_pr = ('pressure_l'+str(user_level)+'level_'+str(i), 'RtrvlAncData')
                    self.outdata[varname_pr] = hPa2Pa * pressure[:, j]
                    self.varAttrs[varname_ak]['coordinates'] = 'longitude latitude'
                    self.varAttrs[varname_ak]['units'] = 'Pa'
              
                varname_ap = ('apriori_term_'+str(user_level), 'RtrvlAncData')
                self.outdata[varname_ap] = ap_mole_m2[:,lev]
                self.varAttrs[varname_ap]['coordinates'] = 'longitude latitude'
                self.varAttrs[varname_ap]['units'] = 'mol m-2'
                
                varname_xa = ('xa_l_'+str(user_level), 'RtrvlAncData')
                self.outdata[varname_xa] = xa_mole_m2[:,lev]
                self.varAttrs[varname_xa]['coordinates'] = 'longitude latitude'
                self.varAttrs[varname_xa]['units'] = 'mol m-2'

                lev = lev + 1
                          
            counter = 0

            for i in obsvars.keys():                
                self.outdata[self.varDict[obsvars[i]]['valKey']] = x_mole_m2[:,user_levels[counter]]
                self.outdata[self.varDict[obsvars[i]]['errKey']] = obs_error_mole_m2[:,user_levels[counter],user_levels[counter]]
                self.outdata[self.varDict[obsvars[i]]['qcKey']] = qa[:,user_levels[counter]]
                counter = counter+1                                

        DimDict['nlocs'] = len(self.outdata[('datetime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])

def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads TROPESS CO netCDF files provided by NASA ??'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of TROPESS L2 CO observation netCDF input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()

    # Read in the CO data
    co = tropess(args.input)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(co.outdata, VarDims, co.varAttrs, AttrData)


if __name__ == '__main__':
    main()
