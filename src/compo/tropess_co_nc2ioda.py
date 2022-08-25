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
    'x': 'carbonmonoxideProfile'  # Total col is not ready, is Profile the right term?
    # units:1
    # standard_name:dry_atmosphere_volume_mixing_ratio_of_carbon_monoxide
    # long_name:carbon_monoxide_vmr
    # comment:Volume mixing ratio (VMR) of Carbon Monoxide relative to dry air
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
avogadro = 6.02214076E23
scm2sm = 1E4
vmr2col = 2.12E13  # What is this value for TROPESS?
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
        for iodavar in obsvars.items():
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = '1' # make sure to update this
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = '1'
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'

        # loop through input filenames
        first = True
        for filename in self.filenames:

            ds_cris = xr.open_mfdataset(filename, parallel=False)

            # get global attributes
            AttrData['sensor'] = ds_cris.attrs['Instrument']
            AttrData['platform'] = ds_cris.attrs['Platform']

            lats = ds_cris['latitude']
            lons = ds_cris['longitude']

            # convert to ioda time format
            # note that datetime_utc variable in the intput is buggy 
            # and has +10sec offset when compared time_tai93.
            times = ds_cris['time'].dt.strftime("%Y-%m-%dT%H:%M:%SZ").values
                
            pressure = ds_cris['pressure'] #hPa
            nlocs = pressure.shape[0]
            nlevs = pressure.shape[1]
            x = ds_cris['x'] # dry_atmosphere_volume_mixing_ratio_of_carbon_monoxide

            # open observation_ops group
            ds_cris_observation_ops= xr.open_mfdataset(filename, group='observation_ops', parallel=False)
    
            # A priori state, as volume mixing ratio (VMR) relative to dry air, unit = 1
            xa = ds_cris_observation_ops['xa']                                                                                                                   
    
            # logarithmic_averaging_kernel
            averaging_kernel = ds_cris_observation_ops['averaging_kernel']
     
            # logarithmic_observation_error
            observation_error = ds_cris_observation_ops['observation_error']
            
            # The estimated state for target 0 based on x[0], xa[0], and avg_kernel[0], as volume mixing ratio (VMR) relative to dry air
            test_x = ds_cris_observation_ops['x_test']
    
            # Calculate apriori term
            user_levels = [1,5] ## read this from YAML?
            lev=0
            ap = np.zeros((nlocs,len(user_levels)))
            for user_level in user_levels:
                ap [:,lev] = 0
                for j in range(nlevs):
                    ap[:,lev] = ln(xa)[:,user_level] - averaging_kernel[:,user_level,j]*ln(xa)[:,j] + ap[:,lev]
                lev=lev+1
    
            ## TOTAL COLUMN CALC:
    
            # H(xt) = ln(xa) + A (ln(xt) - ln(xa))
            # convert VMR to total column (ct) following MOPPIT Version 4 Product User's Guide
            # C = K x sum(delP x VMR); 
            # where K = 2.2 x 10^13 (mol/cm2)/(hPa ppb)
            # for tropess need to remove ppb => K = 2.2 x 10^(13+9) = 2.2 x 10^22 (mol/cm2)/(hPa)
            #vmr2col = 2.2 x 10^22    
            #ap_tc = np.zeros(nlocs)
            #for j in range(nlevs):
            #    for i in range(nlevs-1):
            #        delP = pressure[:,i] - pressure[:,i+1]
            #        sumterm = delP*x[:,i]*averaging_kernel[:,i,j] + sumterm
            #ap_tc[:,j] = (vmr2col/math.log10(math.e)) * sumterm

            # if first: # add this later
    
            # add metadata variables
            self.outdata[('datetime', 'MetaData')] = times
            self.outdata[('latitude', 'MetaData')] = lats
            self.outdata[('longitude', 'MetaData')] = lons
            #self.outdata[('apriori_term', 'RtrvlAncData')] = ap
    
            counter = 0

            for k in user_levels: ## over user_levels or retrieval levels
                varname_ak = ('averaging_kernel_level_'+str(k), 'RtrvlAncData')
                self.outdata[varname_ak] = averaging_kernel.sum(axis=1) # 2D or 1 D? Sum over rows? 
                
                varname_pr = ('pressure_level_'+str(k), 'RtrvlAncData')
                self.outdata[varname_pr] = hPa2Pa * pressure[:, k]
            
                varname_ap = ('apriori_term_'+str(k), 'RtrvlAncData')
                self.outdata[varname_ap] = ap[:,counter]
                counter = counter + 1
               
            varname_xa_ln = ('apriori_state_ln', 'RtrvlAncData')
            self.outdata[varname_xa_ln] = ln(xa)    
    

        DimDict['nlocs'] = len(self.outdata[('datetime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])
        
        for k in user_levels:
            varname = 'averaging_kernel_level_'+str(k)
            vkey = (varname, 'RtrvlAncData')
            self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
            self.varAttrs[vkey]['units'] = ''

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

    # Read in the CO2 data
    co = tropess(args.input)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(co.outdata, VarDims, co.varAttrs, AttrData)


if __name__ == '__main__':
    main()
