#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Standard Python library imports.
import os
import sys
import argparse
import glob
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
from collections import defaultdict, OrderedDict

#pyIoda libraries.
#Append pyioda paths so ioda_conv_engines can be loaded
IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

def is_bit_set(integer_value,bit_position):
    return (integer_value & (1 << bit_position)) != 0

#Global Dictionaries.
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "integer"),
]

ioda2nc = {}
ioda2nc['latitude'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Geolocation Fields/Latitude'
ioda2nc['longitude'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Geolocation Fields/Longitude'
ioda2nc['dateTime'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Geolocation Fields/Time'
ioda2nc['solar_zenith_angle'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Geolocation Fields/SolarZenithAngle'
ioda2nc['prior_o3'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/APrioriLayerO3'
#ioda2nc['Layer_Efficiency'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/LayerEfficiency'
ioda2nc['valKey'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/ColumnAmountO3'
ioda2nc['quality_flag'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/QualityFlags'
ioda2nc['algorithm_flag'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/AlgorithmFlags'
 

obsvars = {
    'integrated_layer_ozone_in_air': 'integrated_layer_ozone_in_air',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
    'satellite': 'aura',
    'sensor': 'omi',
}

DimDict = {
}

VarDims = {
    'integrated_layer_ozone_in_air': ['nlocs'],
}


class omi(object):
    def __init__(self, filenames, sTAI, eTAI, qcOn):
        self.filenames = filenames
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.startTAI = sTAI
        self.endTAI = eTAI
        self.qcOn = qcOn
        self._setVarDict('integrated_layer_ozone_in_air')

        # initialize dictionary with  empty list for MetaData variable to populate later.
        vars2output = list(ioda2nc.keys())
        vars2output.append('scan_position')
        for v in vars2output:
            if(v != 'valKey'):
                self.outdata[(v, 'MetaData')] = []
        self.outdata[self.varDict['integrated_layer_ozone_in_air']['valKey']] = []
        self._read()

    # set ioda variable keys
    def _setVarDict(self,iodavar):
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        #self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        #self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()

    #set variable attributes for IODA
    def _setVarAttr(self,iodavar):
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        missing_value = 9.96921e+36
        int_missing_value = -2147483647
        self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = missing_value
        #self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = missing_value
        #self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = int_missing_value
        varsToAddUnits = list(ioda2nc.keys())
        varsToAddUnits.append('scan_position')
        for v in varsToAddUnits:
            if(v != 'valKey'):
                vkey = (v,'MetaData')
                if( 'pressure' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'Pa'
                elif(v == 'dateTime'):
                    self.varAttrs[vkey]['units'] = 'seconds since 1970-01-01T00:00:00Z'
                elif('angle' in v.lower()): 
                    self.varAttrs[vkey]['units'] = 'degrees'
                elif('flag' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'unitless'
                elif('prior' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'ppmv'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'DU' 
    # Read data needed from raw MLS file.
    def _read_nc(self,filename):
        print("Reading: {}".format(filename))
        ncd = nc.Dataset(filename, 'r')
        d = {}
        # use dictionary above to just read fields we want out of the netcdf.
        for k in list(ioda2nc.keys()):
            d[k] = ncd[ ioda2nc[k] ][...]
            d[k].mask = False
        ncd.close()
        return d 
    def _just_flatten(self,d):
        # make a temporary data dictionary to transfer things into.
        dd = {}
        for k in list(d.keys()):
            if(k == 'dateTime'):
                # for flat array need to make it 2d to match other arrays before flattening again.
                scn = np.arange(1,d['latitude'].shape[1]+1)
                scn_tmp,tmp = np.meshgrid(scn,d[k])
                tmp = tmp.astype(np.int64)
                dd[k] = tmp.flatten().tolist()
                dd['scan_position'] = scn_tmp.flatten().tolist()
            elif(k == 'Prior_O3' ):
                dd[k] = d[k][:,:,0].flatten().tolist()
            else:
                dd[k] = d[k].flatten().tolist()
        dtA = np.asarray(dd['dateTime']) 
        idx = np.where( ( self.startTAI <= dtA) & (dtA<=self.endTAI ) )
        for k in list(dd.keys()):
            dd[k] = np.asarray(dd[k])[idx]
            dd[k] = dd[k].tolist()
        return dd

    def _do_qc(self,d):
        #intialize dictonary of qc'd variables
        dd = {}
        flatVars = list(ioda2nc.keys())
        flatVars.append('scan_position')
        for v in flatVars:
            dd[v] = []
 
        for itime in range(d['latitude'].shape[0]):
            for iscan in range(d['latitude'].shape[1]):
                if(d['dateTime'][itime] < self.startTAI or d['dateTime'][itime] > self.endTAI):
                    continue
                if (d['prior_o3'][itime,iscan, 0] <= 0.0 or d['valKey'][itime, iscan] <= 0.0):
                    continue
                # from Kris' fortran shenanigans:
                #!! A hack to use the quality flags as recommended by the OMI team (Jul 2019)
                #!! without having to change the GSI code
                #!! use any alqf as long as it's not 0
                #!! The GSI will reject alqf = 3 so if alqf=0 set it to 3, otherwise set it to 1
                if ( (d['algorithm_flag'][itime,iscan] == 0) or (d['algorithm_flag'][itime,iscan] == 3) ):
                    continue
                # Code from Kris' Fortran########################
                #!! Bits 0-3 combined into an integer: use 0 or 1 only
                #!! Do not use if bits 6, 8 or 9 are set (to 1)
                #!! counting from bit 1 the bad ones are 7, 9 and 10
                #!! Use everything else
                #  decimal = qf(iscan,itime)
                #  qf(iscan,itime) = 0
                #  call dec2bin(decimal,bintoq,16)
                #  first3 = bintoq(1)+2*bintoq(2)+4*bintoq(3)+8*bintoq(4)
                #!  print *, 'quality DEBUG ', first3, bintoq(:)
                ###############################################

                # this logic is a little squirelly, let's simplify the above statements:
                #
                # 1. first3 is silly. bits 0-3 Bit 0 is a don't care (can be 0 or 1) 
                #    we do care if bit 1, 2 or 3 are set. Sooo, if those are set, kick out.
                # 2. lets not deal with weird QC flags in JEDI, let's just kick it out here.
                #
                # Python helps us here because 0 is an index (as it should be) 
                if ( is_bit_set(d['quality_flag'][itime,iscan],1) or \
                     is_bit_set(d['quality_flag'][itime,iscan],2) or \
                     is_bit_set(d['quality_flag'][itime,iscan],3)  ): continue
                # break out second condition, just because it's mentioned that way for bits 6,8,9                
                if ( is_bit_set(d['quality_flag'][itime,iscan],6) or \
                     is_bit_set(d['quality_flag'][itime,iscan],8) or \
                     is_bit_set(d['quality_flag'][itime,iscan],9)  ): continue
                # could simply this further with one if statement possibly more clever use of a bit masking.
                #FUN! MetaData/scan_position must be a float with IODA (little green jerk)!
                dd['scan_position'].append( float(iscan+1) )                 
                for v in flatVars:
                    if(v == 'dateTime'):
                        dd[v].append(d[v][itime]) 
                    elif( v == 'prior_o3' ):
                        dd[v].append(d[v][itime,iscan,0])
                    elif( v!= 'scan_position'):
                        dd[v].append(d[v][itime,iscan])
                        
                #oPrior.append(np.flip(priorO3[itime,iscan,:],axis=0))
                #oLayerEff.append(np.flip(layerEff[itime,iscan,:],axis=0))

        return dd
                
    def _read(self):
        # set up variable names for IODA
        for iodavar in ['integrated_layer_ozone_in_air',]:
            #self._setVarDict(var)
            self._setVarAttr(iodavar)
       # loop through input filenames
        for f in self.filenames:
            nc_data = self._read_nc(f)
            if(self.qcOn):
                print('Doing QC.')
                d = self._do_qc(nc_data)
            else:
                print('Not doing QC.')
                d = self._just_flatten(nc_data)
            # add metadata variables
            for v in list(d.keys()):
                if(v != 'valKey' ):
                    self.outdata[(v, 'MetaData')].extend(d[v])
            
            for ncvar, iodavar in obsvars.items():
                self.outdata[self.varDict[iodavar]['valKey']].extend(d['valKey'])
                #self.outdata[self.varDict[iodavar]['qcKey']].extend(d['qcKey'])
                #self.outdata[self.varDict[iodavar]['errKey']].extend(np.zeros( len(d['valKey']) ).tolist() )
        DimDict['nlocs'] = len(self.outdata[('longitude', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])
        # add dummy air_pressure so UFO will know this is a total column ob, and not partial.
        self.outdata[('air_pressure','MetaData')] = np.zeros(DimDict['nlocs']).tolist()
        self.varAttrs[('air_pressure','MetaData')]['units'] = 'Pa'

        for k in self.outdata.keys():
            self.outdata[k] = np.asarray(self.outdata[k])
            if(self.outdata[k].dtype =='float64'):
                self.outdata[k] = self.outdata[k].astype('float32')
            elif(self.outdata[k].dtype == 'int64' and k != ('dateTime','MetaData')):
                self.outdata[k] = self.outdata[k].astype('int32') 
            elif(self.outdata[k].dtype == 'uint16' or self.outdata[k].dtype == 'uint8'):
                self.outdata[k] = self.outdata[k].astype(int)

        # EOS AURA uses TAI93 so add seconds offset from UNIX time for IODA
         
        self.outdata[('dateTime','MetaData')] = self.outdata[('dateTime','MetaData')]\
                                                + int((datetime(1993,1,1,0,0) - datetime(1970,1,1,0,0)).total_seconds())
        self.outdata[('dateTime','MetaData')].astype('int64') 
        #ensure lon is 0-360
        self.outdata[('longitude','MetaData')] = self.outdata[('longitude','MetaData')] % 360
def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads OMI O3 HDF5 files provided by NASA (somewhere) '
            'and converts into IODA formatted output files. Multiple '
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of MLS input file(s)",
        type=str, required=True)
    required.add_argument(
        '-y', '--year',
        help="syn. time year",
        type=int, required=True)
    required.add_argument(
        '-m', '--month',
        help="syn. time month",
        type=int,  required=True)
    required.add_argument(
        '-d', '--day',
        help="syn. time day",
        type=int, required=True)
    required.add_argument(
        '-z', '--hour',
        help="syn. time hour.",
        type=int, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-p', '--prefix',
        help="omi filename prefix (default=OMI-Aura_L2-OMTO3)",
        type=str, required=False, default="OMI-Aura_L2-OMTO3",dest='prefix')

    optional.add_argument('--qc', dest='qc', action='store_true',default=True)
    optional.add_argument('--no-qc', dest='qc', action='store_false')
    args = parser.parse_args()
    #Get Day of year for current cycle and associated file(s)     
    cycle_time = datetime(args.year,args.month,args.day,args.hour)
    year = cycle_time.year
    month = cycle_time.month
    day = cycle_time.day
    rawFiles = glob.glob( os.path.join(args.input, args.prefix+"_{}m{}{}".format(year,month,day)+"*.he5") )
    
    # if 00z cycle add previous day's file(s)
    if (args.hour == 0):
        previous_cycle = cycle_time - timedelta(days=1) 
        year = previous_cycle.year
        month = previous_cycle.month
        day = previous_cycle.day
        rawFiles.extend( glob.glob( os.path.join(args.input, args.prefix+"_{}m{}{}".format(year,month,day)+"*.he5") ) )

    rawFiles.sort()
    rawFilesOut = []
    startDateWindow = cycle_time - timedelta(hours=3)
    endDateWindow = cycle_time + timedelta(hours=3)

    for f in rawFiles:
        vv = f.split('_')
        #v003-2020m1214t060743.he5 2020m1214t0003-o87313        
        startDateFile = datetime.strptime(vv[-2][0:-7],"%Ym%m%dt%H%M")
        endDateFile = datetime.strptime(vv[-1][5:-6],"%Ym%m%dt%H%M")
        if( startDateWindow <= startDateFile <= endDateWindow or startDateWindow <= endDateFile <= endDateWindow):
            rawFilesOut.append(f)
    rawFiles = rawFilesOut
    # get start and end times for qc/cropping data in MLS native time format (TAI seconds since Jan 1, 1993.)    
    startTAI = ( ( cycle_time - timedelta(hours=3) ) - datetime(1993,1,1,0) ).total_seconds()
    endTAI = ( ( cycle_time + timedelta(hours=3) ) - datetime(1993,1,1,0) ).total_seconds()

    # Read in the O3 data in window 
    o3 = omi(rawFiles, startTAI, endTAI,args.qc)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(o3.outdata, VarDims, o3.varAttrs, AttrData)

if __name__ == '__main__':
    main()
