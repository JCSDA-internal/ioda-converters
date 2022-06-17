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

obsvars = {
    'integrated_layer_ozone_in_air': 'integrated_layer_ozone_in_air',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}

DimDict = {
}

VarDims = {
    'integrated_layer_ozone_in_air': ['nlocs'],
}


class omi(object):
    def __init__(self, filenames,sTAI,eTAI):
        self.filenames = filenames
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.startTAI = sTAI
        self.endTAI = eTAI
        self.outdata[('dateTime', 'MetaData')] = []
        self.outdata[('latitude', 'MetaData')] = []
        self.outdata[('longitude', 'MetaData')] = []
        self.outdata[('scan_position', 'MetaData')] = []
        self.outdata[('Solar_Zenith_Angle', 'MetaData')] = []
        self._setVarDict('integrated_layer_ozone_in_air')
        self.outdata[self.varDict['integrated_layer_ozone_in_air']['valKey']] = []
        #self.outdata[self.varDict['integrated_layer_ozone_in_air']['errKey']] = []

        self._read()

    # set ioda variable keys
    def _setVarDict(self,iodavar):
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        #self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        #self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()

    #set variable attributes for IODA
    def _setVarAttr(self,iodavar):
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        #self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        #self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        #following current output from gsi converter mol mol-1, this seems wrong when it's times 1e6
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'DU' 
        #self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mol mol-1'

    # Read data needed from raw MLS file.
    def _read_nc(self,filename):
        print("Reading: {}".format(filename))
        ncd = nc.Dataset(filename, 'r')
        lat = ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Geolocation Fields'].variables['Latitude'][:,:] 
        lon = ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Geolocation Fields'].variables['Longitude'][:,:]
        time =  ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Geolocation Fields'].variables['Time'][:]
        solZA = ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Geolocation Fields'].variables['SolarZenithAngle'][:,:]
        
        priorO3 = ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Data Fields'].variables['APrioriLayerO3'][:,:,:]
        print('prior shape',priorO3.shape)
        print('lat shape',lat.shape)
        print('time shape',time.shape)
        layerEff = ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Data Fields'].variables['LayerEfficiency'][:,:,:]
        o3_du = ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Data Fields'].variables['ColumnAmountO3'][:,:]
        print('du shape',o3_du.shape)
        quality = ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Data Fields'].variables['QualityFlags'][:,:]
        qualityAlg = ncd.groups['HDFEOS'].groups['SWATHS'].groups['OMI Column Amount O3'].groups['Data Fields'].variables['AlgorithmFlags'][:,:]
        ncd.close()
        return lat,lon,time,solZA,priorO3,layerEff,o3_du,quality,qualityAlg


    def _do_qc(self,lat,lon,time,solZA,priorO3,layerEff,o3_du,quality,qualityAlg):
        oScanPos = []
        oLat = []
        oLon = []
        oO3 = []
        oTime = []
        oSza = []
        oPrior = []
        oLayerEff = []
 
        for itime in range(lat.shape[0]):
            for iscan in range(lat.shape[1]):
                if(time[itime] < self.startTAI or time[itime] > self.endTAI):
                    continue
                if (priorO3[itime,iscan, 0] <= 0.0 or o3_du[itime, iscan] <= 0.0):
                    continue
                # from Kris' fortran shenanigans:
                #!! A hack to use the quality flags as recommended by the OMI team (Jul 2019)
                #!! without having to change the GSI code
                #!! use any alqf as long as it's not 0
                #!! The GSI will reject alqf = 3 so if alqf=0 set it to 3, otherwise set it to 1
                # We don't GAF about the GSI, or these silly flags, just chuck it. 
                if ( (qualityAlg[itime,iscan] == 0) or (qualityAlg[itime,iscan] == 3) ):
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
                if ( is_bit_set(quality[itime,iscan],1) or \
                     is_bit_set(quality[itime,iscan],2) or \
                     is_bit_set(quality[itime,iscan],3)  ): continue
                # break out second condition, just because it's mentioned that way for bits 6,8,9                
                if ( is_bit_set(quality[itime,iscan],6) or \
                     is_bit_set(quality[itime,iscan],8) or \
                     is_bit_set(quality[itime,iscan],9)  ): continue
                # could simply this further with one if statement possibly more clever use of a bit masking.
                oScanPos.append(iscan+1)
                oLat.append(lat[itime,iscan])
                oLon.append(lon[itime,iscan])
                oO3.append(o3_du[itime,iscan])
                oTime.append(time[itime])
                oSza.append(solZA[itime,iscan])
                oPrior.append(np.flip(priorO3[itime,iscan,:],axis=0))
                oLayerEff.append(np.flip(layerEff[itime,iscan,:],axis=0))
        return oScanPos,oLat,oLon,oO3,oTime,oSza,oPrior,oLayerEff
                
    def _read(self):
        # set up variable names for IODA
        for iodavar in ['integrated_layer_ozone_in_air',]:
            #self._setVarDict(var)
            self._setVarAttr(iodavar)
       # loop through input filenames
        for f in self.filenames:
            lat,lon,time,solZA,priorO3,layerEff,o3_du,quality,qualityAlg = self._read_nc(f)
            vpos,vlat,vlon,vo3,vtime,vsza,arrPrior,arrLayerEff = self._do_qc(lat,lon,time,solZA,
                                                                             priorO3,layerEff,o3_du,
                                                                             quality,qualityAlg)
            # add metadata variables
            self.outdata[('dateTime', 'MetaData')].extend(vtime)
            self.outdata[('latitude', 'MetaData')].extend(vlat)
            self.outdata[('longitude', 'MetaData')].extend(vlon)
            self.outdata[('scan_position', 'MetaData')].extend(vpos)
            self.outdata[('Solar_Zenith_Angle', 'MetaData')].extend(vsza)

            for ncvar, iodavar in obsvars.items():
                self.outdata[self.varDict[iodavar]['valKey']].extend(vo3)
                #self.outdata[self.varDict[iodavar]['qcKey']] = qc_flag
        DimDict['nlocs'] = len(self.outdata[('dateTime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])
        for k in self.outdata.keys():
            self.outdata[k] = np.asarray(self.outdata[k])
        # EOS AURA uses TAI93 so add seconds offset from UNIX time for IODA
        self.outdata[('dateTime','MetaData')] = self.outdata[('dateTime','MetaData')]\
                                                + (datetime(1993,1,1,0,0) - datetime(1970,1,1,0,0)).total_seconds()
        self.outdata[('dateTime','MetaData')] = self.outdata[('dateTime','MetaData')]*1e9 #convert to nano seconds
        self.outdata[('dateTime','MetaData')].astype(np.int64) 
# end omi object.

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
 

    args = parser.parse_args()


    #Get Day of year for current cycle and associated file(s)     
    cycle_time = datetime(args.year,args.month,args.day,args.hour)
    year = cycle_time.year
    month = cycle_time.month
    day = cycle_time.day
    rawFiles = glob.glob( os.path.join(args.input, args.prefix+"_{}m{}{}".format(year,month,day)+"*.he5") )
    
    print('raw',rawFiles)
    # if 00z cycle add previous day's file(s)
    if (args.hour == 0):
        previous_cycle = cycle_time - timedelta(days=1) 
        year = previous_cycle.year
        month = previous_cycle.month
        day = previous_cycle.day
        rawFiles.extend( glob.glob( os.path.join(args.input, args.prefix+"_{}m{}{}".format(year,month,day)+"*.he5") ) )
    print('mor raw',rawFiles) 
    rawFiles.sort()
     

    # get start and end times for qc/cropping data in MLS native time format (TAI seconds since Jan 1, 1993.)    
    startTAI = ( ( cycle_time - timedelta(hours=3) ) - datetime(1993,1,1,0) ).total_seconds()
    endTAI = ( ( cycle_time + timedelta(hours=3) ) - datetime(1993,1,1,0) ).total_seconds()

    # Read in the O3 data in window 
    o3 = omi(rawFiles, startTAI, endTAI)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(o3.outdata, VarDims, o3.varAttrs, AttrData)

if __name__ == '__main__':
    main()
