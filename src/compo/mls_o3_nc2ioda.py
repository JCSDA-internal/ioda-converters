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

#Global Dictionaries.
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("pressure", "float"),
    ("dateTime", "integer"),
]

obsvars = {
    'mole_fraction_of_ozone_in_air': 'mole_fraction_of_ozone_in_air',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}

DimDict = {
}

VarDims = {
    'mole_fraction_of_ozone_in_air': ['nlocs'],
}


class mls(object):
    def __init__(self, filenames,lvmin,lvmax,sTAI,eTAI):
        self.filenames = filenames
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.lmin = lvmin
        self.lmax = lvmax 
        self.startTAI = sTAI
        self.endTAI = eTAI
        self.outdata[('dateTime', 'MetaData')] = []
        self.outdata[('latitude', 'MetaData')] = []
        self.outdata[('longitude', 'MetaData')] = []
        self.outdata[('air_pressure', 'MetaData')] = []
        self._setVarDict('mole_fraction_of_ozone_in_air')
        self.outdata[self.varDict['mole_fraction_of_ozone_in_air']['valKey']] = []
        self.outdata[self.varDict['mole_fraction_of_ozone_in_air']['errKey']] = []

        self._read()

    # set ioda variable keys
    def _setVarDict(self,iodavar):
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()

    #set variable attributes for IODA
    def _setVarAttr(self,iodavar):
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        #following current output from gsi converter mol mol-1, this seems wrong when it's times 1e6
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'mol mol-1' #following current output from gsi converter, this seems wrong when it's times 1e6
        self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mol mol-1'

    # Read data needed from raw MLS file.
    def _read_nc(self,filename):
        print("Reading: {}".format(filename))
        ncd = nc.Dataset(filename, 'r')
        lat = ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Geolocation Fields'].variables['Latitude'][:] 
        lon = ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Geolocation Fields'].variables['Longitude'][:]
        time =  ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Geolocation Fields'].variables['Time'][:]
        pressure = ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Geolocation Fields'].variables['Pressure'][:]
        o3_conc = ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Data Fields'].variables['O3'][:,:]*1e6
        o3_prec = ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Data Fields'].variables['O3Precision'][:,:]*1e6
        convergence = ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Data Fields'].variables['Convergence'][:]
        status = ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Data Fields'].variables['Status'][:]
        quality = ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Data Fields'].variables['Quality'][:]
        ncd.close()
        return lat,lon,time,pressure,o3_conc,o3_prec,convergence,status,quality

    def _calc_error(self,o3,o3_prec,lev):
        #Observation error estimates from MLS.
        oe =['na','na','na','na','na','na','na', 0.02, 0.02, 0.02, 0.02, 0.035, 0.05, 0.05, 0.05, \
             0.125, 0.2, 0.2, 0.2, 0.2, 0.2, 0.225, 0.25, 0.275, \
              0.3, 0.3, 0.3, 0.3, 0.3, 0.275, 0.25, 0.225, 0.2, 0.2, \
              0.2, 0.2, 0.2, 0.15, 0.1, 0.1, 0.1, 0.15, 0.2, 0.2, 0.2, \
              0.3, 0.3, 0.3, 0.3 ]
        ooe = oe[lev]
        # inflate errors for specific levels. (note zero based index)
        if (lev == 7):  # 261 hPa
            ooe = ooe + (0.30 * abs(o3)) # 0.3 is my conservative guess
        elif (lev == 8):  # 215 hPa
            ooe = ooe + (0.20 * abs(o3))
        elif (lev == 9 ): #177 hPa
            ooe = ooe + (0.125 * abs(o3))
        elif (lev == 10 or lev == 11 or lev == 12 ):# 150-100 hPa
            ooe = ooe + (0.05 * abs(o3))

        ooe = np.sqrt(max( (0.5*ooe)**2+(o3_prec)**2, 1.e-6))
        return ooe

    def _do_qc(self,lat,lon,time,pressure,o3_conc,o3_prec,convergence,status,quality):
        oLat = []
        oLon = []
        oO3 = []
        oErr = []
        oP = []
        oTime = []
        for irec in range(lat.shape[0]):
            if( status[irec]%2 != 0 or convergence[irec]>=1.03 or quality[irec]<= 1.0 ):
                continue
            for ilev in range(self.lmin,self.lmax+1):
                if( o3_prec[irec,ilev] < 0.0):
                    continue
                #if outside the window, don't inculde data
                if(time[irec] < self.startTAI or time[irec] > self.endTAI):
                    continue
                oLat.append(lat[irec])
                oLon.append(lon[irec])
                oO3.append(o3_conc[irec,ilev])
                oErr.append(self._calc_error(o3_conc[irec,ilev],o3_prec[irec,ilev],ilev))    
                oP.append(pressure[ilev])
                oTime.append(time[irec])
        return oLat,oLon,oO3,oErr,oP,oTime                
                
    def _read(self):
        # set up variable names for IODA
        for iodavar in ['mole_fraction_of_ozone_in_air',]:
            #self._setVarDict(var)
            self._setVarAttr(iodavar)
       # loop through input filenames
        for f in self.filenames:
            lat,lon,time,pressure,o3_conc,o3_prec,convergence,status,quality = self._read_nc(f)
            vlat,vlon,vo3,verr,vpres,vtime = self._do_qc(lat, lon, time, pressure, o3_conc,
                                                         o3_prec, convergence, status, quality)
            # add metadata variables
            self.outdata[('dateTime', 'MetaData')].extend(vtime)
            self.outdata[('latitude', 'MetaData')].extend(vlat)
            self.outdata[('longitude', 'MetaData')].extend(vlon)
            self.outdata[('air_pressure', 'MetaData')].extend(vpres)
            for ncvar, iodavar in obsvars.items():
                self.outdata[self.varDict[iodavar]['valKey']].extend(vo3)
                self.outdata[self.varDict[iodavar]['errKey']].extend(verr)
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
# end mls object.

def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads MLS O3 HDF5 files provided by NASA (somewhere) '
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
        '-s', '--level-start',
        help="mls level to start 1 based index (default=8)",
        type=int, required=False,default=8,dest='lmin')
    optional.add_argument(
        '-e', '--level-end',
        help="mls level to end 1 based index (default=49)",
        type=int, required=False,default=49,dest='lmax')
    optional.add_argument(
        '-p', '--prefix',
        help="mls filename prefix (default=MLS-Aura_L2GP-O3_v05-01)",
        type=str, required=False, default="MLS-Aura_L2GP-O3_v05-01",dest='prefix')
 

    args = parser.parse_args()

    # check for probably somewhat useless option to modify levels output
    if(args.lmin < 8):
        print("Sorry, level 8 is low as I go! Setting lmin=8.")
        lmin = 8
    else:
        lmin = args.lmin
    if(args.lmax > 49):
        print("Sorry, level 49 is high as I go! Setting lmax=49.")
        lmax = 49
    else:
        lmax = args.lmax

    #Get Day of year for current cycle and associated file(s)     
    cycle_time = datetime(args.year,args.month,args.day,args.hour)
    doy = cycle_time.strftime('%j')
    year = cycle_time.year
    rawFiles = glob.glob( os.path.join(args.input, args.prefix+"*{}d".format(year)+doy+".he5") )
    # if 00z cycle add previous day's file(s)
    if (args.hour == 0):
        previous_cycle = cycle_time - timedelta(days=1) 
        doy = previous_cycle.strftime('%j')
        year = previous_cycle.year
        rawFiles.extend( glob.glob( os.path.join(args.input, args.prefix+"*{}d".format(year)+doy+".he5") ) )

    rawFiles.sort()
    

    # get start and end times for qc/cropping data in MLS native time format (TAI seconds since Jan 1, 1993.)    
    startTAI = ( ( cycle_time - timedelta(hours=3) ) - datetime(1993,1,1,0) ).total_seconds()
    endTAI = ( ( cycle_time + timedelta(hours=3) ) - datetime(1993,1,1,0) ).total_seconds()

    # Read in the O3 data in window over selected levels (if not default 8-49)
    o3 = mls(rawFiles, lmin-1, lmax-1, startTAI, endTAI)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(o3.outdata, VarDims, o3.varAttrs, AttrData)

if __name__ == '__main__':
    main()
