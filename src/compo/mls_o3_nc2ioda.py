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
from collections import defaultdict, OrderedDict,Counter

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
    ("air_pressure", "float"),
    ("dateTime", "integer"),
]

ioda2nc = {}
ioda2nc['latitude'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Latitude'
ioda2nc['longitude'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Longitude'
ioda2nc['dateTime'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Time'
ioda2nc['air_pressure'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Pressure'
ioda2nc['valKey'] = 'HDFEOS/SWATHS/O3/Data Fields/O3'
ioda2nc['Precision'] = 'HDFEOS/SWATHS/O3/Data Fields/O3Precision'
ioda2nc['Convergence'] = 'HDFEOS/SWATHS/O3/Data Fields/Convergence'
ioda2nc['Status'] = 'HDFEOS/SWATHS/O3/Data Fields/Status'
ioda2nc['Quality'] = 'HDFEOS/SWATHS/O3/Data Fields/Quality'
ioda2nc['Solar_Zenith_Angle'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/SolarZenithAngle'
 


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
    def __init__(self, filenames,lvmin,lvmax,sTAI,eTAI,nrt,qcOn):
        self.filenames = filenames
        self.qcOn = qcOn
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.lmin = lvmin
        self.lmax = lvmax 
        self.startTAI = sTAI
        self.endTAI = eTAI
        self.nrt = nrt


        self.outdata[('dateTime', 'MetaData')] = []
        self.outdata[('latitude', 'MetaData')] = []
        self.outdata[('longitude', 'MetaData')] = []
        self.outdata[('air_pressure', 'MetaData')] = []
        self.outdata[('Precision', 'MetaData')] = []
        self.outdata[('Convergence', 'MetaData')] = []
        self.outdata[('Status', 'MetaData')] = []
        self.outdata[('Quality', 'MetaData')] = []
        self.outdata[('Level', 'MetaData')] = []
        self.outdata[('Solar_Zenith_Angle', 'MetaData')] = []
        self._setVarDict('mole_fraction_of_ozone_in_air')
        self.outdata[self.varDict['mole_fraction_of_ozone_in_air']['valKey']] = []
        if(self.qcOn): 
            self.outdata[self.varDict['mole_fraction_of_ozone_in_air']['errKey']] = []

        self._read()

    # set ioda variable keys
    def _setVarDict(self,iodavar):
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        if(self.qcOn):
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
    def _read_nc(self,filename,ifile,maxfile):
        print("Reading: {}".format(filename))
        ncd = nc.Dataset(filename, 'r')
        end_of_file = len(ncd.groups['HDFEOS'].groups['SWATHS'].groups['O3'].groups['Geolocation Fields'].variables['Latitude'][:])
        # unles nrt, use all profiles in file.
        start = 0
        end = end_of_file
        # if it is nrt, and the last file use all but first two and last 3 profiles
        if( ifile == maxfile and self.nrt == True):
            print("last file:{}".format(filename))
            start = 2
            end = end_of_file - 3
        # otherwise if nrt skip first 2 and last 8 profiles to avoid duplicates.
        elif( self.nrt == True ):
            start = 2
            end = end_of_file - 8
        else:
            start = 0 
            end = end_of_file
        d = {}
        for k in list(ioda2nc.keys()):
            if (k == 'air_pressure'):
                d[k] = ncd[ ioda2nc[k] ][...]*100. #convert to Pa
                d[k].mask = False
            else:
                d[k] = ncd[ ioda2nc[k] ][start:end,...]
                d[k].mask = False
              
            if(k == 'valKey' or k =='Precision'):
                d[k] = d[k]*1e6 #convert mol/mol to PPMV
        return d 

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

    def _just_flatten(self,d):
        #only output desired levels (lmin through lmax)
        dd = {}
        dd['valKey'] = d['valKey'][:,self.lmin:self.lmax]
        dd['Precision'] = d['Precision'][:,self.lmin:self.lmax]
        lvec = np.arange(self.lmin+1,self.lmax+1)
        dd['Level'],dd['Status'] = np.meshgrid(np.arange(self.lmin+1,self.lmax+1),d['Status'])
        dd['air_pressure'],dd['dateTime'] = np.meshgrid(d['air_pressure'][self.lmin:self.lmax],d['dateTime'])
        _,dd['Quality'] = np.meshgrid(lvec,d['Quality'])
        _,dd['Convergence'] = np.meshgrid(lvec,d['Convergence'])
        _,dd['Status'] = np.meshgrid(lvec,d['Status'])
        _,dd['latitude'] = np.meshgrid(lvec,d['latitude'])
        _,dd['longitude'] = np.meshgrid(lvec,d['longitude'])
        _,dd['Solar_Zenith_Angle'] = np.meshgrid(lvec,d['Solar_Zenith_Angle'])
        for k in list(dd.keys()):
            
            dd[k] = dd[k].flatten().tolist() 
        return dd
 


    def _do_qc(self,d):
        oLat = []
        oLevel = []
        oLon = []
        oO3 = []
        oErr = []
        oP = []
        oTime = []
        oConv = []
        oQual = []
        oPrec = []
        oStat = []
        oSza = []
        nrec = d['latitude'].shape[0]
        status = d['Status']
        o3_prec = d['Precision']
        o3_conc = d['valKey']
        time = d['dateTime']
        convergence = d['Convergence']
        quality = d['Quality']
        lat = d['latitude']
        lon = d['longitude']
        pressure = d['air_pressure']
        for irec in range(nrec):
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
                oQual.append(quality[irec])
                oConv.append(convergence[irec])
                oPrec.append(o3_prec[irec,ilev])
                oStat.append(status[irec])
                oLevel.append(ilev+1)
                oSza.append(d['Solar_Zenith_Angle'][irec])
        d['latitude'] = oLat
        d['longitude'] = oLon
        d['dateTime'] = oTime
        d['air_pressure'] = oP
        d['valKey'] = oO3
        d['errKey']= oErr
        d['Quality'] = oQual
        d['Convergence'] = oConv
        d['Precision'] = oPrec
        d['Status'] = oStat 
        d['Level'] = oLevel
        return d
                
    def _read(self):
        first = True
        # set up variable names for IODA
        for iodavar in ['mole_fraction_of_ozone_in_air',]:
            #self._setVarDict(var)
            self._setVarAttr(iodavar)
       # loop through input filenames
        for i,f in enumerate(self.filenames):
            nc_data = self._read_nc(f,i,len(self.filenames)-1)
            #dups = [item for item, count in Counter(time).items() if count > 1]
            if(self.qcOn):
                print("Performing QC.")
                d = self._do_qc(nc_data)
            else:
                print("Not Performing QC.")
                d = self._just_flatten(nc_data)
            for v in list(d.keys()):
                if(v != 'valKey' and v != 'errKey'):
                    self.outdata[(v,'MetaData')].extend(d[v]) 
            for ncvar, iodavar in obsvars.items():
                self.outdata[self.varDict[iodavar]['valKey']].extend(d['valKey'])
                if(self.qcOn):
                    self.outdata[self.varDict[iodavar]['errKey']].extend(d['errKey'])
                #self.outdata[self.varDict[iodavar]['qcKey']] = qc_flag
        DimDict['nlocs'] = len(self.outdata[('dateTime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])
        # run a time duplicate check to see if NRT works.
        for k in self.outdata.keys():
            self.outdata[k] = np.asarray(self.outdata[k])
            if(self.outdata[k].dtype =='float64'):
                self.outdata[k] = self.outdata[k].astype('float32')
            elif(self.outdata[k].dtype == 'int64' and k != ('dateTime','MetaData')):
                self.outdata[k] = self.outdata[k].astype('int32') 

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
    optional.add_argument('--qc', dest='qc', action='store_true',default=True)
    optional.add_argument('--no-qc', dest='qc', action='store_false')
 

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
    if('NRT' in args.prefix):
        nrt = True
    else:
        nrt = False

    #Get Day of year for current cycle and associated file(s)     
    cycle_time = datetime(args.year,args.month,args.day,args.hour)
    current_doy = cycle_time.strftime('%j')
    current_year = cycle_time.year
    current_hour = cycle_time.hour
    # if 00z cycle add previous day's file(s)
    if (args.hour == 0):
        previous_3hr = cycle_time - timedelta(hours=3) 
        prev_doy = previous_3hr.strftime('%j')
        prev_year = previous_3hr.year
        if(nrt):
            rawFiles = glob.glob( os.path.join(args.input, args.prefix+"*{}d".format(prev_year)+prev_doy+"*.he5") ) 
            rawFiles.extend( glob.glob( os.path.join(args.input, args.prefix+"*{}d".format(current_year)+current_doy+"*.he5") ) )
        else:
            rawFiles = glob.glob( os.path.join(args.input, args.prefix+"*{}d".format(prev_year)+prev_doy+"*.he5") ) 
            rawFiles.extend( glob.glob( os.path.join(args.input, args.prefix+"*{}d".format(current_year)+current_doy+".he5") ) )
    elif(args.hour !=0 and nrt):
        rawFiles = glob.glob( os.path.join(args.input, args.prefix+"*{}d".format(current_year)+current_doy+"*.he5") )
    else:
        rawFiles = glob.glob( os.path.join(args.input, args.prefix+"*{}d".format(current_year)+current_doy+".he5") )

    rawFiles.sort()
    if(nrt):
        #limit files only between start and end of window.    
        rawFilesOut = []
        startDateWindow = cycle_time - timedelta(hours=3)
        endDateWindow = cycle_time + timedelta(hours=3)
        for f in rawFiles:
            ftime = datetime.strptime(f[-17::],"%Yd%jt%H%M.he5")
            if( startDateWindow <= ftime <= endDateWindow ):
                rawFilesOut.append(f)
        rawFiles = rawFilesOut

    if( len(rawFiles) == 0):
        sys.exit('No Raw Files Found!!!')   
    # get start and end times for qc/cropping data in MLS native time format (TAI seconds since Jan 1, 1993.)    
    startTAI = ( ( cycle_time - timedelta(hours=3) ) - datetime(1993,1,1,0) ).total_seconds()
    endTAI = ( ( cycle_time + timedelta(hours=3) ) - datetime(1993,1,1,0) ).total_seconds()


    # Read in the O3 data in window over selected levels (if not default 8-49)
    # if nrt is set, assumes near real-time and will skip first two and last 8 profiles for most files 
    # (last 3 profiles skipped if it is the last file in window)
    # RTM regarding Near Real time (NRT) in 
    # https://discnrt1.gesdisc.eosdis.nasa.gov/data/Aura_NRT/ML2SO2_NRT.005/doc/NRT-user-guide-v5.pdf
    o3 = mls(rawFiles, lmin-1, lmax-1, startTAI, endTAI, nrt,args.qc)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(o3.outdata, VarDims, o3.varAttrs, AttrData)
    
if __name__ == '__main__':
    main()
