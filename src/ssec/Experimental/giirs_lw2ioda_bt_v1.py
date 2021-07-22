#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import argparse
import numpy as np
from datetime import datetime, timedelta
import netCDF4 as nc
import re
import dateutil.parser
import math
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict


vName = "brightness_temperature"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

LocMdata = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
RecMdata = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
VarMdata = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
AttrData = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))


class LwRadiance(object):
    def __init__(self, filenames, writer):
        self.filenames = filenames
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.units_values = {}  # cannot use an ordered dict for this
        self.writer = writer
        self.readInFiles()

    def xferMdataVar(self, ncd, inName, outName, mData, dType):
        # This method adds a variable to the given metadata dictionary element.
        ncVar = ncd.variables[inName]
        mData[outName] = self.writer.FillNcVector(ncVar[:], dType)
        if 'units' in ncVar.ncattrs():
            self.units_values[outName] = ncVar.getncattr('units')

    def xferLocMdataVar(self, ncd, numFiles, inName, outName, mData, dType):
        # This method adds a variable to the given metadata dictionary element.
        ncVar = ncd.variables[inName]
        if (numFiles == 1):
            mData[outName] = self.writer.FillNcVector(ncVar[:], dType)
            if 'units' in ncVar.ncattrs():
                self.units_values[outName] = ncVar.getncattr('units')
        else:
            mData[outName] = np.append(mData[outName], self.writer.FillNcVector(ncVar[:], dType), axis=0)

    def readInFiles(self):
        # This method will transfer variable data from the input file into a
        # dictionary (self.data) that the netcdf writer understands.
        #
        # The mapping from input variables to ioda variables for IR LW radiance is:
        #
        #   dimensions:
        #      LWchannel -> nvars
        #      LWdetector -> nlocs
        #
        #   global attributes:
        #      'Observing Beginning Date' -> YYYY-MM-DD
        #      'Observing Beginning Time' -> HH:MM:SS
        #      The two above attributes get translated to a single datetime value
        #      which becomes the value for each location in datetime@MetaData.
        #      The seconds value is a float number possibly containing fractional seconds.
        #
        #   variables:
        #      LW_wnum(LWchannel) -> channel_wavenumber@VarMetaData
        #      Number the channels starting with 1 assuming that the order
        #      in variables indexed by LWchannel are sequential channel numbers (1..n).
        #
        #      IRLW_Latitude(LWdetector)  -> latitude@MetaData
        #      IRLW_Longitude(LWdetector) -> longitude@MetaData
        #      IRLW_SolarZenith(LWdetector) -> solar_zenith_angle@MetaData
        #      IRLW_SolarAzimuth(LWdetector) -> solar_azimuth_angle@MetaData
        #      IRLW_SatelliteZenith(LWdetector) -> sensor_zenith_angle@MetaData
        #      IRLW_SatelliteAzimuth(LWdetector) -> sensor_azimuth_angle@MetaData
        #
        #      ES_RealLW(LWchannel, Lwdetector) -> brightness_temperature_#@ObsValue
        #      For now, fabricate the QC marks and error esitmate by setting all
        #      locations to 0 for brightness_temperature_#@PreQC and setting all locations
        #      to 2.0 for brightness_temperature_#@ObsError.
        valKey = vName, self.writer.OvalName()
        errKey = vName, self.writer.OerrName()
        qcKey = vName, self.writer.OqcName()

        numFiles = 0
        self.writer._nlocs = 0
        for f in self.filenames:
            numFiles += 1
            ncd = nc.Dataset(f, 'r')
            print(f)
            # Get the number of locations and channels
            nlocs = ncd.dimensions["LWdetector"].size
            nchans = ncd.dimensions["LWchannel"].size
            self.writer._nlocs += nlocs
            self.writer._nvars = nchans
            self.writer._nrecs = 1

            # Channel metadata associated with longwave radiance
            if (numFiles == 1):
                self.xferMdataVar(ncd, 'LW_wnum', 'channel_wavenumber', VarMdata, 'float')

                VarMdata['channel_number'] = self.writer.FillNcVector(
                    np.ma.array(range(1, nchans+1)), 'integer')
                self.units_values['channel_number'] = '1'

            # The file contains a single image with a sub-second scan time. We
            # are currently retaining date-time stamps to the nearest second so
            # for now just grab the beginning scan time and use that for all locations.
            # The variables holding the time offset from 1 Jan, 0000 are problematic
            # since the python datetime package doesn't allow year zero, and it's not
            # clear how days are coverted over several millinnea. The observation
            # date and time are also in global attributes and appear to line up with
            # the file name.
            obsDate = ncd.getncattr("Observing Beginning Date").split("-")
            obsTime = ncd.getncattr("Observing Beginning Time").split(":")
            obsSeconds = timedelta(seconds=int(round(float(obsTime[2]))))
            obsDateTime = datetime(year=int(obsDate[0]), month=int(obsDate[1]),
                                   day=int(obsDate[2]), hour=int(obsTime[0]),
                                   minute=int(obsTime[1])) + obsSeconds
            obsDtimeString = obsDateTime.strftime("%Y-%m-%dT%H:%M:%SZ")

            # Form a vector nlocs long containing the datetime stamp
            if (numFiles == 1):
                LocMdata['datetime'] = self.writer.FillNcVector(
                    np.full((nlocs), obsDtimeString), 'datetime')
                self.units_values['datetime'] = 'ISO 8601 format'
            else:
                LocMdata['datetime'] = np.append(LocMdata['datetime'], self.writer.FillNcVector(np.full((nlocs), obsDtimeString), 'datetime'), axis=0)

            # Read in the latitude and longitude associated with the long wave data
            self.xferLocMdataVar(ncd, numFiles, 'IRLW_Latitude', 'latitude', LocMdata, 'float')
            self.xferLocMdataVar(ncd, numFiles, 'IRLW_Longitude', 'longitude', LocMdata, 'float')

            self.xferLocMdataVar(ncd, numFiles, 'IRLW_Longitude', 'scan_position', LocMdata, 'integer')

            # Read in the instrument meta data associated with the long wave data
            self.xferLocMdataVar(ncd, numFiles, 'IRLW_SolarZenith', 'solar_zenith_angle', LocMdata, 'float')
            self.xferLocMdataVar(ncd, numFiles, 'IRLW_SolarAzimuth', 'solar_azimuth_angle', LocMdata, 'float')
            self.xferLocMdataVar(ncd, numFiles, 'IRLW_SatelliteZenith', 'sensor_zenith_angle', LocMdata, 'float')
            self.xferLocMdataVar(ncd, numFiles, 'IRLW_SatelliteZenith', 'sensor_view_angle', LocMdata, 'float')
            self.xferLocMdataVar(ncd, numFiles, 'IRLW_SatelliteAzimuth', 'sensor_azimuth_angle', LocMdata, 'float')

            # Read in the long wave radiance
            # For now fabricate the QC marks and error estimates
            ncVar = ncd.variables['ES_RealLW']
            lwRadiance = ncVar[:]
            print(self.writer._nlocs)
            H  = 6.6237E-27
            C  = 2.99791E+10
            B  = 1.38024E-16
            C1 = 2.*H*C*C
            C2 = H*C/B
            # GIIRS wavenumbers
            wn_lw=np.arange(700,1130.001,0.625)
            wn_mw=np.arange(1650,2250.001,0.625)
            TBB = lwRadiance*0-999
            
            for iloc in range(nlocs):            
                for ivar in range(1,nchans-1):
                #Hamming apodization for Ch2 - Ch688
                    lwRadiance_apo = 0.23*lwRadiance[ivar-1,iloc] +0.54*lwRadiance[ivar,iloc] + 0.23*lwRadiance[ivar+1,iloc]
                #Radiane to Bright Temperature  
                    TBB[ivar,iloc] = C2*wn_lw[ivar]/math.log(C1*wn_lw[ivar]**3/lwRadiance_apo+1)
                #For the first channel Ch1 and last channel Ch689 in long wave band
                ivar = 0
                lwRadiance_apo = lwRadiance[ivar,iloc] 
                TBB[ivar,iloc] = C2*wn_lw[ivar]/math.log(C1*wn_lw[ivar]**3/lwRadiance_apo+1)  
                ivar = nchans-1
                lwRadiance_apo = lwRadiance[ivar,iloc] 
                TBB[ivar,iloc] = C2*wn_lw[ivar]/math.log(C1*wn_lw[ivar]**3/lwRadiance_apo+1)          
            
            if 'units' in ncVar.ncattrs():
                Units = ncVar.getncattr('units')
            else:
                Units = None
            for ivar in range(nchans):
                varName = "brightness_temperature_%d" % (ivar + 1)
                if (numFiles == 1):
                    self.data[(varName, 'ObsValue')] = self.writer.FillNcVector(TBB[ivar, :], 'float')
                    self.data[(varName, 'PreQC')] = self.writer.FillNcVector(np.full((nlocs), 0), 'integer')
                    self.data[(varName, 'ObsError')] = self.writer.FillNcVector(np.full((nlocs), 2.0), 'float')
                else:
                    self.data[(varName, 'ObsValue')] = np.append(self.data[(varName, 'ObsValue')], self.writer.FillNcVector(TBB[ivar, :], 'float'), axis=0)
                    self.data[(varName, 'PreQC')] = np.append(self.data[(varName, 'PreQC')], self.writer.FillNcVector(np.full((nlocs), 0), 'integer'), axis=0)
                    self.data[(varName, 'ObsError')] = np.append(self.data[(varName, 'ObsError')], self.writer.FillNcVector(np.full((nlocs), 2.0), 'float'), axis=0)
                if Units:
                   self.units_values[varName] = Units

            ncd.close()


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read NSMC GIIRS long wave radiance file(s) and convert'
            ' to a concatenated IODA formatted output file.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of giirs input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="name of ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    writer = iconv.NcWriter(args.output, [], locationKeyList)

    # Read in the giirs lw radiance
    lwrad = LwRadiance(args.input, writer)

    # Add the 'date_time_string' attribute which sets the reference datetime for
    # the observations.
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    # Write the obs data and meta data into the ioda file
    writer.BuildNetcdf(lwrad.data, RecMdata, LocMdata, VarMdata, AttrData, lwrad.units_values)


if __name__ == '__main__':
    main()
