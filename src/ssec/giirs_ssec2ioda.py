#!/usr/bin/env python

#
# (C) Copyright 2019-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
import collections
from datetime import datetime, timedelta
import sys
import math
from pathlib import Path

import dateutil.parser
import numpy as np
import netCDF4 as nc

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv


class LwRadiance:
    VAR_NAME = "brightness_temperature"

    LOCATION_KEYS = [
        ("latitude", "float"),
        ("longitude", "float"),
        ("datetime", "string")
    ]

    # GIIRS wavenumbers
    WN_LW = np.arange(700, 1130.001, 0.625)
    WN_MW = np.arange(1650, 2250.001, 0.625)

    def __init__(self, center_datetime, out_file):
        """
        Arguments:
         center_datetime: Window center (type: datetime or string representation)
         out_file: Name of IODA file to write (type: str or Path)
        """
        if isinstance(center_datetime, str):
            center_datetime = datetime.strptime(center_datetime, '%Y%m%d%H')
        self.center_datetime = center_datetime
        self.outfile = Path(out_file)
        self.writer = iconv.NcWriter(str(out_file), [], self.LOCATION_KEYS)

        self.data = collections.defaultdict(dict)
        self.units_values = {}  # cannot use an ordered dict for this
        self.LocMdata = collections.defaultdict(dict)
        self.VarMdata = collections.defaultdict(dict)
        self.AttrData = collections.defaultdict(dict)
        self.AttrData['date_time_string'] = self.center_datetime.strftime("%Y-%m-%dT%H:%M:%SZ")

    def readInFiles(self, filenames):
        # This method will transfer variable data from the input file into a
        # dictionary (self.data) that the netcdf writer understands.
        #
        # Input:
        #  Filenames list of filenames to process
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
        valKey = self.VAR_NAME, self.writer.OvalName()
        errKey = self.VAR_NAME, self.writer.OerrName()
        qcKey = self.VAR_NAME, self.writer.OqcName()

        numFiles = 0
        self.writer._nlocs = 0

        sat_zen_max = 74
        detector_number = 128
        detector_array = np.arange(1, detector_number+1, dtype='i4')

        #Configure channel variables
        nchans = 689
        self.writer._nvars = nchans
        self.writer._nrecs = 1

        #Vectorized constants for vectorized rad2bt transform
        # bt = K1/log(1+K3/rad) in native units
        K1 = self.WN_LW*100*self._K1
        K1 = K1.astype(np.float32)[:, np.newaxis] #column vector
        K3 = (1e5*self._K2*(self.WN_LW*100)**3.).astype(np.float32)
        K3 = K3.astype(np.float32)[:, np.newaxis] #column vector

        #obs_vals (nchans, nlocs) will contain all observed values
        # values are appended for each dataset
        obs_vals = np.empty((nchans,0), dtype=np.float32, order='F')
        meta_datetime = []

        for f in (ff.strip() for ff in filenames):
            with nc.Dataset(f, 'r') as ncd:
                sat_zen = ncd.variables['IRLW_SatelliteZenith'][:]
                mask = (sat_zen < sat_zen_max)
                nlocs = len(sat_zen[mask])
                if nlocs == 0:
                    print("[GIIRS2IODA] NO VALID OBS. Skipping.")
                    continue
                numFiles += 1
                print(f"[GIIRS2IODA] Processing {numFiles}/{len(filenames)} #locs:{nlocs} -- {Path(f).name}")

                # Channel metadata associated with longwave radiance
                if (numFiles == 1):
                    self._xferMdataVar(ncd, 'LW_wnum', 'channel_wavenumber', self.VarMdata, 'float')
                    self.VarMdata['channel_number'] = self.writer.FillNcVector(np.arange(1, nchans+1,dtype=np.int32), 'integer')
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

                meta_datetime.extend([obsDtimeString]*nlocs)

                # Read in the latitude and longitude associated with the long wave data
                self._xferLocMdataVar(ncd, numFiles, 'IRLW_Latitude', 'latitude', self.LocMdata, 'float', mask)
                self._xferLocMdataVar(ncd, numFiles, 'IRLW_Longitude', 'longitude', self.LocMdata, 'float', mask)
                # detector number
                self._xferLocMdataVar_detector(ncd, numFiles, detector_array*1.0, 'scan_position', self.LocMdata, 'float', mask)

                # Read in the instrument meta data associated with the long wave data
                self._xferLocMdataVar(ncd, numFiles, 'IRLW_SolarZenith', 'solar_zenith_angle', self.LocMdata, 'float', mask)
                self._xferLocMdataVar(ncd, numFiles, 'IRLW_SolarAzimuth', 'solar_azimuth_angle', self.LocMdata, 'float', mask)
                self._xferLocMdataVar(ncd, numFiles, 'IRLW_SatelliteZenith', 'sensor_zenith_angle', self.LocMdata, 'float', mask)
                self._xferLocMdataVar(ncd, numFiles, 'IRLW_SatelliteZenith', 'sensor_view_angle', self.LocMdata, 'float', mask)
                self._xferLocMdataVar(ncd, numFiles, 'IRLW_SatelliteAzimuth', 'sensor_azimuth_angle', self.LocMdata, 'float', mask)

                self._xferLocMdataVar(ncd, numFiles, 'Cloud_Fraction', 'cloud_fraction', self.LocMdata, 'float', mask)

                # Read in the long wave radiance
                # For now fabricate the QC marks and error estimates
                ncVar = ncd.variables['ES_RealLW']
                lwRad = np.array(ncVar[:, mask])
                print("[GIIRS2IODA] #Locs: {self.writer._nlocs}")

                TBB = np.full(lwRad.shape, nc.default_fillvals['f4'], dtype=np.float32)

                lwRadiance_apo = np.copy(lwRad)
                lwRadiance_apo[1:-1, :] = 0.23*lwRad[:-2, :] + 0.54*lwRad[1:-1, :] + 0.23*lwRad[2:, :]
                valid = (lwRadiance_apo > 0) & (lwRadiance_apo < 300)
                #Vectorized conversion: valid radiances to brightness temperature
                np.place(TBB, valid, np.broadcast_to(K1,TBB.shape)[valid] / np.log1p( np.broadcast_to(K3,TBB.shape)[valid]/lwRadiance_apo[valid] ))

                obs_vals = np.append(obs_vals, TBB, axis=1)

        #Write final data arrays as NcVectors
        nlocs = obs_vals.shape[1]
        self.writer._nlocs = nlocs
        self.LocMdata['datetime'] = self.writer.FillNcVector(meta_datetime, 'datetime')
        self.units_values['datetime'] = 'ISO 8601 format'
        for ivar in range(nchans):
            varname = f"brightness_temperature_{ivar+1}"
            self.units_values[varname] = 'K'
            self.data[(varname,'ObsValue')] = self.writer.FillNcVector(obs_vals[ivar,:],'float')
            self.data[(varname,'PreQC')] = self.writer.FillNcVector(np.full(nlocs,0,dtype=np.int32),'integer')
            self.data[(varname,'ObsError')] = self.writer.FillNcVector(np.full(nlocs,2.,dtype=np.float32),'float')


    def writeIODA(self):
        self.writer.BuildNetcdf(self.data, self.LocMdata, self.VarMdata, self.AttrData, self.units_values)

    # Private Methods

    def _xferMdataVar(self, ncd, inName, outName, mData, dType):
        # This method adds a variable to the given metadata dictionary element.
        ncVar = ncd.variables[inName]
        mData[outName] = self.writer.FillNcVector(ncVar[:], dType)
        if 'units' in ncVar.ncattrs():
            self.units_values[outName] = ncVar.getncattr('units')

    def _xferLocMdataVar(self, ncd, numFiles, inName, outName, mData, dType, mask):
        # This method adds a variable to the given metadata dictionary element.
        ncVar = ncd.variables[inName]
        if (numFiles == 1):
            mData[outName] = self.writer.FillNcVector(ncVar[mask], dType)
            if 'units' in ncVar.ncattrs():
                self.units_values[outName] = ncVar.getncattr('units')
        else:
            mData[outName] = np.append(mData[outName], self.writer.FillNcVector(ncVar[mask], dType), axis=0)

    def _xferLocMdataVar_detector(self, ncd, numFiles, detector, outName, mData, dType, mask):
        # This method adds a variable to the given metadata dictionary element.
        ncVar = detector
        if (numFiles == 1):
            mData[outName] = self.writer.FillNcVector(ncVar[mask], dType)
            self.units_values[outName] = "1"
        else:
            mData[outName] = np.append(mData[outName], self.writer.FillNcVector(ncVar[mask], dType), axis=0)

    # Radiance to brightness temp conversion
    _H_PLANCK = 6.62606957 * 1e-34  # SI-unit = [J*s]
    _K_BOLTZMANN = 1.3806488 * 1e-23  # SI-unit = [J/K]
    _C_SPEED = 2.99792458 * 1e8  # SI-unit = [m/s]
    _K1 = _H_PLANCK * _C_SPEED / _K_BOLTZMANN
    _K2 = 2 * _H_PLANCK * _C_SPEED**2

    @classmethod
    def _rad2bt(cls, wavnum, radiance):
        # Radiance to brightness temp conversion
        K3 = cls._K2 * wavnum**3
        bt = cls._K1 * wavnum / np.log1p(K3/radiance)
        return bt


def main():
    parser = argparse.ArgumentParser(
        description=(
            'Read NSMC GIIRS long wave radiance file(s) and convert'
            ' to a concatenated IODA formatted output file.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="file name of giirs input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path to ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)
    args = parser.parse_args()

    # Read in the giirs lw radiance
    lwrad = LwRadiance(args.date, args.output)
    lwrad.readInFiles(args.input)
    lwrad.writeIODA()


if __name__ == '__main__':
    main()
