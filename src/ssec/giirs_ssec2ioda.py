#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
import collections
from datetime import datetime, timedelta
import itertools
import math
from pathlib import Path
import sys

import dateutil.parser
import numpy as np
import netCDF4 as nc

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv


class Giirs2Ioda:
    VAR_NAME = "brightness_temperature"

    LOCATION_KEYS = [
        ("latitude", "float"),
        ("longitude", "float"),
        ("datetime", "string")
    ]

    # map from iooda metadata var name to ssec var name
    LOC_MDATA_MAP = {'latitude': 'IRLW_Latitude',
                     'longitude': 'IRLW_Longitude',
                     'solar_zenith_angle': 'IRLW_SolarZenith',
                     'solar_azimuth_angle': 'IRLW_SolarAzimuth',
                     'sensor_zenith_angle': 'IRLW_SatelliteZenith',
                     'sensor_view_angle': 'IRLW_SatelliteZenith',
                     'sensor_azimuth_angle': 'IRLW_SatelliteAzimuth',
                     'cloud_fraction': 'Cloud_Fraction'}

    # Filtering parameters
    SAT_ZEN_MAX = 74  # Maximum allowable IRLW_SatelliteZenith value before filtering

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

        totalFiles = 0  # Total number of files read in
        succFiles = 0  # Number of files with at least one valid obs
        nlocs_tot = 0  # Total localization read in
        self.writer._nlocs = 0
        self.writer._nrecs = 1
        meta_datetime = []
        for f in (ff.strip() for ff in filenames):
            with nc.Dataset(f, 'r') as ncd:
                totalFiles += 1
                try:
                    sat_zen = ncd.variables['IRLW_SatelliteZenith'][:]
                except KeyError as e:
                    print(f"[GIIRS2IODA] Processing {totalFiles}/{len(filenames)} Missing metadata variables. Skipping. -- {Path(f).name} ")
                    continue
                mask = sat_zen < self.SAT_ZEN_MAX
                nlocs = np.count_nonzero(mask)
                if nlocs == 0:
                    print(f"[GIIRS2IODA] Processing {totalFiles}/{len(filenames)} NO VALID OBS. Skipping. -- {Path(f).name} ")
                    continue
                succFiles += 1
                print(f"[GIIRS2IODA] Processing {totalFiles}/{len(filenames)} #locs:{nlocs} -- {Path(f).name}")

                # Initialize data structures based on channel and detector layout
                if (succFiles == 1):
                    # Get number of channels
                    nchans = ncd.dimensions["LWchannel"].size
                    self.writer._nvars = nchans

                    # Vectorized constants for vectorized rad2bt transform
                    # bt = K1/log(1+K3/rad) in native units
                    LW_wnum = np.asarray(ncd.variables["LW_wnum"])
                    K1 = LW_wnum*100*self._K1
                    K1 = K1.astype(np.float32)[:, np.newaxis]  # column vector
                    K3 = (1e5*self._K2*(LW_wnum*100)**3.).astype(np.float32)
                    K3 = K3.astype(np.float32)[:, np.newaxis]  # column vector

                    # Get number of detectors (obs) per-channel
                    ndetectors = ncd.dimensions["LWdetector"].size
                    detector_array = np.arange(1, ndetectors+1, dtype=np.int32)

                    # Determine maximum size of output and pre-allocate output arrays
                    self.nlocs_max = ndetectors * len(filenames)  # Maximum possible locs
                    # obs_vals (nchans, nlocs) will contain all observed values
                    # values are appended for each dataset
                    obs_vals = np.full((nchans, self.nlocs_max), nc.default_fillvals['f4'], dtype=np.float32, order='F')
                    # Note, scan_position is integer valued, but currently must be a float32 to be recognized by UFO/CRTM operator
                    self.LocMdata['scan_position'] = np.full(self.nlocs_max, nc.default_fillvals['f4'], dtype=np.float32)
                    self.units_values['scan_position'] = '1'

                    self.VarMdata['channel_wavenumber'] = self.writer.FillNcVector(LW_wnum, 'float')
                    self.VarMdata['channel_number'] = self.writer.FillNcVector(np.arange(1, nchans+1, dtype=np.int32), 'integer')
                    self.units_values['channel_wavenumber'] = ncd.variables['LW_wnum'].getncattr('units')
                    self.units_values['channel_number'] = '1'
                    for new_key, old_key in self.LOC_MDATA_MAP.items():
                        self._initialize_metadata_key(new_key, old_key, ncd)
                else:
                    # Check this file uses the same dimensions as the initial file
                    this_nchans = ncd.dimensions["LWchannel"].size
                    this_ndetectors = ncd.dimensions["LWdetector"].size
                    if this_nchans != nchans:
                        raise RuntimeError(f"Number of channels:{this_nchans} does not match with initial value channels:{nchans}")
                    if this_ndetectors != ndetectors:
                        raise RuntimeError(f"Number of detectors:{this_ndetectors} does not match with initial value channels:{ndetectors}")

                # The file contains a single image with a sub-second scan time. We
                # are currently retaining date-time stamps to the nearest second so
                # for now just grab the beginning scan time and use that for all locations.
                obsDate = ncd.getncattr("Observing Beginning Date").split("-")
                obsTime = ncd.getncattr("Observing Beginning Time").split(":")
                obsSeconds = timedelta(seconds=int(round(float(obsTime[2]))))
                obsDateTime = datetime(year=int(obsDate[0]), month=int(obsDate[1]),
                                       day=int(obsDate[2]), hour=int(obsTime[0]),
                                       minute=int(obsTime[1])) + obsSeconds
                obsDtimeString = obsDateTime.strftime("%Y-%m-%dT%H:%M:%SZ")

                meta_datetime.extend([obsDtimeString]*nlocs)

                # Read metadata
                for new_key, old_key in self.LOC_MDATA_MAP.items():
                    if old_key in ncd.variables:
                        if new_key not in self.LocMdata:
                            # If we haven't seen this key before initialize it now and set to fillvalue for all previous locs
                            self._initialize_metadata_key(new_key, old_key, ncd)
                        self.LocMdata[new_key][nlocs_tot:nlocs_tot+nlocs] = np.asarray(ncd.variables[old_key])[mask]
                self.LocMdata['scan_position'][nlocs_tot:nlocs_tot+nlocs] = detector_array[mask]  # detector number

                # Read in the long wave radiance
                # For now fabricate the QC marks and error estimates
                ncVar = ncd.variables['ES_RealLW']
                lwRad = np.array(ncVar[:, mask])

                lwRadiance_apo = np.copy(lwRad)
                lwRadiance_apo[1:-1, :] = 0.23*lwRad[:-2, :] + 0.54*lwRad[1:-1, :] + 0.23*lwRad[2:, :]
                valid = (lwRadiance_apo > 0) & (lwRadiance_apo < 300)
                # Vectorized conversion: valid radiances to brightness temperature
                shape = (nchans, nlocs)
                np.place(obs_vals[:, nlocs_tot:nlocs_tot+nlocs], valid,
                         np.broadcast_to(K1, shape)[valid] / np.log1p(np.broadcast_to(K3, shape)[valid]/lwRadiance_apo[valid]))

                nlocs_tot += nlocs

        # Write final data arrays as NcVectors
        self.writer._nlocs = nlocs_tot
        self.LocMdata['datetime'] = self.writer.FillNcVector(meta_datetime, 'datetime')
        self.units_values['datetime'] = 'ISO 8601 format'
        self.LocMdata = {k: v[:nlocs_tot] for k, v in self.LocMdata.items()}  # Remove unused extra space in metadata
        for ivar in range(nchans):
            varname = f"brightness_temperature_{ivar+1}"
            self.units_values[varname] = 'K'
            self.data[(varname, 'ObsValue')] = self.writer.FillNcVector(obs_vals[ivar, :nlocs_tot], 'float')
            self.data[(varname, 'PreQC')] = self.writer.FillNcVector(np.full(nlocs_tot, 0, dtype=np.int32), 'integer')
            self.data[(varname, 'ObsError')] = self.writer.FillNcVector(np.full(nlocs_tot, 2., dtype=np.float32), 'float')
        print(f"[GIIRS2IODA] Processed {nlocs_tot} total locs from {succFiles}/{totalFiles} valid files.")

    def writeIODA(self):
        self.writer.BuildNetcdf(self.data, self.LocMdata, self.VarMdata, self.AttrData, self.units_values)

    # Private Methods

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

    def _initialize_metadata_key(self, new_key, old_key, ncd):
        """
        Common initialization for LocMdata metadata for new variable names
        new_key - IODA variable name
        old_key - GIIRS SSEC NetCDF variable name
        ncd - GIIRS NetCDF scan file
        """
        if old_key in ncd.variables:
            self.LocMdata[new_key] = np.full(self.nlocs_max, nc.default_fillvals['f4'], dtype=np.float32)
            if 'units' in ncd.variables[old_key].ncattrs():
                self.units_values[new_key] = ncd.variables[old_key].getncattr('units')


def main():
    parser = argparse.ArgumentParser(
        description=(
            'Read SSEC GIIRS long wave radiance file(s) and convert'
            ' to a concatenated IODA formatted output file converting radiance'
            ' to brightness-temperature units.')
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

    conv = Giirs2Ioda(args.date, args.output)
    conv.readInFiles(args.input)
    conv.writeIODA()


if __name__ == '__main__':
    main()
