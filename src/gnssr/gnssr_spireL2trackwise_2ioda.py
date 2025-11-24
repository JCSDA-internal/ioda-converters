#!/usr/bin/env python3
#
# (C) British Crown Copyright 2020 Met Office
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import os, sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import re
import time
import yaml


IODA_CONV_PATH = Path(__file__).parent / "../lib/python3.12/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent / '..' / 'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

t0 = time.perf_counter()

# Read the user configured yaml file
parser = argparse.ArgumentParser(
        description=("Read GNSS-R L2 wind-speed file(s) and Converter from original"
                     "netCDF format for observations to IODA netCDF format.")
)
parser.add_argument('-i', '--input',
                    help="Requires a path to a YAML configuration file",
                    type=str, required=True)
args = parser.parse_args()

with open(args.input, "r") as yamlfile:
    yaml_data = yaml.load(yamlfile, Loader=yaml.FullLoader)
print(yaml_data)

# set up the IODA writer for each 6h DA window to produce 4 files one for each 00, 06, 12, 18 hour
qflg = yaml_data['qflg']
da_window_length = yaml_data['da_window_length'] # hours
da_window_shift = yaml_data['da_window_shift'] # hours
datetime_start = datetime.strptime(yaml_data['datetime_start'], '%Y-%m-%dT%H:%M:%S')
datetime_end = datetime.strptime(yaml_data['datetime_end'], '%Y-%m-%dT%H:%M:%S')
out_dir = yaml_data['out_dir']
in_dir = yaml_data['in_dir']

assim_datetimes = []
da_time = datetime_start
while da_time <= datetime_end:
    assim_datetimes.append(da_time)
    da_time += timedelta(hours=da_window_length)

# Define the location describing attributes of the variable(s)
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "string")
]

obsvars = {
    'windSpeed': 'wind_speed',
}

attr_data = {
    'converter': os.path.basename(__file__),
}

# Set up an empty dictionary which will defines dimension size
dim_dict = {
}

# Set up a dictionary which defines the variable dimensions (including metadata variables)
var_dims = {
    'windSpeed': ['Location'],
}


class GnssrL2(object):

    def __init__(self, gnssr_dir, assim_datetimes):
        self.time_idxs = None
        self.assim_datetimes = assim_datetimes
        self.gnssr_dir = gnssr_dir
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.gnssrData = defaultdict(lambda: defaultdict(dict))

    def __del__(self):
        print(f'GNSS-R L2 ocean windspeed object is done and deleted now.')

    def set_preqc_flags(self):
        # In UFO QC flag = 0: observation passes quality check. 
        # QC flag = 11: observation fails pre-processing QC
        qflgs = self.gnssrData['sample_flags']
        dims = self.gnssrData['wind_speed'].shape
        preqc = np.zeros(dims)
        preqc[np.where(qflgs == 0)] = 11
        #preqc[np.where(self.gnssrData['wind_speed'] == -9999)] = 11
        self.gnssrData['preqc'] = preqc.astype('int32')
        
    def read_gnssr_files(self):
        # Walk through the directory and read in all the GNSS-R files
        # Also findout how many total nlocs we will hold in the dictionary
        nlocs = 0
        print(f'{self.gnssr_dir}')
        self.gnssrData["file_list"] = []
        for root, dirs, files in os.walk(self.gnssr_dir, topdown=True):       
            self.gnssrData["file_list"] += [os.path.join(root, filename) for filename in files 
                                            if "spire_gnss-br_L2_gbrOcn" in filename]
        self.gnssrData["file_list"] = sorted(self.gnssrData["file_list"])
        
        for filename in self.gnssrData["file_list"]:
            try:
                dataset_input = nc.Dataset(filename, 'r')
            except IOError:
                print(f"Error opening netCDF file: {filename}")
                print("Skipping this file and going to next")
                continue
            nc_dim_dict = dataset_input.dimensions
            nlocs += nc_dim_dict["sample_time"].size
        print(self.gnssrData["file_list"])
        
        # Set total number of locations
        self.gnssrData["nlocs"] = nlocs
        
        # Define and initialise the dictionary elements
        self.gnssrData["obs_times"] = np.zeros(nlocs, dtype=np.object_)
        self.gnssrData["lats"] = np.zeros(nlocs)
        self.gnssrData["lons"] = np.zeros(nlocs)
        self.gnssrData["sample_flags"] = np.zeros(nlocs)  # Wind confidence flag: 1 - ok, 0 - questionable
        self.gnssrData["wind_speed"] = np.zeros(nlocs)
        self.gnssrData["wind_speed_error"] = np.zeros(nlocs)
        self.gnssrData["incidence_angle"] = np.zeros(nlocs)
        self.gnssrData["quality_flags"] = np.zeros(nlocs)
        self.gnssrData["quality_ice_flag"] = np.zeros(nlocs)
        self.gnssrData["sp_coast_distance"] = np.zeros(nlocs)  # m
        self.gnssrData["reflect_snr_at_sp"] = np.zeros(nlocs)
        self.gnssrData["sigma0_dB"] = np.zeros(nlocs)
        self.gnssrData["mss"] = np.zeros(nlocs)
        self.gnssrData["rx_id"] = np.zeros(nlocs)
        self.gnssrData["tx_prn"] = np.zeros(nlocs)
        self.gnssrData["tx_svn"] = np.zeros(nlocs)
        self.gnssrData["tx_id"] = np.full(nlocs, '', dtype=np.object_)    # constellation string e.g. G = GPS
        self.gnssrData["gnss_constellation_id"] = np.zeros(nlocs)         # set from tx_id string
        self.gnssrData["code"] = np.full(nlocs, '', dtype=np.object_)     # gnsss code type e.g. L1_CA
        self.gnssrData["constellation"] = np.full(nlocs, '', dtype=np.object_)     # e.g. spire
        
        ns = 0
        for filename in self.gnssrData["file_list"]:
            dataset_input = nc.Dataset(filename, 'r')
            nc_attrs = dataset_input.__dict__
            nc_dim_dict = dataset_input.dimensions
            nlocs_local = nc_dim_dict["sample_time"].size
            
            # Get metadata values
            # -------------------
            # Get first Time values as we will filter the obs based on the time and DA window
            # Format times
            timestamp_utc = dataset_input['sample_time'][:]  # 1D [time]
            time_deltavals = np.array([timedelta(seconds=i) for i in timestamp_utc])
            file_start_timestring = nc_attrs['file_start_time'][0:26]
            file_start_datetime = datetime.strptime(file_start_timestring, '%Y-%m-%dT%H:%M:%S.%f')
            self.gnssrData["obs_times"][ns:ns+nlocs_local]  = file_start_datetime + time_deltavals
            
            print(f'ns = {ns}')
            print(f'nlocs = {nlocs}')
            
            self.gnssrData["sample_flags"][ns:ns+nlocs_local] = np.array(dataset_input['wind_confidence'][:])
            self.gnssrData["wind_speed"][ns:ns+nlocs_local] = np.array(dataset_input['wind'][:])
            self.gnssrData["wind_speed_error"][ns:ns+nlocs_local] = np.array(dataset_input['wind_std'][:])
            self.gnssrData["lons"][ns:ns+nlocs_local] = np.array(dataset_input['sp_lon'][:])
            self.gnssrData["lats"][ns:ns+nlocs_local] = np.array(dataset_input['sp_lat'][:])
            self.gnssrData["incidence_angle"][ns:ns+nlocs_local] = np.array(dataset_input['sp_incidence_angle'][:])
            self.gnssrData["quality_flags"][ns:ns+nlocs_local] = np.array(dataset_input['quality_flags'][:])
            self.gnssrData["quality_ice_flag"][ns:ns+nlocs_local] = np.array(dataset_input['quality_ice_flag'][:])
            self.gnssrData["sp_coast_distance"][ns:ns+nlocs_local] = np.array(dataset_input['sp_coast_distance'][:] * -1000.)  # convert to m from km, positive values over sea
            self.gnssrData["reflect_snr_at_sp"][ns:ns+nlocs_local] = np.array(dataset_input['reflect_snr_at_sp'][:]) 
            self.gnssrData["sigma0_dB"][ns:ns+nlocs_local] = np.array(dataset_input['sigma0_dB'][:]) 
            self.gnssrData["mss"][ns:ns+nlocs_local] = np.array(dataset_input['mss'][:])
            # Get values from global file attributes
            rx_id_int = int(re.findall(r'\d+', nc_attrs["rx_id"])[0])  #  Convert to int e.g. FM172 -> 172
            self.gnssrData["rx_id"][ns:ns+nlocs_local] = np.full(nlocs_local, rx_id_int)
            self.gnssrData["tx_prn"][ns:ns+nlocs_local] = np.full(nlocs_local, nc_attrs["tx_prn"])
            self.gnssrData["tx_svn"][ns:ns+nlocs_local] = np.full(nlocs_local, nc_attrs["tx_svn"])
            self.gnssrData["code"][ns:ns+nlocs_local] = nc_attrs["code"]
            self.gnssrData["constellation"][ns:ns+nlocs_local] = nc_attrs["constellation"]
            tx_id_str = re.findall(r'\D+', nc_attrs["tx_id"])[0]  #  Convert to string e.g. G9 -> G
            self.gnssrData["tx_id"][ns:ns+nlocs_local] = tx_id_str
            # fill constellation ID
            match tx_id_str:
                case "G":
                    # GPS / 401
                    self.gnssrData["gnss_constellation_id"][ns:ns+nlocs_local] = np.full(nlocs_local, 401)
                case "R":
                    # GLONASS / 402
                    self.gnssrData["gnss_constellation_id"][ns:ns+nlocs_local] = np.full(nlocs_local, 402)
                case "E":
                    # GALILEO / 403
                    self.gnssrData["gnss_constellation_id"][ns:ns+nlocs_local] = np.full(nlocs_local, 403)               
            
            dataset_input.close()
            ns += nlocs_local
    
        print(self.gnssrData) 

    def setup_ioda_vars(self):
        loc_idxs = self.loc_idxs
        
        # set up variable names for IODA
        for iodavar in ['windSpeed']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'latitude longitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'latitude longitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'latitdue longitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'm/s'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'm/s'
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'
            self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = -999.
            self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = -999.
            self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = -999

        # write global attributes out
        # The 'date_time_string' attribute sets the reference datetime for
        # the observations.
        dim_dict["Location"] = len(loc_idxs)  # nc_dim_dict['sample'].size
        attr_data["nlocs"] = np.int64(dim_dict["Location"])
        attr_data["date_time"] = (self.gnssrData["obs_times"][loc_idxs][0]).strftime('%Y-%m-%dT%H:%M:%SZ')
        attr_data["gnssr_l2_files_used"] = [os.path.basename(p) for p in self.gnssrData["file_list"]]
        datetime_str = np.array([i.strftime('%Y-%m-%dT%H:%M:%SZ') for
                                 i in self.gnssrData["obs_times"][loc_idxs]], dtype=np.object_)
        print(datetime_str)
        # add observation metadata variables
        self.outdata[('dateTime', 'MetaData')] = datetime_str
        self.outdata[('latitude', 'MetaData')] = self.gnssrData["lats"][loc_idxs].astype('float32')
        self.outdata[('longitude', 'MetaData')] = self.gnssrData["lons"][loc_idxs].astype('float32')
        self.outdata[('sample_flags','MetaData')] = self.gnssrData["sample_flags"][loc_idxs].astype('int32')
        self.outdata[('specularPointIncidenceAngle', 'MetaData')] = self.gnssrData["incidence_angle"][loc_idxs].astype('float32')
        self.outdata[('qualityFlags', 'MetaData')] = self.gnssrData["quality_flags"][loc_idxs].astype('int32')
        self.outdata[('qualityIceFlag', 'MetaData')] = self.gnssrData["quality_ice_flag"][loc_idxs].astype('int32')
        self.outdata[('distanceToCoastline', 'MetaData')] = self.gnssrData["sp_coast_distance"][loc_idxs].astype('float32')
        self.outdata[('signalToNoiseRatio', 'MetaData')] = self.gnssrData["reflect_snr_at_sp"][loc_idxs].astype('float32')
        self.outdata[('sigma0', 'MetaData')] = self.gnssrData["sigma0_dB"][loc_idxs].astype('float32')
        self.outdata[('meanSquareSlope', 'MetaData')] = self.gnssrData["mss"][loc_idxs].astype('float32')
        self.outdata[('windSpeedStandardDeviation', 'MetaData')] = self.gnssrData["wind_speed_error"][loc_idxs].astype('float32')
        self.outdata[('satelliteReceiverId', 'MetaData')] = self.gnssrData["rx_id"][loc_idxs].astype('int32')
        self.outdata[('satelliteTransmitterId', 'MetaData')] = self.gnssrData["tx_prn"][loc_idxs].astype('int32')
        self.outdata[('gnssSpaceVehicleNumber', 'MetaData')] = self.gnssrData["tx_svn"][loc_idxs].astype('int32')
        self.outdata[('gnssConstellation', 'MetaData')] = self.gnssrData["gnss_constellation_id"][loc_idxs].astype('int32')
        self.outdata[('gnssCodeType', 'MetaData')] = self.gnssrData["code"][loc_idxs]
        self.outdata[('satelliteConstellation', 'MetaData')] = self.gnssrData["constellation"][loc_idxs]
        
        # add output variables
        for iodavar in ['windSpeed']:
            # We populate each preqc variable with the qflg value given in the GNSS-R L2 file. Hence, the qflg MetaData
            # and PreQC/wind_speed in IODA file will have the same values but different dimensions. Each value in the
            # PreQC["wind_speed"][t] = qflg[t] value.
            ws_flat_values= np.array(self.gnssrData["wind_speed"][loc_idxs]).astype('float32')
            ws_flat_errors = np.array(self.gnssrData["wind_speed_error"][loc_idxs]).astype('float32')
            ws_flat_preqc = np.array(self.gnssrData["preqc"][loc_idxs]).astype('int32')
            self.outdata[self.varDict[iodavar]['valKey']] = ws_flat_values
            self.outdata[self.varDict[iodavar]['errKey']] = ws_flat_errors
            self.outdata[self.varDict[iodavar]['qcKey']] = ws_flat_preqc

def main():
    # Read in the entire GNSS-R L2 wind-speed file data
    scf = GnssrL2(yaml_data['in_dir'], assim_datetimes)  # Read in GNSS-R L2 ws data
    scf.read_gnssr_files()
    scf.set_preqc_flags()
     
    # Iterate over all the DA windows
    for current_da_time in scf.assim_datetimes:
        print(f'Current DA time = {current_da_time}')
        # Get indices for observations to keep in the current DA window
        all_datetimes = scf.gnssrData["obs_times"]
        # find time indices in 0 column which we need to select for this DA window
        # Need to ensure observation can't occure in multiple DA windows
        scf.time_idxs = np.array([i for i, o in enumerate(all_datetimes)
                                  if ((o-current_da_time).total_seconds() / 3600. <   da_window_shift and
                                      (o-current_da_time).total_seconds() / 3600. >= -da_window_shift)])
        print(f"time_idx = {scf.time_idxs.shape}")
        if len(scf.time_idxs) == 0:
            print(f'No data matches requested time interval')
            continue
        
        # Remove data for which the quality flag is set
        if qflg:
            # Remove all data that fails pre-processing QC. Currently this includes data where GNSS-R quality_flags = 1.
            idx_qc_window = np.where(scf.gnssrData["preqc"][scf.time_idxs] == 0)
            scf.loc_idxs = scf.time_idxs[idx_qc_window]
        else:
            scf.loc_idxs = scf.time_idxs
        print(f'QC flags are {qflg}, the total number of observations in this DA window are {len(scf.time_idxs)} '
              f'with the number passing QC = {len(scf.loc_idxs)}')
        
        scf.setup_ioda_vars()
        attr_data["date_time_string"] = current_da_time.strftime('%Y-%m-%dT%H:%M:%SZ')
        out_file_name = 'gnssr_windspeed_L2_' + current_da_time.strftime('%Y%m%dT%H%MZ') + ".nc4"
        out_file_path = os.path.join(out_dir, out_file_name)
        writer = iconv.IodaWriter(out_file_path, locationKeyList, dim_dict)
        writer.BuildIoda(scf.outdata, var_dims, scf.varAttrs, attr_data)  # write the output file


                
    del scf
    t1 = time.perf_counter()
    print(f'Total time taken to run the code: {(t1-t0)} seconds or {(t1-t0)/60.} minutes')


if __name__ == '__main__':
    main()
