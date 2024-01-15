#!/usr/bin/env python3
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# Spire Global UK Ltd
# www.spire.com


import os, sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import time
import yaml


IODA_CONV_PATH = Path(__file__).parent / "@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent / '..' / 'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict

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
while da_time < datetime_end:
    assim_datetimes.append(da_time)
    da_time += timedelta(hours=da_window_length)

# Define the location describing attributes of the variable(s)
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

obsvars = {
    'wind_speed': 'wind_speed',
}

attr_data = {
    'converter': os.path.basename(__file__),
}

# Set up an empty dictionary which will defines dimention size
dim_dict = {
}

# Set up a dictionary which defines the variable dimensions (including metadata variables)
var_dims = {
    'wind_speed': ['nlocs'],
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
        incidence_angle = self.gnssrData['incidence_angle']
        dims = self.gnssrData['wind_speed'].shape
        preqc = np.zeros(dims)
        preqc[np.where(qflgs != 0)] = 11  # data over land and failing GNSS-R ws qc
        preqc[np.where(incidence_angle > 60.)] = 11  # data with incidence angle over 60 deg
        preqc[np.where(self.gnssrData['wind_speed'] == -9999)] = 11
        self.gnssrData['preqc'] = preqc.astype('int32')
        
    def read_gnssr_files(self):
        # Walk through the directory and read in all the GNSS-R files
        # Also findout how many total nlocs we will hold in the dictionary
        nlocs = 0
        print(f'{self.gnssr_dir}')
        self.gnssrData["file_list"] = []
        for root, dirs, files in os.walk(self.gnssr_dir, topdown=True):
            self.gnssrData["file_list"] += [os.path.join(root, filename) for filename in files if "cyg.ddmi"
                                             in filename and "l2.wind-mss.a31.d32" in filename and
                                             datetime.strptime(filename[10:18],'%Y%m%d') <= self.assim_datetimes[-1]
                                             and
                                             datetime.strptime(filename[10:18], '%Y%m%d') >= self.assim_datetimes[0]]
        self.gnssrData["file_list"] = sorted(self.gnssrData["file_list"])

        for filename in self.gnssrData["file_list"]:
            try:
                dataset_input = nc.Dataset(filename, 'r')
            except IOError:
                print(f"Error opening netCDF file: {filename}")
                print("Skipping this file and going to next")
                continue
            nc_dim_dict = dataset_input.dimensions
            nlocs += nc_dim_dict["sample"].size
        print(self.gnssrData["file_list"])
 
        # get global attributes from the last file
        self.gnssrData["nlocs"] = nlocs
        nc_attrs = dataset_input.__dict__
        self.gnssrData["satellite"] = nc_attrs["project"]
        self.gnssrData["sensor"] = nc_attrs["sensor"]
        self.gnssrData["observation_type"] = "Wind Speed"
            
        # Define and initialise the dictionary elements
        self.gnssrData["obs_times"] = np.zeros(nlocs, dtype=np.object_)
        self.gnssrData["lats"] = np.zeros(nlocs)
        self.gnssrData["lons"] = np.zeros(nlocs)
        self.gnssrData["sample_flags"] = np.zeros(nlocs)  # General QC flag, 0 - pass, 1 - no pass
        self.gnssrData["fds_sample_flags"] = np.zeros(nlocs)  # Fully developed seas QC flags, 0 - pass
        self.gnssrData["yslf_sample_flags"] = np.zeros(nlocs)  # Young Seas QC flags, 0 - pass
        self.gnssrData["wind_speed"] = np.zeros(nlocs)
        self.gnssrData["wind_speed_error"] = np.zeros(nlocs)
        self.gnssrData["wind_speed_bias"] = np.zeros(nlocs)
        self.gnssrData["incidence_angle"] = np.zeros(nlocs)  # L1 DDM observations are filtered against inc_angle > 60
                                                              # Inclduded here in case L2 also should be filtered based
                                                              # on this criteria
        self.gnssrData["filename_used"] = np.zeros(nlocs)
   
        ns = 0
        for filename in self.gnssrData["file_list"]:
            dataset_input = nc.Dataset(filename, 'r')
            nc_attrs = dataset_input.__dict__
            nc_dim_dict = dataset_input.dimensions
            nlocs_local = nc_dim_dict["sample"].size
            
            # Get metadata values
            # -------------------
            # Get first Time values as we will filter the obs based on the time and DA window
            # Format times
            timestamp_utc = dataset_input['sample_time'][:]  # 1D [time]
            time_deltavals = np.array([timedelta(seconds=i) for i in timestamp_utc])
            file_start_timestring = nc_attrs['time_coverage_start'][0:26]
            file_start_datetime = datetime.strptime(file_start_timestring, '%Y-%m-%dT%H:%M:%S.%f')
            self.gnssrData["obs_times"][ns:ns+nlocs_local]  = file_start_datetime + time_deltavals

            print(f'ns = {ns}')
            print(f'nlocs = {nlocs}')

            self.gnssrData["sample_flags"][ns:ns+nlocs_local] = np.array(dataset_input['sample_flags'][:])
            self.gnssrData["fds_sample_flags"][ns:ns+nlocs_local] = np.array(dataset_input['fds_sample_flags'][:])
            self.gnssrData["yslf_sample_flags"][ns:ns+nlocs_local] = np.array(dataset_input['yslf_sample_flags'][:])
            self.gnssrData["wind_speed"][ns:ns+nlocs_local] = np.array(dataset_input['wind_speed'][:])
            self.gnssrData["wind_speed_error"][ns:ns+nlocs_local] = np.array(dataset_input['wind_speed_uncertainty'][:])
            self.gnssrData["wind_speed_bias"][ns:ns+nlocs_local] = np.array(dataset_input['wind_speed_bias'][:])
            self.gnssrData["lons"][ns:ns+nlocs_local] = np.array(dataset_input['lon'][:])
            self.gnssrData["lats"][ns:ns+nlocs_local] = np.array(dataset_input['lat'][:])
            self.gnssrData["incidence_angle"][ns:ns+nlocs_local] = np.array(dataset_input['incidence_angle'][:])

            dataset_input.close()
            ns += nlocs_local

        print(self.gnssrData)

    def setup_ioda_vars(self):
        loc_idxs = self.loc_idxs
        
        # set up variable names for IODA
        for iodavar in ['wind_speed']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['biasKey'] = iodavar, iconv.ObiastermName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'latitude longitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'latitude longitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'latitdue longitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'latitude longitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'm/s'
            self.varAttrs[iodavar, iconv.ObiastermName()]['units'] = 'm/s'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'm/s'
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'
            self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = -999.
            self.varAttrs[iodavar, iconv.ObiastermName()]['_FillValue'] = -999.
            self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = -999.
            self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = -999

        # compute decimal hour
        time_diff = self.gnssrData["obs_times"][loc_idxs] - self.gnssrData["obs_times"][loc_idxs][0]
        helper = np.vectorize(lambda x: x.total_seconds())
        self.gnssrData["time_decimal_hour"] = helper(time_diff)

        # write global attributes out
        # The 'date_time_string' attribute sets the reference datetime for
        # the observations.
        dim_dict["nlocs"] = len(loc_idxs)  # nc_dim_dict['sample'].size
        attr_data["nlocs"] = np.int64(dim_dict["nlocs"])
        attr_data["observation_type"] = self.gnssrData["observation_type"]
        attr_data["satellite"] = self.gnssrData["satellite"]
        attr_data["sensor"] = self.gnssrData["sensor"]
        attr_data["date_time"] = (self.gnssrData["obs_times"][loc_idxs][0]).strftime('%Y-%m-%dT%H:%M:%SZ')
        attr_data["gnssr_l2_files_used"] = self.gnssrData["file_list"]
        
        datetime_str = np.array([i.strftime('%Y-%m-%dT%H:%M:%SZ') for
                                 i in self.gnssrData["obs_times"][loc_idxs]], dtype=np.object_)
        print(datetime_str)
        # add observation metadata variables
        self.outdata[('datetime', 'MetaData')] = datetime_str
        self.outdata[('decimal_hour', 'MetaData')] = self.gnssrData["time_decimal_hour"].astype('float32')
        self.outdata[('latitude', 'MetaData')] = self.gnssrData["lats"][loc_idxs].astype('float32')
        self.outdata[('longitude', 'MetaData')] = self.gnssrData["lons"][loc_idxs].astype('float32')
        self.outdata[('sample_flags','MetaData')] = self.gnssrData["sample_flags"][loc_idxs].astype('float32')
        self.outdata[('fds_sample_flags','MetaData')] = self.gnssrData["fds_sample_flags"][loc_idxs].astype('float32')
        self.outdata[('yslf_sample_flags','MetaData')] = self.gnssrData["yslf_sample_flags"][loc_idxs].astype('float32')
        #self.outdata[('wind_speed_bias','MetaData')] = self.gnssrData["wind_speed_error"][loc_idxs].astype('float32')

        # add output variables
        for iodavar in ['wind_speed']:
            # We populate each preqc variable with the qflg value given in the GNSS-R L2 file. Hence, the qflg MetaData
            # and PreQC/wind_speed in IODA file will have the same values but different dimensions. Each value in the
            # PreQC["wind_speed"][t] = qflg[t] value.
            ws_flat_values= np.array(self.gnssrData["wind_speed"][loc_idxs]).astype('float32')
            ws_flat_bias= np.array(self.gnsrrData["wind_speed_bias"][loc_idxs]).astype('float32')
            ws_flat_errors = np.array(self.gnssrData["wind_speed_error"][loc_idxs]).astype('float32')
            ws_flat_preqc = np.array(self.gnssrData["preqc"][loc_idxs]).astype('int32')
            self.outdata[self.varDict[iodavar]['valKey']] = ws_flat_values
            self.outdata[self.varDict[iodavar]['biasKey']] = ws_flat_bias
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
        scf.time_idxs = np.array([i for i, o in enumerate(all_datetimes)
                                  if abs((o-current_da_time).total_seconds() / 3600.) <= da_window_shift])
        print(f"time_idx = {scf.time_idxs.shape}")
        if len(scf.time_idxs) == 0:
            print(f'No data matches requested time interval')
            continue
            
        # Remove data for which the quality flag
        if qflg:
            # Remove all data that fails pre-processing QC. Currently this includes data where GNSS-R quality_flags = 0.
            idx_qc_window = np.where(scf.gnssrData["preqc"][scf.time_idxs] == 0)
            scf.loc_idxs = scf.time_idxs[idx_qc_window]
        else:
            scf.loc_idxs = scf.time_idxs
        print(f'QC flags are {qflg}, the total number of observations in this DA window are {len(scf.time_idxs)} '
              f'with the number passing QC = {len(scf.loc_idxs)}')

        scf.setup_ioda_vars()
        attr_data["date_time_string"] = current_da_time.strftime('%Y-%m-%dT%H:%M:%SZ')
        out_file_name = 'gnssr_windspeed_L2_' + current_da_time.strftime('%Y-%m-%dT%H:%M:%SZ') + ".nc4"
        out_file_path = os.path.join(out_dir, out_file_name)
        writer = iconv.IodaWriter(out_file_path, locationKeyList, dim_dict)
        writer.BuildIoda(scf.outdata, var_dims, scf.varAttrs, attr_data)  # write the output file
        
    del scf
    t1 = time.perf_counter()
    print(f'Total time taken to run the code: {(t1-t0)} seconds or {(t1-t0)/60.} minutes')


if __name__ == '__main__':
    main()
