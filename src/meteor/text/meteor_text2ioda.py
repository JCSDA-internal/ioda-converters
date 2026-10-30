#!/usr/bin/env python3

# converter for tcvitals format (tropical storm central pressure) data

import numpy as np
import netCDF4 as nc
import argparse
import pandas as pd
from datetime import datetime
import dateutil.parser
from builtins import str
#from pyioda import ioda_obs_space as ioda_ospace
import os
import glob


def read_file(filename,time_window):
    custom_names = ['Year', 'Month', 'Day', 'Hour', 'Altitude', 'Zonal_wind', 'zw_error', 'Merid_wind', 'mw_error']
#                ctr      id     name    datestr     lat      latns
    colspecs = [(0, 4), (5, 8), (9, 18), (19, 32), (33, 36), (36, 37),
                (38, 42), (42, 43), (44, 47), (48, 51), (52, 56)]
#                   lon      lonew      dir       spd       pres
    ## TIME WINDOW to YYYYMO format
    start_date = time_window[0]
    end_date = time_window[1]
    #print(time_window)
    YYYYMOs = start_date[0:4]+start_date[5:7]
    YYYYMOe = end_date[0:4]+end_date[5:7]
    # load station file information
    station_filename = (filename+'\\Station_LatLon.txt')
    stnInfo = pd.read_table(station_filename,skiprows=1,delimiter=',',names=['StationID','lat','lon'])

    
    ct = 0
    # initialize this to be a string variable
    data_list = []

    # Loop through everything inside the base directory
    for station_folder_name in os.listdir(filename):
        # Construct the full path to the station folder (e.g., ...\Step1_complete_winds\ALO)
        station_path = os.path.join(filename, station_folder_name)
       
        # Check to make sure it is actually a folder before proceeding
        if os.path.isdir(station_path):
           
            # Now search for your specific file pattern INSIDE this station folder
            file_pattern = os.path.join(station_path, f'*_{YYYYMOs}.txt')
            matching_files = glob.glob(file_pattern)
            #print(matching_files)
            if matching_files:
                #read that file as table
                tempdata = pd.read_table(matching_files[0], skiprows=1, header=None, names=custom_names)

                # create datetime from 4 columns
                obdate = pd.to_datetime({"year": tempdata.Year,
                "month": tempdata.Month,
                "day": tempdata.Day,
                "hour": tempdata.Hour})

                # make a table with start stop times
                tempdata_tw = tempdata[(obdate > start_date) & (obdate < end_date)].copy()
                #tempdata_tw['StationID']= pd.Series(dtype='string')
                # add station ID, LAT, LON as columns
                # get length of observation
                nobs = len(tempdata_tw.Zonal_wind)
                # get values to repeat
                lon = stnInfo.lon[ct]
                lat = stnInfo.lat[ct]
                station_id = station_folder_name # change based on location

                # MAKE array with repeat values to match the number of obs
                #stid_arr = pd.Series(station_id * nobs, dtype='string')
                #tempdata_tw.assign(Station_ID=pd.Series([str(station_id)] * nobs, dtype='string'))
                #print(stid_arr)

                #stid_arr = station_id.repeat(nobs).reset_index(drop=True)    # give a name
                lonarr = lon.repeat(nobs)    # give a name
                latarr = lat.repeat(nobs)

                # append the three columns to the existing table
                tempdata_tw['StationID'] = station_id
                tempdata_tw['Latitude'] = latarr
                tempdata_tw['Longitude'] = lonarr
                #print(tempdata_tw)
                data_list.append(tempdata_tw)
                # increase the counter by 1 for each station folder
                ct = ct+1
    master_df = pd.concat(data_list, ignore_index=True)
    print(master_df)
    return master_df


def create_output(data, output_name): # maynot need file
   
    float_missing_value = nc.default_fillvals['f4']
#
    # Use start date to
   
# add to the masterlist
    lonarr   = data["longitude"]
    latarr   = data["latitude"]
    stid_arr = data["Station_ID"]


    zw = data.Zonal_wind
    zw_err = data.zw_error

    mw = data.Merid_wind
    mw_err = data.mw_error

    alt = data.Altitude
    # KEEP
    epoch = datetime.fromisoformat("1970-01-01T00:00:00Z")
    # Select the columns and convert them to datetime all at once
    obdate = pd.to_datetime({"year": data.Year,
            "month": data.Month,
            "day": data.Day,
            "hour": data.Hour})
    eparr = np.full_like(obdate, epoch)
    obdiff = obdate - eparr
    sec_since_epoch = obdiff.astype('int64') // 10**9
    #print(sec_since_epoch)

       

    obsspace = ioda_ospace.ObsSpace(output_name, mode='w', dim_dict=dims)
    obsspace.create_var('MetaData/dateTime', dtype='int64') \
        .write_attr('units', 'seconds since 1970-01-01T00:00:00Z') \
        .write_attr('long_name', 'dateTime') \
        .write_data(sec_since_epoch)
    obsspace.create_var('MetaData/Altitude', dtype=np.float32) \
        .write_attr('units', 'km') \
        .write_attr('long_name', 'Altitude') \
        .write_data(alt)
   
    obsspace.create_var('MetaData/latitude', dtype=np.float32) \
        .write_attr('units', 'degrees_north') \
        .write_attr('valid_range', np.array([-90, 90], dtype=np.float32)) \
        .write_attr('long_name', 'Latitude') \
        .write_data(latarr)
    obsspace.create_var('MetaData/longitude', dtype=np.float32) \
        .write_attr('units', 'degrees_east') \
        .write_attr('valid_range', np.array([-180, 180], dtype=np.float32)) \
        .write_attr('long_name', 'Longitude') \
        .write_data(lonarr) # should this be array?
   
    obsspace.create_var('MetaData/stationIdentification') \
        .write_attr('long_name', 'stationIdentification') \
        .write_data(stid_arr)
   
# obsvalue
    obsspace.create_var('ObsValue/windEastward', dtype=np.float32) \
        .write_attr('units', 'm/s') \
        .write_attr('valid_range', np.array([-200, 200], dtype=np.float32)) \
        .write_attr('long_name', 'ObsValue') \
        .write_attr('coordinates', 'longitude latitude') \
        .write_data(zw)
    obsspace.create_var('ObsValue/windNorthward', dtype=np.float32) \
        .write_attr('units', 'm/s') \
        .write_attr('valid_range', np.array([-200, 200], dtype=np.float32)) \
        .write_attr('long_name', 'ObsValue') \
        .write_attr('coordinates', 'longitude latitude') \
        .write_data(mw)

# ObsError: initial error values loaded from the input ioda file
    obsspace.create_var('ObsError/windEastward', dtype=np.float32) \
        .write_attr('units', 'm/s') \
        .write_attr('long_name', 'ObsError') \
        .write_attr('coordinates', 'longitude latitude') \
        .write_data(zw_err)
    obsspace.create_var('ObsError/windNorthward', dtype=np.float32) \
        .write_attr('units', 'm/s') \
        .write_attr('long_name', 'ObsError') \
        .write_attr('coordinates', 'longitude latitude') \
        .write_data(mw_err)
   

def main():

    desc = "Reads a tcvitals (tropical storm center data) file and converts into IODA format"
    parser = argparse.ArgumentParser(description=desc)
## INPUT
    parser.add_argument(
        '-d', '--data_input', help='Input the directory where station folders are stored',
        type=str, required=True, default=None)
#    parser.add_argument(
#        '-s', '--station_input', help='Input MRD  station text file',
#        type=str, required=True, default=None)
    parser.add_argument(
        '-sdt', '--start_input', help='Input the start datetime for time window. Use format yyyy-MM-ddTHH:mm:ss',
        type=str, required=True, default=None)
    parser.add_argument(
        '-edt', '--end_input', help='Input the end datetime for time window. Use format yyyy-MM-ddTHH:mm:ss',
        type=str, required=True, default=None)
## OUTPUT
    parser.add_argument(
        '-o', '--output', help='Output MRD ioda nc4 file',
        type=str, required=True, default=None)
    # ingest initial date from input

    # window_interval = #6th hour
    # get range
    # input for create output
    args = parser.parse_args()

    start_date = args.start_input
    end_date = args.end_input
    time_window = [start_date,end_date]

    data = read_file(args.data_input,time_window)

    #stn_info = args.station_input
    create_output(data,args.output)


if __name__ == '__main__':
    main()


'''
- Naming conventions
- windEastward
- windNorthward

how should I save these files?
loop through all folders and all text and
synoptic hour
0 6 12 18 synoptic hours
21 3 9 16 previous hours

'''
