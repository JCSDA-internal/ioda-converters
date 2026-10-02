#!/usr/bin/env python3

# converter for tcvitals format (tropical storm central pressure) data

import numpy as np
import netCDF4 as nc
import argparse
import pandas as pd
from datetime import datetime
import dateutil.parser
from builtins import str
from pyioda import ioda_obs_space as ioda_ospace


def read_file(file_name):

    names = ["center", "storm_id", "storm_name", "datestr",
             "lat", "latns", "lon", "lonew", "stdir", "stspd", "pcen"]

#                ctr      id     name    datestr     lat      latns
    colspecs = [(0, 4), (5, 8), (9, 18), (19, 32), (33, 36), (36, 37),
                (38, 42), (42, 43), (44, 47), (48, 51), (52, 56)]
#                   lon      lonew      dir       spd       pres

    data = pd.read_fwf(file_name, colspecs=colspecs, header=None, names=names)

    return data


def create_output(data, output_name):

    float_missing_value = nc.default_fillvals['f4']

    lon = data.lon / 10.0
    lon[data.lonew == 'W'] = 360. - lon
    lat = data.lat / 10.0
    lat[data.latns == 'S'] = - lat
# calculate 'obs error' as in GSI
    tcp_refps = 1000.0
    tcp_width = 50.0
    tcp_ermin = 0.75
    tcp_ermax = 5.0
#      alpha=max(min(psdif/tcp_width,one),zero)
    psdif = tcp_refps - data.pcen
    alpha = psdif / tcp_width
    alpha[alpha > 1.0] = 1.0
    alpha[alpha < 0.0] = 0.0
    oberr = tcp_ermin+(tcp_ermax-tcp_ermin)*alpha
    nobs = len(lat)
    height = np.zeros(nobs)
    obstype = np.full(nobs, 112)
    obssubtype = np.full(nobs, 0)
    preqc = np.full(nobs, 2)
    preuseflg = np.full(nobs, 1)
    tempK = np.full(nobs, float_missing_value)
    psminPa = data.pcen*100.
    oberrPa = oberr*100.
    epoch = datetime.fromisoformat("1970-01-01T00:00:00Z")
    obdate = pd.to_datetime(data.datestr, format="%Y%m%d %H%M")
    eparr = np.full_like(obdate, epoch)
    obdiff = obdate - eparr
    sec_since_epoch = obdiff.astype('int64') // 10**9
    station_id = data['center'].str.cat(data['storm_id'], sep='_')
    dims = {
        'Location': np.arange(0, psminPa.shape[0]),
    }
    obsspace = ioda_ospace.ObsSpace(output_name, mode='w', dim_dict=dims)
    obsspace.create_var('MetaData/dateTime', dtype='int64') \
        .write_attr('units', 'seconds since 1970-01-01T00:00:00Z') \
        .write_attr('long_name', 'dateTime') \
        .write_data(sec_since_epoch)
    obsspace.create_var('MetaData/stationElevation', dtype=np.float32) \
        .write_attr('units', 'm') \
        .write_attr('long_name', 'Height Of Station') \
        .write_data(height)
    obsspace.create_var('MetaData/height', dtype=np.float32) \
        .write_attr('units', 'm') \
        .write_attr('long_name', 'Height Of Station') \
        .write_data(height)
    obsspace.create_var('MetaData/latitude', dtype=np.float32) \
        .write_attr('units', 'degrees_north') \
        .write_attr('valid_range', np.array([-90, 90], dtype=np.float32)) \
        .write_attr('long_name', 'Latitude') \
        .write_data(lat)
    obsspace.create_var('MetaData/longitude', dtype=np.float32) \
        .write_attr('units', 'degrees_east') \
        .write_attr('valid_range', np.array([0, 360], dtype=np.float32)) \
        .write_attr('long_name', 'Longitude') \
        .write_data(lon)
    obsspace.create_var('MetaData/pressure', dtype=np.float32) \
        .write_attr('units', 'Pa') \
        .write_attr('valid_range', np.array([105000, 80000], dtype=np.float32)) \
        .write_attr('long_name', 'Pressure') \
        .write_data(psminPa)
    obsspace.create_var('MetaData/stationIdentification', dtype=station_id.dtype) \
        .write_attr('long_name', 'stationIdentification') \
        .write_data(station_id)
    obsspace.create_var('ObsType/stationPressure', dtype=np.int32) \
        .write_attr('long_name', 'Station Pressure Observation Type') \
        .write_data(obstype)

    obsspace.create_var('ObsSubType/stationPressure', dtype=np.int32) \
        .write_attr('long_name', 'Station Pressure Observation subType') \
        .write_data(obssubtype)

# ObsError: initial error values loaded from the input ioda file
    obsspace.create_var('ObsError/stationPressure', dtype=np.float32) \
        .write_attr('units', 'Pa') \
        .write_attr('long_name', 'ObsError') \
        .write_attr('coordinates', 'longitude latitude') \
        .write_data(oberrPa)

    obsspace.create_var('ObsValue/stationPressure', dtype=np.float32) \
        .write_attr('units', 'Pa') \
        .write_attr('valid_range', np.array([105000, 80000], dtype=np.float32)) \
        .write_attr('long_name', 'ObsValue') \
        .write_attr('coordinates', 'longitude latitude') \
        .write_data(psminPa)

# temporary fake array for temperature requested by SfcCorrected
    obsspace.create_var('ObsValue/airTemperature', dtype=np.float32) \
        .write_attr('units', 'K') \
        .write_attr('valid_range', np.array([250, 380], dtype=np.float32)) \
        .write_attr('long_name', 'ObsValue') \
        .write_attr('coordinates', 'longitude latitude') \
        .write_data(tempK)


def main():

    desc = "Reads a tcvitals (tropical storm center data) file and converts into IODA format"
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument(
        '-i', '--input', help='Input tcvitals text file',
        type=str, required=True, default=None)
    parser.add_argument(
        '-o', '--output', help='Output tcvitals ioda nc4 file',
        type=str, required=True, default=None)
    args = parser.parse_args()

    data = read_file(args.input)

    create_output(data, args.output)


if __name__ == '__main__':
    main()
