#!/usr/bin/env python

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
from monetio import aeronet
import pandas as pd
sys.path.append("@SCRIPT_LIB_PATH@")

def aeronet_monet_to_jedi(df, wavelengths=[]):
    aod_cols = ['aod_{}nm'.format(wv) for wv in wavelengths]
    cols=aod_cols.copy()
    # add metadata columns
    if 'time' not in cols:
        cols.append('time')
    if 'latitude' not in cols:
        cols.append('latitude')
    if 'longitude' not in cols:
        cols.append('longitude')
    if 'elevation' not in cols:
        cols.append('elevation')
    if 'aeronet_instrument_number' not in cols:
        cols.append('aeronet_instrument_number')
    if 'siteid' not in cols:
        cols.append('siteid')
    if df.index.name is not None:
        index_name = df.index.name
    else:
        index_name = 'index'

    q = df.loc[:,~df.columns.duplicated()][cols]
    w = q.to_xarray().rename({index_name: "x"})
    nvars = len(aod_cols)
    w['x'].data = w.x.astype(np.int32)
    w['nvars'] = arange(1,nvars+1).astype(np.int32)
    w = w.rename(dict(latitude='latitude@MetaData', 
                      longitude='longitude@MetaData', 
                      time='time@MetaData', x='nlocs',
                      elevation='height_above_mean_sea_level@MetaData',
                      aeronet_instrument_number='station_number@MetaData',
                      siteid='station_id@MetaData'))
    for index,aod in enumerate(aod_cols):
        w = w.rename({aod:'aerosol_optical_depth_{}@MetaData'.format(index+1)})
        w['sensor_channel@VarMetaData'] = (('nvars'), wavelengths)
        w['aerosol_optical_depth_{}@KnownObsBias'.format(index+1)] = (('nlocs'), np.ones(len(w.nlocs)) * 0.02 )
        w['aerosol_optical_depth_{}@ObsError'.format(index+1)] = (('nlocs'), np.ones(len(w.nlocs)) *0.02)
        w['aerosol_optical_depth_{}@PreQc'.format(index+1)] = (('nlocs'), np.ones(len(w.nlocs)) *0.)
    w.attrs['date_time'] = w['time@MetaData'].to_index().min().floor('D').strftime("%Y%m%d%H")
    w.attrs['observation_type'] = 'Aod'
    w.attrs['MONET'] = 'Created using the Model and Observation I/O (monetio; https://monetio.readthedocs.io)'
    w['time@MetaData'] = w['time@MetaData'].dt.strftime('%Y-%m-%dT%H:%M:%SZ')
    w['wavenumber@MetaData'] = (('nvars'), np.array(wavelengths) * 1e-9)
    w['nlocs'] = arange(1,len(s.nlocs)+1).astype(np.int32)
    for k in w.data_vars.keys():
        try:
            if w[k].dtype== np.dtype('O'):
                raise ValueError
            w[k] = w[k].fillna(-999)
            w[k].attrs['_FillValue'] = -999
        except ValueError:
            continue
    return fix_dtype(w)

def fix_dtype(ds):
    import numpy as np
    da = ds.copy()
    for k in da.keys():
        if da[k].dtype == np.dtype(np.float64):
            da[k].data = da[k].astype(np.float32)
        if da[k].dtype == np.dtype(np.int64):
            da[k] =  da[k].astype(np.int32)
     return da

def main():

    parser = argparse.ArgumentParser(
        description=('Read Local or remote Aeronet data (aerosol optical depth) file and Converter'
                     ' of native NetCDF format for observations of optical'
                     ' depth from VIIRS AOD550 to IODA netCDF format.')
    )
    parser.add_argument('-i', 
                        '--input',
                        help="name of aod550 input file(s)",
                        type=str, 
                        required=False,
                        default=None)
    parser.add_argument('-s', 
                        '--start_date',
                        help='Start Date to retrieve data. Example "2019-01-01"',
                        type=str, 
                        required=False, 
                        default=None)
    parser.add_argument('-e', 
                        '--end_date',
                        help='End date to retrieve data. Example "2019-01-02"',
                        type=str, 
                        required=False, 
                        default=None)
    parser.add_argument('-n', 
                        '--nprocs',
                        help='number of processors to use',
                        type=int, 
                        default=1, 
                        required=False)
    parser.add_argument('-f', 
                        '--frequency',
                        help='Frequency (in time)  on which to interpolate data to. Example --frequency="H"',
                        type=str, 
                        default='H',
                        required=False)
    parser.add_argument('-p', 
                        '--product', 
                        help='Aeronet Product to Download',
                        default='AOD15', 
                        type='str', 
                        required=False)
    parser.add_argument('-w',
                        '--wavelengths',
                        nargs='+',
                        default=[440,470,550,670,870,1020,1240],
                        required=False)
    parser.add_argument('-o', 
                        '--output',
                        help="name of ioda output file",
                        type=str, 
                        required=True)
    args = parser.parse_args()

    #get product 
    p = args.product
    
    # get nprocs
    nprocs = args.nprocs
    
    # get frequency
    freq = args.frequency
    
    # get output 
    out = args.output
    
    #get wanted wavelengths
    wv = args.wavelengths

    # get start date and end date
    s, e = args.start_date, args.end_date

    #get input file
    #check if local first
    if args.input is not None:
        df = aeronet.add_local(args.input, product=p, freq=freq, interp_to_values=wv)
    else:
        if s is None or e is None:
            try:
                raise ValueError
            except:
                print('If using monetio to download data you must provide the start and end date...')
        else:
            dates = pd.date_range(start=s,end=e)
            df = aeronet.add_data(dates=dates,product=p,freq=freq,interp_to_values=wv)
    
    nc = aeronet_monet_to_jedi(df,wavelengths=wv)
    nc.to_netcdf(out)

if __name__ == '__main__':
    main()
