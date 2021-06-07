#!/usr/bin/env python
# read airnow data and convert to netcdf
import netCDF4 as nc
import numpy as np
import inspect, os, sys, argparse
import pandas as pd
from datetime import datetime

# IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
# if not IODA_CONV_PATH.is_dir():
#    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
# sys.path.append(str(IODA_CONV_PATH.resolve()))
# import meteo_utils
# import ioda_conv_ncio as iconv
# from orddicts import DefaultOrderedDict

def read_monitor_file(sitefile=None):

        colsinuse = [
            0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
            20, 21
        ]
        airnow = pd.read_csv(sitefile,
                             delimiter='|',
                             header=None,
                             usecols=colsinuse,
                             dtype={0: str},
                             encoding="ISO-8859-1")
        airnow.columns = [
            'siteid', 'Site_Code', 'Site_Name', 'Status', 'Agency',
            'Agency_Name', 'EPA_region', 'latitude', 'longitude', 'Elevation',
            'GMT_Offset', 'Country_Code', 'CMSA_Code', 'CMSA_Name', 'MSA_Code',
            'MSA_Name', 'state_Code', 'state_Name', 'County_Code',
            'County_Name', 'City_Code'
        ]
        airnow['airnow_flag'] = 'AIRNOW'
        airnow.columns = [i.lower() for i in airnow.columns]
        return airnow

def filter_bad_values(df):
    """Short summary.

    Returns
    -------
    type
        Description of returned object.

    """

    df.loc[(df.obs > 3000) | (df.obs < 0), 'obs'] = np.NaN
    return df
    
def long_to_wide(df):
    from pandas import Series, merge
    w = df.pivot_table(
        values='obs', index=['time', 'siteid'],
        columns='variable').reset_index()
    cols = Series(df.columns)
    g = df.groupby('variable')
    for name, group in g:
        w[name + '_unit'] = group.units.unique()[0]
    # mergeon = hstack((index.values, df.variable.unique()))
    return merge(w, df, on=['siteid', 'time'])

def add_data(infile,sitefile):
   df=pd.read_csv(infile,delimiter='|',
                          header=None,
                          error_bad_lines=False,
                          encoding='ISO-8859-1')
   cols = ['date', 'time', 'siteid', 'site', 'utcoffset', 'variable', 'units',
            'obs', 'source']
   df.columns = cols
   df['obs'] = df.obs.astype(float)
   df['siteid'] = df.siteid.str.zfill(9)
   df['utcoffset'] = df.utcoffset.astype(int)
   df['time'] = pd.to_datetime(df.date + ' ' + df.time,
                                format='%m/%d/%y %H:%M',
                                exact=True)
   df.drop(['date'], axis=1, inplace=True)
   df['time_local'] = df.time + pd.to_timedelta(df.utcoffset, unit='H')
   
   monitor_df = read_monitor_file(sitefile)
   df = pd.merge(df, monitor_df, on='siteid')
   df.drop_duplicates(inplace=True)
   df = filter_bad_values(df)
   return long_to_wide(df)
   
if __name__ == '__main__':

    # get command line arguments
 parser = argparse.ArgumentParser(
        description=(
            'Reads single AIRNow text file '
            ' and converts into IODA formatted output files.')
    )

 required = parser.add_argument_group(title='required arguments')
 required.add_argument(
        '-i', '--input',
        help="path of AIRNow text input file",
        type=str, required=True)
 required.add_argument(
        '-s', '--sitefile',
        help="path of AIRNow site list file",
        type=str, required=True)
 required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

 args = parser.parse_args()
 print('infile=',args.input,args.sitefile)
 f=add_data(args.input,args.sitefile)

 f3=f.dropna(subset=['OZONE','PM2.5'],how='all') 
# f3=f.dropna(subset=['PM2.5'])
 nlocs,columns=f3.shape

 a=nc.Dataset(args.output,'w')
 nvars_dim=a.createDimension('nvars',2)
 nlocs_dim=a.createDimension('nlocs',nlocs)
 nstring_dim=a.createDimension('nstring',50)
 ndatetime_dim=a.createDimension('ndatetime',20)

 x1nvars=a.createVariable('nvars','f4',('nvars',))
 x1nvars[:]=0.
 x1nlocs=a.createVariable('nlocs','f4',('nlocs',))
 x1nlocs[:]=0.
 x1nstring=a.createVariable('nstring','f4',('nstring',))
 x1nstring[:]=0.
 x1ndatetime=a.createVariable('ndatetime','f4',('ndatetime',))
 x1ndatetime[:]=0.

 meta_group=a.createGroup('MetaData')
 x2=meta_group.createVariable('record_number','i4',('nlocs',))
 x2[:]=2
 x3=meta_group.createVariable('latitude','f4',('nlocs',))
 x3[:]=np.array(f3['latitude'])
 x4=meta_group.createVariable('longitude','f4',('nlocs',))
 x4[:]=np.array(f3['longitude'])
 x5=meta_group.createVariable('station_elevation','f4',('nlocs',))
 x5[:]=10.
 x5a=meta_group.createVariable('height','f4',('nlocs',))
 x5a.units='m'
 x5a[:]=10. 
 x6=meta_group.createVariable('station_id',str,('nlocs',))
 c=np.empty([nlocs],'S20')
 c[:]=np.array(f3.siteid)
 x6[:]=c
 x7=meta_group.createVariable('datetime',str,('nlocs',))
 d=np.empty([nlocs],'S20')
 d[:]=f3.time[1].strftime('%Y-%m-%dT%H:%M:%SZ')*nlocs
 x7[:]=d
# time difference
 x8=meta_group.createVariable('time','f4',('nlocs',))
 x8[:]='0.0'

 value_group=a.createGroup('ObsValue')
 x9=value_group.createVariable('pm25_tot','f4',('nlocs',))
 x9[:]=np.array(f3['PM2.5'].fillna(nc.default_fillvals['f4']))
 x12=value_group.createVariable('o3','f4',('nlocs',))
 x12[:]=np.array((f3['OZONE']/1000).fillna(nc.default_fillvals['f4']))

 err_group=a.createGroup('ObsError')
 x10=err_group.createVariable('pm25_tot','f4',('nlocs',))
 x10[:]=0.1
 x13=err_group.createVariable('o3','f4',('nlocs',))
 x13[:]=0.1
 
 qc_group=a.createGroup('PreQC')
 x11=qc_group.createVariable('pm25_tot','i4',('nlocs',))
 x11[:]=0
 x14=qc_group.createVariable('o3','i4',('nlocs',))
 x14[:]=0

 var_group=a.createGroup('VarMetaData')  
 x1=var_group.createVariable('variable_names',str,('nvars',))
 varstring=np.empty(2,'S50')
 varstring[:]=['pm25_tot','o3']
 x1[:]=varstring

 a.nvars=np.int32(2)
 a.nlocs=np.int32(nlocs)
 ctime=f3.time[1].strftime('%Y%m%d%H')
 
 print('ctime=',ctime)
 a.date_time=np.int32(ctime)
 a.close()
