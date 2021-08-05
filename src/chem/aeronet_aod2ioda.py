#!/usr/bin/env python3
# read/interpolate online aeronet AOD data and convert to netcdf
# Usage:
#	python aeronet_aod2ioda.py -t 2021080500 -w 6 -o aeronet_aod.nc
#	-t: time of AERONET AOD in YYYYMMDDHH format
#	-w: time wihdow within which AERONET AOD will be collected and converted (e.g., [time-window/2, time+window/2])
#       -o: output file name. 

import netCDF4 as nc
import numpy as np
import inspect, sys, os, argparse
import pandas as pd
from datetime import datetime, timedelta
from builtins import object, str
from numpy import NaN
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import pytspack as pts
import meteo_utils
import ioda_conv_ncio as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

def dateparse(x):
    return datetime.strptime(x, '%d:%m:%Y %H:%M:%S')

def add_data(dates=None,
             product='AOD15',
             latlonbox=None,
             daily=False,
             interp_to_aod_values=None,
             inv_type=None,
             freq=None,
             siteid=None,
             detect_dust=False, n_procs=1, verbose=10):
    a = AERONET()
    df = a.add_data(dates=dates,
                    product=product,
                    latlonbox=latlonbox,
                    daily=daily,
                    interp_to_aod_values=interp_to_aod_values,
                    inv_type=inv_type,
                    siteid=siteid,
                    freq=freq,
                    detect_dust=detect_dust)
    return df.reset_index(drop=True)


class AERONET(object):
    def __init__(self):
        from numpy import concatenate, arange
        self.baseurl = 'https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_v3?'
        self.dates = [
            datetime.strptime('2016-06-06 12:00:00', '%Y-%m-%d %H:%M:%S'),
            datetime.strptime('2016-06-10 13:00:00', '%Y-%m-%d %H:%M:%S')
        ]
        self.datestr = []
        self.df = pd.DataFrame()
        self.daily = None
        self.prod = None
        self.inv_type = None
        self.siteid = None
        self.objtype = 'AERONET'
        self.usecols = concatenate((arange(30), arange(65, 83)))
        self.latlonbox = None
        self.url = None
        self.new_aod_values = None

    def build_url(self):
        sy = self.dates.min().strftime('%Y')
        sm = self.dates.min().strftime('%m').zfill(2)
        sd = self.dates.min().strftime('%d').zfill(2)
        sh = self.dates.min().strftime('%H').zfill(2)
        ey = self.dates.max().strftime('%Y').zfill(2)
        em = self.dates.max().strftime('%m').zfill(2)
        ed = self.dates.max().strftime('%d').zfill(2)
        eh = self.dates.max().strftime('%H').zfill(2)
        if self.prod in [
                'AOD10', 'AOD15', 'AOD20', 'SDA10', 'SDA15', 'SDA20', 'TOT10',
                'TOT15', 'TOT20'
        ]:
            base_url = 'https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_v3?'
            inv_type = None
        else:
            base_url = 'https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_inv_v3?'
            if self.inv_type == 'ALM15':
                inv_type = '&ALM15=1'
            else:
                inv_type = '&AML20=1'
        date_portion = 'year=' + sy + '&month=' + sm + '&day=' + sd + \
            '&hour=' + sh + '&year2=' + ey + '&month2=' + em + '&day2=' + ed +\
            '&hour2=' + eh
        if self.inv_type is not None:
            product = '&product=' + self.prod
        else:
            product = '&' + self.prod + '=1'
            self.inv_type = ''
        time = '&AVG=' + str(self.daily)
        if self.siteid is not None:
            latlonbox = '&site={}'.format(self.siteid)
        elif self.latlonbox is None:
            latlonbox = ''
        else:
            lat1 = str(float(self.latlonbox[0]))
            lon1 = str(float(self.latlonbox[1]))
            lat2 = str(float(self.latlonbox[2]))
            lon2 = str(float(self.latlonbox[3]))
            latlonbox = '&lat1=' + lat1 + '&lat2=' + \
                lat2 + '&lon1=' + lon1 + '&lon2=' + lon2
        print(base_url)
        print(date_portion)
        print(product)
        print(inv_type)
        print(time)
        print(latlonbox)
        if inv_type is None:
            inv_type = ''
        self.url = base_url + date_portion + product + \
            inv_type + time + latlonbox + '&if_no_html=1'

    def read_aeronet(self):
        print('Reading Aeronet Data...')
        df = pd.read_csv(self.url,
                         engine='python',
                         header=None,
                         skiprows=6,
                         parse_dates={'time': [1, 2]},
                         date_parser=dateparse,
                         na_values=-999)
        columns = self.get_columns()
        df.columns = columns
        df.index = df.time
        df.rename(columns={
            'site_latitude(degrees)': 'latitude',
            'site_longitude(degrees)': 'longitude',
            'site_elevation(m)': 'elevation',
            'aeronet_site': 'siteid'
        },
            inplace=True)
        df.dropna(subset=['latitude', 'longitude'], inplace=True)
        df.dropna(axis=1, how='all', inplace=True)
        self.df = df

    def get_columns(self):
        header = pd.read_csv(self.url, skiprows=5, header=None,
                             nrows=1).values.flatten()
        final = ['time']
        for i in header:
            if "Date(" in i or 'Time(' in i:
                pass
            else:
                final.append(i.lower())
        return final

    def add_data(self,
                 dates=None,
                 product='AOD15',
                 latlonbox=None,
                 daily=False,
                 interp_to_aod_values=None,
                 inv_type=None,
                 freq=None,
                 siteid=None,
                 detect_dust=False):
        self.latlonbox = latlonbox
        self.siteid = siteid
        if dates is None:  # get the current day
            self.dates = pd.date_range(start=pd.to_datetime('today'),
                                       end=pd.to_datetime('now'),
                                       freq='H')
        else:
            self.dates = dates
        self.prod = product.upper()
        if daily:
            self.daily = 20  # daily data
        else:
            self.daily = 10  # all points
        if inv_type is not None:
            self.inv_type = 'ALM15'
        else:
            self.inv_type = inv_type
        if 'AOD' in self.prod:
            self.new_aod_values = interp_to_aod_values
        self.build_url()
        try:
            self.read_aeronet()
        except:
            print(self.url)
        if freq is not None:
            self.df = self.df.groupby('siteid').resample(
                freq).mean().reset_index()
        if detect_dust:
            self.dust_detect()
        if self.new_aod_values is not None:
            self.calc_new_aod_values()
        return self.df

    def calc_new_aod_values(self):

        def _tspack_aod_interp(row, new_wv=[440., 470., 550., 670., 870., 1240.]):
            try:
                import pytspack
            except ImportError:
                print('You must install pytspack before using this function')
            aod_columns = [aod_column for aod_column in row.index if 'aod_' in aod_column]
            aods = row[aod_columns]
            wv = [float(aod_column.replace('aod_', '').replace('nm', '')) for aod_column in aod_columns]
            a = pd.DataFrame({'aod': aods}).reset_index()
	    # Interpolate AOD based on log(wv)
            wv_log=np.log(wv, dtype='float64')
            a['wv'] = wv_log
            new_wv_log=np.log(new_wv, dtype='float64')
            df_aod_nu = a.dropna()
            df_aod_nu_sorted = df_aod_nu.sort_values(by='wv').dropna()
            if len(df_aod_nu_sorted) < 2:
                return new_wv_log * NaN
            else:
                x, y, yp, sigma = pytspack.tspsi(df_aod_nu_sorted.wv.values, df_aod_nu_sorted.aod.values)
                yi = pytspack.hval(new_wv_log, x, y, yp, sigma)
                return yi

        out = self.df.apply(_tspack_aod_interp, axis=1, result_type='expand', new_wv=self.new_aod_values)
        names = 'aod_int_' + pd.Series(self.new_aod_values.astype(int).astype(str)) + 'nm'
        out.columns = names.values
        self.df = pd.concat([self.df, out], axis=1)

    def dust_detect(self):
        """Detect dust from AERONET. See [Dubovik et al., 2002].

        AOD_1020 > 0.3 and AE(440,870) < 0.6

        Returns
        -------
        type
            Description of returned object.

        """
        self.df['dust'] = (self.df['aod_1020nm'] >
                           0.3) & (self.df['440-870_angstrom_exponent'] < 0.6)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
	     description=(
		          'Reads online AERONET data from NASA website '
			  ' and converts into IODA formatted output files')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
		    '-t', '--time',
		    help="time (YYYYMMDDTHH) of AERONET AOD files to be downloaded from NASA website",
		    type=str, required=True)
    required.add_argument(
		    '-w', '--window',
		    help="An integer/float number defines a time window in hours centered at time argument within which AERONET AOD data will be downloaded",
		    type=float, required=True)
    required.add_argument(
		    '-o', '--output',
		    help="path of AERONET AOD IODA file",
		    type=str, required=True)

    args = parser.parse_args()
    date_center1 = args.time
    hwindow=args.window
    hwindow=hwindow/2.0
    outfile = args.output
    date_center = datetime.strptime(date_center1, '%Y%m%d%H') 
    date_start = date_center + timedelta(hours=-1.*hwindow)
    date_end = date_center + timedelta(hours=hwindow)

    print('Download AERONET AOD within +/- ' + str(hwindow) + ' hours at: ')
    print(date_center)

    dates = pd.date_range(start=date_start,end=date_end,freq='H')

    # Define AOD wavelengths, channels and frequencies
    aod_wav = np.array([340., 380., 440., 500., 675, 870., 1020., 1640.,], dtype=np.float32)
    aod_chan = np.array([1,   2,    3,    4,    5,   6,    7,     8 ], dtype=np.intc)

    # If AOD at other wavelenths (e.g., 550nm) are needed, please define/calculate aod_new_wav, aod_new_chan (other than 1-8),
    # frequency_new, concatanate aod_new_chan to aod_chan, and frequency_new to frequency and modify outcols and obsvars. 
    # In the current, AOD at aod_new_wav is not written out in the IODA files (e.g, obsvars).
    aod_new_wav=np.array([550.,], dtype=np.float32)
    aod_new_chan=None
    frequency_new=None
    speed_light = 2.99792458E8
    frequency = speed_light*1.0E9/aod_wav
    print('Calculate AERONET AOD at wavelengths/channels/frequencies: ')
    print(aod_wav)
    print(aod_chan)
    print(frequency)

    # Read and extract online AERONET AOD
    print('Read and extract online AERONET AOD')
    outcols=['time', 'siteid', 'longitude', 'latitude', 'elevation', 'aod_int_550nm', 'aod_340nm', 'aod_380nm', 'aod_440nm', 'aod_500nm', 'aod_675nm','aod_870nm', 'aod_1020nm','aod_1640nm']

    f3 = add_data(dates=dates, product='AOD15',interp_to_aod_values=aod_new_wav)

    # Define AOD varname that match with those in f3
    nlocs, columns = f3.shape
    if nlocs==0:
        print('No avaiable AERONET AOD at ' + date_center1 + '  and exit')
        exit(0)
    obsvars = { 'aerosol_optical_depth_1': 'aod_340nm',  'aerosol_optical_depth_2': 'aod_380nm',
		'aerosol_optical_depth_3': 'aod_440nm',  'aerosol_optical_depth_4': 'aod_675nm',
		'aerosol_optical_depth_5': 'aod_500nm',  'aerosol_optical_depth_6': 'aod_870nm', 
		'aerosol_optical_depth_7': 'aod_1020nm', 'aerosol_optical_depth_8': 'aod_1640nm',}

    locationKeyList = [("latitude", "float"), ("longitude", "float"), ("datetime", "string"),] 
    writer = iconv.NcWriter(outfile, locationKeyList)
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    units = {}
    units['latitude'] = 'degree'
    units['longitude'] = 'degree'
    units['station_elevation'] = 'm'
    
    # Define varDict variables
    print('Define varDict variables')
    for key, value in obsvars.items():
        print(key, value)
        varDict[key]['valKey'] = key, writer.OvalName()
        varDict[key]['errKey'] = key, writer.OerrName()
        varDict[key]['qcKey'] = key, writer.OqcName()

    # Define loc_mdata
    print('Define loc_mdata')
    loc_mdata['latitude'] = np.array(f3['latitude'])
    loc_mdata['longitude'] = np.array(f3['longitude'])
    loc_mdata['station_elevation'] = np.array(f3['elevation'])
    loc_mdata['surface_type'] = np.full((nlocs), 1)

    c = np.empty([nlocs], dtype='S50')
    c[:] = np.array(f3.siteid)
    loc_mdata['station_id'] = writer.FillNcVector(c, 'string')

     # Define datetime
    d = np.empty([nlocs], 'S20')
    for i in range(nlocs):
        d[i]=f3.time[i].strftime('%Y-%m-%dT%H:%M:%SZ') 
    loc_mdata['datetime'] = writer.FillNcVector(d, 'datetime')

    # Define var_mdata
    print('Define var_mdata')
    var_mdata['frequency'] = writer.FillNcVector(frequency, 'float')
    var_mdata['sensor_channel'] = writer.FillNcVector(aod_chan, 'integer')

    for key, value in obsvars.items():
        outdata[varDict[key]['valKey']] = np.array(f3[value].fillna(nc.default_fillvals['f4']))
        outdata[varDict[key]['qcKey']] = np.where(outdata[varDict[key]['valKey']] == nc.default_fillvals['f4'], 1, 0)
        outdata[varDict[key]['errKey']] = np.where(outdata[varDict[key]['valKey']] == nc.default_fillvals['f4'], nc.default_fillvals['f4'], 0.02)

    # Define global atrributes
    print('Define global atrributes')
    AttrData = {'observation_type': 'Aod', 
		'date_time_string': date_center.strftime('%Y-%m-%dT%H:%M:%SZ'),
		'sensor': "aeronet", 
		'surface_type': 'ocean=0,land=1,costal=2',}

    # Write out IODA V1 NC files
    print('Write into IODA format file: ' + outfile)
    writer._nvars = len(aod_wav)
    writer._nlocs = nlocs
    writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, units)
