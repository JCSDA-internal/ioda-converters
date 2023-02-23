#!/usr/bin/python

"""
Python code to ingest netCDF4 or HDF5 ATMS data
"""

import argparse
from datetime import datetime, timezone
import glob
# from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
import os.path
from os import getcwd
import sys

import h5py
import numpy as np

from apply_BG.apply_BG import apply_BG_class

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

# globals
SNPP_WMO_sat_ID = 224
NOAA20_WMO_sat_ID = 225
NOAA21_WMO_sat_ID = 226
ATMS_WMO_sensor_ID = 621

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

GlobalAttrs = {
    "sensor": str(ATMS_WMO_sensor_ID),
    "platformCommonName": "ATMS",
    "platformLongDescription": "ATMS Brightness Temperature Data",
    "sensorCentralFrequency": [23.8,
                               31.4, 50.3, 51.76, 52.8, 53.596, 54.40, 54.94, 55.50,
                               57.2903, 57.2903, 57.2903, 57.2903, 57.2903, 57.2903,
                               88.20, 165.5, 183.31, 183.31, 183.31, 183.31, 183.31],
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

iso8601_string = "seconds since 1970-01-01T00:00:00Z"
epoch = datetime.fromisoformat(iso8601_string[14:-1])


def main(args):

    output_filename = args.output
    dtg = datetime.strptime(args.date, '%Y%m%d%H')

    input_files = [(i) for i in args.input]
    # read / process files in parallel
    obs_data = {}
    # create a thread pool -- running out of memory for multiple files
#   with ProcessPoolExecutor(max_workers=args.threads) as executor:
#       for file_obs_data in executor.map(get_data_from_files, input_files):
#           if not file_obs_data:
#               print("INFO: non-nominal file skipping")
#               continue
#           if obs_data:
#               concat_obs_dict(obs_data, file_obs_data)
#           else:
#               obs_data = file_obs_data

    WMO_sat_ID = get_WMO_satellite_ID(input_files[0])
    GlobalAttrs['platform'] = np.int32(WMO_sat_ID)
    remap = args.remap
    if remap == "BG":
        file_obs_data = remapBG(input_files)
        obs_data = file_obs_data
    else:
        for afile in input_files:
            file_obs_data = get_data_from_files(afile)
            WMO_sat_ID = get_WMO_satellite_ID(afile)
            if not file_obs_data:
                print("INFO: non-nominal file skipping")
                continue
            if obs_data:
                concat_obs_dict(obs_data, file_obs_data)
            else:
                obs_data = file_obs_data
            if WMO_sat_ID != GlobalAttrs['platform']:
                print(' ERROR:  IODA and subsequent UFO expect individual files to be a single satellite and sensor ')
                print('    .... initial file satellite: ', GlobalAttrs['platform'])
                print('    ...... final file satellite: ', WMO_sat_ID)
                sys.exit()

    nlocs_int = np.array(len(obs_data[('latitude', metaDataName)]), dtype='int64')
    nlocs = nlocs_int.item()
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs['datetimeRange'] = np.array([datetime.fromtimestamp(obs_data[('dateTime', metaDataName)][0], timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                            datetime.fromtimestamp(obs_data[('dateTime', metaDataName)][-1], timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")],
                                            dtype=object)
    GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")

    # pass parameters to the IODA writer
    VarDims = {
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
    }

    DimDict = {
        'Location': nlocs,
        'Channel': obs_data[('sensorChannelNumber', metaDataName)],
    }
    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    set_obspace_attributes(VarAttrs)
    set_metadata_attributes(VarAttrs)

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_data_from_files(zfiles):

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    # for afile in zfiles:
    afile = zfiles
    if True:
        f = h5py.File(afile, 'r')

        if afile[-3:] == 'nc4':
            g = h5py.File(afile, 'r')
            obs_data = get_data_nasa_disc(f, g, obs_data)
        else:
            g = get_geo_noaa_class(afile)
            obs_data = get_data_noaa_class(f, g, obs_data)

        f.close()
        g.close()

    return obs_data


def get_data_nasa_disc(f, g, obs_data, add_qc=True):

    # NASA GES DISC keys
    WMO_sat_ID = get_WMO_satellite_ID(f.filename)

    # example: dimension ( 135, 96 ) == dimension( nscan, nbeam_pos )
    nscans = np.shape(g['lat'])[0]
    nbeam_pos = np.shape(g['lat'])[1]
    obs_data[('latitude', metaDataName)] = np.array(g['lat'][:, :].flatten(), dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(g['lon'][:, :].flatten(), dtype='float32')
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(g['channel'][:], dtype='int32')
    obs_data[('sensorScanPosition', metaDataName)] = np.tile(np.arange(nbeam_pos, dtype='float32') + 1, (nscans, 1)).flatten()
    obs_data[('solarZenithAngle', metaDataName)] = np.array(g['sol_zen'][:, :].flatten(), dtype='float32')
    obs_data[('solarAzimuthAngle', metaDataName)] = np.array(g['sol_azi'][:, :].flatten(), dtype='float32')
    obs_data[('sensorZenithAngle', metaDataName)] = np.array(g['sat_zen'][:, :].flatten(), dtype='float32')
    obs_data[('sensorAzimuthAngle', metaDataName)] = np.array(g['sat_azi'][:, :].flatten(), dtype='float32')
    obs_data[('sensorViewAngle', metaDataName)] = np.array(g['view_ang'][:, :].flatten(), dtype='float32')
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(g['obs_time_utc']), dtype='int64')

    # example: dimension ( 135, 96, 22 ) == dimension( nscan, nbeam_pos, nchannel )
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('brightnessTemperature', obsValName)] = np.array(np.vstack(g['antenna_temp']), dtype='float32')
    obs_data[('brightnessTemperature', obsErrName)] = np.full((nlocs, nchans), 5.0, dtype='float32')
    obs_data[('brightnessTemperature', qcName)] = np.full((nlocs, nchans), 0, dtype='int32')

    if add_qc:
        obs_data = atms_gross_quality_control(obs_data)

    return obs_data


def get_geo_noaa_class(afile):
    path, filename = os.path.split(afile)
    if 'GATMO' in filename:
        g = h5py.File(afile, 'r')
    else:
        geo_file = filename.replace('SATMS', 'GATMO')
        ftype, sat, date, time, end_time, orbit, _, _, _ = filename.split('_')
        geo_search = '_'.join(['GATMO', sat, date, time, end_time, orbit, '*.h5'])
        geo_search = os.path.join(path, geo_search)

        gfile = glob.glob(geo_search)
        if (len(gfile) > 0):
            g = h5py.File(gfile[0], 'r')
        else:
            print("could not find geofile matching: %s" % filename)
            print("tried searching geofile: %s" % geo_search)
            sys.exit()
    return g


def get_data_noaa_class(f, g, obs_data, add_qc=True):

    # NOAA CLASS h5 SDR-GEO  and SDR-Data
    WMO_sat_ID = get_WMO_satellite_ID(f.filename)

    # example: dimension ( 180, 96 ) == dimension( nscan, nbeam_pos )
    obs_data[('latitude', metaDataName)] = np.array(g['All_Data']['ATMS-SDR-GEO_All']['Latitude'][:, :].flatten(), dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(g['All_Data']['ATMS-SDR-GEO_All']['Longitude'][:, :].flatten(), dtype='float32')

    nscans = np.shape(g['All_Data']['ATMS-SDR-GEO_All']['Latitude'])[0]
    nbeam_pos = np.shape(g['All_Data']['ATMS-SDR-GEO_All']['Latitude'])[1]
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(np.arange(np.shape(g['All_Data']['ATMS-SDR_All']['BrightnessTemperature'])[2])+1, dtype='int32')
    obs_data[('sensorScanPosition', metaDataName)] = np.tile(np.arange(nbeam_pos, dtype='float32') + 1, (nscans, 1)).flatten()
    obs_data[('solarZenithAngle', metaDataName)] = np.array(g['All_Data']['ATMS-SDR-GEO_All']['SolarZenithAngle'][:, :].flatten(), dtype='float32')
    obs_data[('solarAzimuthAngle', metaDataName)] = np.array(g['All_Data']['ATMS-SDR-GEO_All']['SolarAzimuthAngle'][:, :].flatten(), dtype='float32')
    obs_data[('sensorZenithAngle', metaDataName)] = np.array(g['All_Data']['ATMS-SDR-GEO_All']['SatelliteZenithAngle'][:, :].flatten(), dtype='float32')
    obs_data[('sensorAzimuthAngle', metaDataName)] = np.array(g['All_Data']['ATMS-SDR-GEO_All']['SatelliteAzimuthAngle'][:, :].flatten(), dtype='float32')
    # put in approximate
    # scanang = (real(ibeam) - 1.0) * 1.11 -  52.726
    obs_data[('sensorViewAngle', metaDataName)] = (obs_data[('scan_position', metaDataName)] - 1.0) * 1.11 - 52.726

    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(g['obs_time_utc']), dtype='int64')

    # example: dimension ( 180, 96, 22 ) == dimension( nscan, nbeam_pos, nchannel )
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])

    # this section is for the NOAA CLASS files and need to be tested
    scaled_data = np.vstack(f['All_Data']['ATMS-SDR_All']['BrightnessTemperature'])
    scale_fac = f['All_Data']['ATMS-SDR_All']['BrightnessTemperatureFactors'][:].flatten()

    obs_data[('brightnessTemperature', obsValName)] = np.array((scaled_data * scale_fac[0]) + scale_fac[1], dtype='float32')
    obs_data[('brightnessTemperature', obsErrName)] = np.full((nlocs, nchans), 5.0, dtype='float32')
    obs_data[('brightnessTemperature', qcName)] = np.full((nlocs, nchans), 0, dtype='int32')

    if add_qc:
        obs_data = atms_gross_quality_control(obs_data)

    return obs_data


def atms_gross_quality_control(obs_data):

    tb_key = 'brightnessTemperature'
    # anomalous temperatures pre-Beta data
    # 20221125T12Z_PT6H and 20221127T06_PT6H
    # quality top values for ch15 330K arises from this
    good = (obs_data[(tb_key, obsValName)][:, 0] > 10) & \
        (obs_data[(tb_key, obsValName)][:, 0] < 400) & \
        (obs_data[(tb_key, obsValName)][:, 14] > 10) & \
        (obs_data[(tb_key, obsValName)][:, 14] < 330) & \
        (obs_data[(tb_key, obsValName)][:, 16] > 10) & \
        (obs_data[(tb_key, obsValName)][:, 16] < 400) & \
        (obs_data[('latitude', metaDataName)] >= -90) & \
        (obs_data[('latitude', metaDataName)] <= 90) & \
        (obs_data[('sensorZenithAngle', metaDataName)] > 0) & \
        (obs_data[('sensorZenithAngle', metaDataName)] < 80)

    for k in obs_data:
        if metaDataName in k[1] and 'sensorChannelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good]
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :]

    return obs_data


def get_WMO_satellite_ID(filename):

    afile = os.path.basename(filename)
    if 'SNPP' in afile or 'npp' in afile:
        WMO_sat_ID = SNPP_WMO_sat_ID
    elif 'J1' in afile or 'j01' in afile:
        WMO_sat_ID = NOAA20_WMO_sat_ID
    elif 'J2' in afile or 'j02' in afile:
        WMO_sat_ID = NOAA21_WMO_sat_ID
    else:
        WMO_sat_ID = -1
        print(f"could not determine satellite from filename: {afile}")
        sys.exit()

    return WMO_sat_ID


def get_epoch_time(obs_time_utc):

    # ugh this does not seem generic
    year = obs_time_utc[:, :, 0].flatten()
    month = obs_time_utc[:, :, 1].flatten()
    day = obs_time_utc[:, :, 2].flatten()
    hour = obs_time_utc[:, :, 3].flatten()
    minute = obs_time_utc[:, :, 4].flatten()
    second = obs_time_utc[:, :, 5].flatten()

    # following examples here could be written better potentially
    iterables = [year, month, day, hour, minute, second]
    # ensure the year is plausible (65535 appears in some data) if not set to 01Jan1900 (revisit)
    this_datetime = [datetime(adate[0], adate[1], adate[2], adate[3], adate[4], adate[5])
                     if adate[0] < 2200 else datetime(2200, 1, 1, 0, 0, 0)
                     for adate in zip(*iterables)]

    time_offset = [round((adatetime - epoch).total_seconds()) for adatetime in this_datetime]

    return time_offset


def create_string_dtg(obs_datetime, offset=datetime(1982, 1, 1, 0, 0).timestamp()):

    dtg = []
    for adatetime in obs_datetime:
        cdtg = datetime.fromtimestamp(adatetime/1.e6-offset).strftime("%Y-%m-%dT%H:%M:%SZ")
        dtg.append(cdtg)

    return dtg


def get_string_dtg(obs_time_utc):

    year = obs_time_utc[:, :, 0].flatten()
    month = obs_time_utc[:, :, 1].flatten()
    day = obs_time_utc[:, :, 2].flatten()
    hour = obs_time_utc[:, :, 3].flatten()
    minute = obs_time_utc[:, :, 4].flatten()
    second = 0
    dtg = []
    for i, yyyy in enumerate(year):
        cdtg = ("%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (yyyy, month[i], day[i], hour[i], minute[i]))
        if "655" in cdtg:
            cdtg = ("%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (2200, 1, 1, 0, 0))
        dtg.append(cdtg)

    return dtg


def init_obs_loc():
    obs = {
        ('brightnessTemperature', obsValName): [],
        ('brightnessTemperature', obsErrName): [],
        ('brightnessTemperature', qcName): [],
        ('sensorChannelNumber', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('sensorScanPosition', metaDataName): [],
        ('solarZenithAngle', metaDataName): [],
        ('solarAzimuthAngle', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
        ('sensorAzimuthAngle', metaDataName): [],
        ('sensorViewAngle', metaDataName): [],
    }

    return obs


def concat_obs_dict(obs_data, append_obs_data):
    # For now we are assuming that the obs_data dictionary has the "golden" list
    # of variables. If one is missing from append_obs_data, a warning will be issued.
    append_keys = list(append_obs_data.keys())
    for gv_key in obs_data.keys():
        if gv_key in append_keys:
            obs_data[gv_key] = np.append(obs_data[gv_key], append_obs_data[gv_key], axis=0)
        else:
            print("WARNING: ", gv_key, " is missing from append_obs_data dictionary")


def set_metadata_attributes(VarAttrs):
    VarAttrs[('sensorZenithAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('sensorViewAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('solarZenithAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('sensorAzimuthAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('solarAzimuthAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
    VarAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value

    return VarAttrs


def set_obspace_attributes(VarAttrs):
    VarAttrs[('brightnessTemperature', obsValName)]['units'] = 'K'
    VarAttrs[('brightnessTemperature', obsErrName)]['units'] = 'K'

    VarAttrs[('brightnessTemperature', obsValName)]['_FillValue'] = float_missing_value
    VarAttrs[('brightnessTemperature', obsErrName)]['_FillValue'] = float_missing_value
    VarAttrs[('brightnessTemperature', qcName)]['_FillValue'] = int_missing_value

    return VarAttrs


def remapBG(input_files):

    print('Remap ATMS: Backus–Gilbert Inversion method')

    obs_data = init_obs_loc()

    src_ch = [1, 2]
    tgt_ch = [1, 2]
    src_bm = 5.2
    tgt_bm = 3.3
    nfov = 96

    orb = 'des'
    orbit_num = 'non'
    ta_vmin, ta_vmax = 210, 275
    dif_vmin, dif_vmax = -5, 5

    coef_dir = str(Path(__file__).parent/'apply_BG'/'Coeff_5.2to3.3')
    apply_BG_obj = apply_BG_class(input_files,
                                  orb, orbit_num,
                                  src_ch,
                                  coef_dir,
                                  nfov,
                                  ta_vmin, ta_vmax,
                                  dif_vmin, dif_vmax)
    apply_BG_obj.ingest()
    apply_BG_obj.prepcoef()

    lat = []
    lon = []
    ta = []
    ta_rmp = []
    satzen = []
    satazi = []
    solzen = []
    solazi = []
    tim = []
    taAllCh = []
    viewang = []
    [viewang, taAllCh, lat, lon, satzen, satazi, solzen, solazi, tim] = apply_BG_obj.apply()

    g = h5py.File(input_files[0], 'r')
    WMO_sat_ID = get_WMO_satellite_ID(input_files[0])
    # example: dimension ( 135, 96 ) == dimension( nscan, nbeam_pos )
    nscans = lat.shape[0]
    print('nscans=', nscans)
    nbeam_pos = lat.shape[1]
    print('nbeam_pos=', nbeam_pos)
    obs_data[('latitude', metaDataName)] = np.array(lat.flatten(), dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(lon.flatten(), dtype='float32')
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(g['channel'][:], dtype='int32')
    obs_data[('sensorScanPosition', metaDataName)] = np.tile(np.arange(nbeam_pos, dtype='float32') + 1, (nscans, 1)).flatten()
    obs_data[('solarZenithAngle', 'MetaData')] = np.array(solzen.flatten(), dtype='float32')
    obs_data[('solarAzimuthAngle', 'MetaData')] = np.array(solazi.flatten(), dtype='float32')
    obs_data[('sensorZenithAngle', 'MetaData')] = np.array(satzen.flatten(), dtype='float32')
    obs_data[('sensorAzimuthAngle', 'MetaData')] = np.array(satazi.flatten(), dtype='float32')

    obs_data[('sensorViewAngle', metaDataName)] = np.array(viewang.flatten(), dtype='float32')
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')

    # example: dimension ( 135, 96, 22 ) == dimension( nscan, nbeam_pos, nchannel )
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])
    nlocs = len(obs_data[('latitude', metaDataName)])

    obs_data[('brightnessTemperature', obsValName)] = np.array(np.vstack(taAllCh), dtype='float32')
    obs_data[('brightnessTemperature', obsErrName)] = np.full((nlocs, nchans), 5.0, dtype='float32')
    obs_data[('brightnessTemperature', qcName)] = np.full((nlocs, nchans), 0, dtype='int32')

    for timearray in tim:
        timearray[:1] = timearray[0]
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(timearray), dtype='int64')

    # if add_qc:
    #    obs_data = atms_gross_quality_control(obs_data)

    return obs_data


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description=(
            'Reads the satellite data '
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of satellite observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-o', '--output',
        help='path to output ioda file',
        type=str, default=os.getcwd())
    optional.add_argument(
        '-m', '--remap',
        help="method for remapping ATMS: BG",
        type=str, default=os.getcwd())

    args = parser.parse_args()

    main(args)
