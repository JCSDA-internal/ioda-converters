#!/usr/bin/env python3

#
# (C) Copyright 2020-2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#


"""
Python code to ingest netCDF4 IRS data
"""
from matplotlib import pyplot as plt
import argparse
from datetime import datetime, timezone, timedelta
import os.path
import sys
import glob
import h5py
import netCDF4 as nc
import numpy as np
import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import set_metadata_attributes, set_obspace_attributes
from pyiodaconv.def_jedi_utils import compute_scan_angle
from pyiodaconv.def_jedi_utils import ioda_int_type, ioda_float_type, epoch
from pyiodaconv.def_jedi_utils import concat_obs_dict
from functools import partial
from concurrent.futures import ProcessPoolExecutor

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# Hardwire time instead of reading attribute, because it's formatted wrong in
# proxy data product (2000:01:01)
iso8601_string_irs = "seconds since 2000-01-01T00:00:00Z"
epoch_irs = datetime.fromisoformat(iso8601_string_irs[14:-1])
epoch_irs = epoch_irs.replace(tzinfo=timezone.utc)
irs_offset = (epoch_irs - epoch).total_seconds()


# globals
# from https://github.com/wmo-im/CCT/blob/master/C05.csv
# AKA METEOSAT-13 and METEOSAT-16
MTG_S1_WMO_sat_ID = 72
MTG_S2_WMO_sat_ID = 75
# from https://github.com/wmo-im/CCT/blob/master/C08.csv
IRS_WMO_inst_ID = 212

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

GlobalAttrs = {
    "platformCommonName": "IRS",
    "platformLongDescription": "MeteoSat Third Generation MTG-IRS Principle Component Score Data",
}


def main(args):

    obs_data = False
    # convert namespace to a dictionary
    task_params = vars(args)
    input_files = task_params.pop('input')
    for iii, fff in enumerate(input_files):
        print(iii, fff)
    output_file = task_params.pop('output')
    dtg = task_params.pop('date')
    threads = task_params.pop('threads')
    func_with_args = partial(get_data_from_files, **task_params)
    with ProcessPoolExecutor(max_workers=threads) as executor:
        for file_obs_data in executor.map(func_with_args, input_files):
            if not file_obs_data:
                print("INFO: non-nominal file skipping")
                continue
            if obs_data:
                concat_obs_dict(obs_data, file_obs_data[1])
            else:
                obs_data = file_obs_data[1]
                wavenumber = file_obs_data[0]

    # serial option
#   for afile in input_files:
#       file_obs_data = get_data_from_files(afile, **task_params)
#       if obs_data:
#           concat_obs_dict(obs_data, file_obs_data[1])
#       else:
#           obs_data = file_obs_data[1]
#           wavenumber = file_obs_data[0]

    obs_data[('sensorCentralWavenumber', metaDataName)] = np.array(wavenumber, dtype='float32')
    obs_data[('sensorChannelNumber', metaDataName)] = np.arange(1, len(wavenumber)+1, dtype='int32')
    nlocs_int = np.array(len(obs_data[('latitude', metaDataName)]), dtype='int64')
    nlocs = nlocs_int.item()
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])

    if nlocs == 0:
        print(f'  ...  WARNING: no data found exiting without writing output')
        return

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs['converter'] = os.path.basename(__file__)
    if dtg:
        GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")

    # pass parameters to the IODA writer
    VarDims = {
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
        'sensorCentralWavenumber': ['Channel'],
    }
    DimDict = {
        'Location': nlocs,
        'Channel': obs_data[('sensorChannelNumber', metaDataName)],
    }
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    set_obspace_attributes(VarAttrs)
    # set_metadata_attributes(VarAttrs)

    k = 'brightnessTemperature'
    VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[(k, 'ObsValue')]['units'] = 'K'
    VarAttrs[(k, 'ObsError')]['units'] = 'K'

    obs_data[('longitude', metaDataName)] = obs_data[('longitude', metaDataName)] % 360
    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def readMatrix(f, band):
    h5 = h5py.File(f, 'r')
    ReconstructionOperator = np.asarray(h5[band + '/reconstruction_operator'])
    Mean = np.asarray(h5[band + '/mean_spectrum'])
    h5.close()
    # flip reconstruction operator to be consistent with what is done for
    # local reconstruction operator
    return ReconstructionOperator.T, Mean


def applyPc(f, scores, operator_local, scores_local, band):
    R, meanz = readMatrix(f, band)
    Rl = operator_local
    # reshape like this to preserve dwells and columns
    s = np.asarray(scores).transpose([2, 0, 1])
    sl = np.asarray(scores_local).transpose([2, 0, 1])
    # make a storage variable for rads
    # l = pc dim, m/n spatial dim
    # k spectral dim
    l, m, n = s.shape
    k = R[:, :].shape[0]

    # flatten along spatial dimensions
    ss = s.reshape(l, m*n)

    # flatten local along spatial dimensions
    # ll = local pc dim
    ll, _, _ = sl.shape
    ssl = sl.reshape(ll, m*n)

    # do the math/reconstruct the radiances
    # note matlab code (used in latest proxy data documentation)
    # will be a little different owing to how it deals with
    # rows/columns pagemtimes, etc.

    r = meanz.reshape(1, meanz.shape[0]).T + np.dot(R, ss)   # + np.dot(Rl,ssl)
    return r


def snake_2_camel(snake):
    items = snake.split('_')
    return items[0]+''.join(i.title() for i in items[1::])


def r2tb(Radiance, nu, c1=1.191042972e-16, c2=1.4387769e-2):
    cc2 = c2*nu
    cc1 = c1*nu**3
    Temperature = cc2 / np.log((cc1 / Radiance) + 1.0)
    return Temperature


def get_data_from_files(afile, resolution=160, scan_shape=(160, 160), include_reconstructed_radiance=False, baseEV=None):
    print('processing file', afile)
    f = nc.Dataset(afile)
    obs_data = {}

    # for every data in "data" group make it metadata and camelCase
    for k in f['data'].variables.keys():
        if ('dwell' in k or 'time' in k or 'stroke' in k):
            continue
        dtype = str(f['data/'+k][:].dtype)
        kCamel = snake_2_camel(k)
        if ('float' in dtype):
            obs_data[(kCamel, metaDataName)] = np.array(f['data/'+k][:], dtype='float32')
        elif ('int' in dtype):
            obs_data[(kCamel, metaDataName)] = np.array(f['data/'+k][:], dtype='int32')

    obs_data[('dateTime', metaDataName)] = np.full(f['data/longitude'].shape, f['data/time'][:]+irs_offset, dtype='float64')
    obs_data[('dwellType', metaDataName)] = np.full(f['data/longitude'].shape, f['data/dwell_type'][:], dtype='int32')
    obs_data[('dwellNumber', metaDataName)] = np.full(f['data/longitude'].shape, f['data/dwell_number'][:], dtype='int32')

    # fill in wavenumbers and radiance values
    lw_quality = {}
    for k in f['data/lwir/quality_band'].variables.keys():
        if ('warning' in k and 'number' not in k):
            lw_quality[k] = f['data/lwir/quality_band/'+k][:]

    mw_quality = {}
    for k in f['data/mwir/quality_band'].variables.keys():
        if ('warning' in k and 'number' not in k):
            mw_quality[k] = f['data/mwir/quality_band/'+k][:]
    overall_quality = np.zeros(mw_quality['instrument_quality_warning'].shape, dtype='int32')

    # revisit this if quality keys differ in the future.
    for k in mw_quality.keys():
        overall_quality += mw_quality[k].astype('int32') + lw_quality[k].astype('int32')

    wn_lw = np.asarray(f['data/lwir/wavenumber'][:]).astype('float64')
    wn_mw = np.asarray(f['data/mwir/wavenumber'][:]).astype('float64')
    all_wn = np.concatenate([wn_lw, wn_mw])

    # only when requested compute reconstructed radiances and add to IODA output
    if include_reconstructed_radiance:
        rads_lw = applyPc(baseEV,
                          f['data/lwir/compressed/global_pc_scores'][:],
                          f['data/lwir/compressed/local_pcr_operator'][:],
                          f['data/lwir/compressed/local_pc_scores'][:], 'lwir')
        rads_mw = applyPc(baseEV,
                          f['data/mwir/compressed/global_pc_scores'][:],
                          f['data/mwir/compressed/local_pcr_operator'][:],
                          f['data/mwir/compressed/local_pc_scores'][:], 'mwir')
        rads = np.concatenate([rads_lw, rads_mw])
        # put reconstructed radiances into output if requested
        obs_data[('brightnessTemperature', obsValName)] = (
            np.array(r2tb(rads[:].T, all_wn), dtype='float32').
            transpose().
            reshape(all_wn.shape[0], *scan_shape)
        )
    pcname = 'principalComponentScore{}'
    nscore_lw = f['data/lwir/compressed/global_pc_scores'][:].shape[2]
    nscore_mw = f['data/mwir/compressed/global_pc_scores'][:].shape[2]

    big_score = np.zeros([scan_shape[0], scan_shape[1], nscore_lw+nscore_mw])
    big_score[:, :, 0:nscore_lw] = np.asarray(f['data/lwir/compressed/global_pc_scores'][:])     # .T
    big_score[:, :, nscore_lw:nscore_lw+nscore_mw] = np.asarray(f['data/mwir/compressed/global_pc_scores'][:])   # .T
    big_score = big_score.astype('float32')
    for i in range(nscore_lw+nscore_mw):
        obs_data[(pcname.format(i+1), metaDataName)] = big_score[:, :, i]
    obs_data = subsample_and_flatten(obs_data, resolution=resolution)
    obs_data = assign_WMO_ID(obs_data, f.platform)

    return all_wn, obs_data


def subsample_and_flatten(obs_data, resolution=160, base_resolution=4):

    """
    Downsamples observation grids based on resolution and
    flattens spatial dimensions for downstream processing.

    Inputs
    obs_data :  observation data dictionary
    resolution : desired output resolution in km
    base_resolution : assumed full resolution in km

    Output:
    obs_data_out : sampled and flattened arrays

    """
    stride = int(resolution / base_resolution)
    # Calculate the center of the stride block
    # For stride=1 (no thinning), offset is 0
    # For stride=40 (thinning), offset is 20
    offset = stride // 2
    obs_data_out = {}

    for k, data in obs_data.items():
        # Case 1: 3D Data (e.g., [Channels, Rows, Cols])
        if data.ndim == 3 and 'brightnessTemperature' in str(k):
            # Slicing with [0::1] is effectively a no-op, keeping logic consistent
            sliced = data[:, offset::stride, offset::stride]
            nchans, rows, cols = sliced.shape
            # Reshape to [Points, Channels]
            obs_data_out[k] = sliced.reshape(nchans, rows * cols).T

        # Case 2: 2D Data (e.g., [Rows, Cols] Lat/Lon grids)
        elif data.ndim == 2:
            obs_data_out[k] = data[offset::stride, offset::stride].flatten()

        # Case 3: 1D or Scalar Data
        else:
            obs_data_out[k] = data

    return obs_data_out


def assign_WMO_ID(obs_data, platform):
    nlocs = len(obs_data[('latitude', metaDataName)])
    if platform == 'MTS1':
        WMO_sat_ID = MTG_S1_WMO_sat_ID
    elif platform == 'MTS2':
        WMO_sat_ID = MTG_S2_WMO_sat_ID
    else:
        print(f" Warning unknown satellite: {platform=}")
        WMO_sat_ID = int_missing_value
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
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
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-o', '--output',
        help='path to output ioda file',
        type=str, default=os.path.join(os.getcwd(), 'output.nc4'))
    optional.add_argument(
        '--resolution',
        type=int,
        choices=[4, 8, 16, 32, 64, 128, 160],
        default=160,
        help="Target resolution in km [Base is 4km (i.e. no thinning); default is 160km]")
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, default=None)
    optional.add_argument(
        '--scan_shape',
        nargs=2,
        metavar=('ROWS', 'COLS'),
        help="Dimensions of the scan (default: 160 160)",
        type=int,
        default=(160, 160),)
    optional.add_argument(
        '--include_reconstructed_radiance',
        help="include computation of reconstructed radiances (requires --baseEV)",
        action='store_true',)
    optional.add_argument(
        '--baseEV',
        help="full path to PC base to project over",
        type=str,
        default=None,)
    args = parser.parse_args()
    # Check dependency: if flag is True, baseEV must not be None
    if args.include_reconstructed_radiance and args.baseEV is None:
        parser.error("--include_reconstructed_radiance requires --baseEV to be specified.")

    # Check file existence: if baseEV is provided, verify the path
    if args.baseEV:
        if not os.path.isfile(args.baseEV):
            parser.error(f"The file specified in --baseEV does not exist: {args.baseEV}")

    if args.date:
        try:
            args.date = datetime.strptime(args.date, "%Y%m%d%H")
        except ValueError:
            parser.error(f"Invalid date format: '{args.date}'. Expected YYYYMMDDHH")

    main(args)
