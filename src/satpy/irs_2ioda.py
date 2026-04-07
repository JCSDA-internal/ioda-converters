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
import argparse
from datetime import datetime, timezone, timedelta
import os.path
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


# Map coverage string attribute to a 0-based integer index
coverage_2_index = {'Q1': 0, 'Q2': 1, 'Q3': 2, 'Q4': 3}

# create a set of pairs that are
# the next lowest allowable resolution
resolution_pairs = {
    160: 128,
    128: 64,
    64: 32,
    32: 16,
    16: 8,
    8: 4,
    4: 4,
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
    if (threads > 1):
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
    else:
        for afile in input_files:
            file_obs_data = get_data_from_files(afile, **task_params)
            if obs_data:
                concat_obs_dict(obs_data, file_obs_data[1])
            else:
                obs_data = file_obs_data[1]
                wavenumber = file_obs_data[0]

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
        'radiance': ['Location', 'Channel'],
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

    k = 'radiance'
    VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[(k, 'ObsValue')]['units'] = 'W m-2 sr-1 m'
    VarAttrs[(k, 'ObsError')]['units'] = 'W m-2 sr-1 m'

    obs_data[('longitude', metaDataName)] = obs_data[('longitude', metaDataName)] % 360
    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def apodizeHammingMatmul(spectrum):
    A = np.zeros([max(spectrum.shape), max(spectrum.shape)])
    np.fill_diagonal(A, 0.54)
    msk_upper = np.eye(A.shape[0], k=1, dtype=bool)
    msk_lower = np.eye(A.shape[0], k=-1, dtype=bool)
    A[msk_upper] = 0.23
    A[msk_lower] = 0.23
    # Apply Boundary condition for 2 point vs 3 point window on edges
    A[0, 0] = A[0, 0]/0.77
    A[0, 1] = A[0, 1]/0.77
    A[-1, -2] = A[-1, -2]/0.77
    A[-1, -1] = A[-1, -1]/0.77
    spectrum_out = A@spectrum.T
    return spectrum_out.T


def readMatrix(f, band, apodize=False):
    h5 = h5py.File(f, 'r')
    ReconstructionOperator = np.asarray(h5[band + '/reconstruction_operator'])
    Mean = np.asarray(h5[band + '/mean_spectrum'])
    h5.close()
    if (apodize):
        ReconstructionOperator = apodizeHammingMatmul(ReconstructionOperator)
        Mean = apodizeHammingMatmul(Mean)
    # flip reconstruction operator to be consistent with what is done for
    # local reconstruction operator
    return ReconstructionOperator.T, Mean


def applyPc(f, scores, operator_local, scores_local, band, apodize):
    R, meanz = readMatrix(f, band, apodize)
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


def get_data_from_files(
    afile,
    resolution=160,
    scan_shape=(160, 160),
    include_reconstructed_radiance=False,
    include_reconstructed_tb=False,
    apodize_reconstructed=False,
    baseEV=None,
    max_zenith=90,
    step_resolution=None,
    resolution_step=60,
    get_largest_pc=False,
    get_cloud_fraction=False,
    window=3,
    max_dwells=73
):
    print('processing file', afile)
    f = nc.Dataset(afile)
    obs_data = {}

    # for every data in "data" group make it metadata and camelCase
    for k in f['data'].variables.keys():
        if ('dwell' in k or 'time' in k or 'stroke' in k):
            continue
        dtype = str(f['data/'+k][:].dtype)
        kCamel = snake_2_camel(k)

        if ('Angle' in kCamel and 'satellite' in kCamel):
            kCamel = kCamel.replace('satellite', 'sensor')
        if ('float' in dtype):
            obs_data[(kCamel, metaDataName)] = np.array(f['data/'+k][:], dtype='float32')
        elif ('int' in dtype):
            obs_data[(kCamel, metaDataName)] = np.array(f['data/'+k][:], dtype='int32')

    obs_data[('dateTime', metaDataName)] = np.full(f['data/longitude'].shape, f['data/time'][:]+irs_offset, dtype='float64')
    obs_data[('dwellType', metaDataName)] = np.full(f['data/longitude'].shape, f['data/dwell_type'][:], dtype='int32')
    obs_data[('dwellNumber', metaDataName)] = np.full(f['data/longitude'].shape, f['data/dwell_number'][:], dtype='int32')

    # --- sensorScanPosition: unique identifier across all coverages and pixels ---
    # Encoding: coverage_idx * (N_DWELLS * n_rows * n_cols)
    #         + dwell_number  * (n_rows * n_cols)
    #         + dwell_row     * n_cols
    #         + dwell_col
    #
    # coverage attribute is one of Q1, Q2, Q3, Q4 (N dwells each default 73).
    # dwell_number is converted 0-based within the coverage (0–N).
    #    dwell_row and dwell_col are the 2D pixel indices within the dwell (scan_shape).

    coverage_str = str(f.coverage)                             # e.g. 'Q2'
    coverage_idx = coverage_2_index.get(coverage_str, -1)
    if coverage_idx == -1:
        raise ValueError(f"Unknown coverage attribute '{coverage_str}'. "
                         f"Expected one of {list(coverage_2_index.keys())}.")

    n_rows, n_cols = scan_shape
    dwell_num = int(f['data/dwell_number'][:]) - 1             # scalar, 0-based

    # Build row/col index grids that match the spatial shape of the dwell
    row_idx = np.arange(n_rows, dtype='int32')
    col_idx = np.arange(n_cols, dtype='int32')
    row_grid, col_grid = np.meshgrid(row_idx, col_idx, indexing='ij')  # (n_rows, n_cols)

    # multiplication in int64 is on purpose to avoid potential silent overflow
    obs_data[('sensorScanPosition', metaDataName)] = (
        np.int64(coverage_idx) * np.int64(max_dwells * n_rows * n_cols)
        + np.int64(dwell_num) * np.int64(n_rows * n_cols)
        + row_grid * np.int64(n_cols)
        + col_grid
    ).astype('int32')

    sat_alt = f['state/platform/platform_altitude'][0]

    cnt_nx, cnt_ny = obs_data[('sensorZenithAngle', metaDataName)].shape

    # compute_scan angle is kind of odd. Only 2nd and 3rd args do anything.
    obs_data[('sensorViewAngle', metaDataName)] = compute_scan_angle(
        obs_data[('sensorZenithAngle', metaDataName)].flatten(),
        sat_alt*np.ones(cnt_nx*cnt_ny),
        obs_data[('sensorZenithAngle', metaDataName)].flatten()).reshape(cnt_nx, cnt_ny).astype('float32')

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

    obs_data[('overallQuality', metaDataName)] = overall_quality

    wn_lw = np.asarray(f['data/lwir/wavenumber'][:]).astype('float64')
    wn_mw = np.asarray(f['data/mwir/wavenumber'][:]).astype('float64')
    all_wn = np.concatenate([wn_lw, wn_mw])

    # only when requested compute reconstructed radiances and add to IODA output
    if include_reconstructed_radiance or include_reconstructed_tb:
        rads_lw = applyPc(baseEV,
                          f['data/lwir/compressed/global_pc_scores'][:],
                          f['data/lwir/compressed/local_pcr_operator'][:],
                          f['data/lwir/compressed/local_pc_scores'][:], 'lwir', apodize_reconstructed)
        rads_mw = applyPc(baseEV,
                          f['data/mwir/compressed/global_pc_scores'][:],
                          f['data/mwir/compressed/local_pcr_operator'][:],
                          f['data/mwir/compressed/local_pc_scores'][:], 'mwir', apodize_reconstructed)
        rads = np.concatenate([rads_lw, rads_mw])
        # put reconstructed radiances into output if requested
        if (include_reconstructed_tb):
            obs_data[('brightnessTemperature', obsValName)] = (
                np.array(r2tb(rads[:].T, all_wn), dtype='float32').
                transpose().
                reshape(all_wn.shape[0], *scan_shape)
            )
        if (include_reconstructed_radiance):
            obs_data[('radiance', obsValName)] = (rads.reshape(all_wn.shape[0], *scan_shape).astype('float32'))

    pcname = 'principalComponentScore{}'
    nscore_lw = f['data/lwir/compressed/global_pc_scores'][:].shape[2]
    nscore_mw = f['data/mwir/compressed/global_pc_scores'][:].shape[2]

    big_score = np.zeros([scan_shape[0], scan_shape[1], nscore_lw+nscore_mw])
    big_score[:, :, 0:nscore_lw] = np.asarray(f['data/lwir/compressed/global_pc_scores'][:])     # .T
    big_score[:, :, nscore_lw:nscore_lw+nscore_mw] = np.asarray(f['data/mwir/compressed/global_pc_scores'][:])   # .T
    big_score = big_score.astype('float32')

    for i in range(nscore_lw+nscore_mw):
        obs_data[(pcname.format(i+1), metaDataName)] = big_score[:, :, i]

    # save relevant PC score metrics
    obs_data[('globalPcrScoresLw', metaDataName)] = np.asarray(f['data/lwir/compressed/global_pcr_scores']).astype('float32')
    obs_data[('globalPcrScoresMw', metaDataName)] = np.asarray(f['data/mwir/compressed/global_pcr_scores']).astype('float32')

    obs_data[('globalPcrQualityLw', metaDataName)] = np.asarray(f['data/lwir/compressed/global_pcrs_quality']).astype('int32')
    obs_data[('globalPcrQualityMw', metaDataName)] = np.asarray(f['data/mwir/compressed/global_pcrs_quality']).astype('int32')

    obs_data[('spatialSampleQualityLw', metaDataName)] = np.asarray(f['data/lwir/compressed/spatial_sample_quality']).astype('int32')
    obs_data[('spatialSampleQualityMw', metaDataName)] = np.asarray(f['data/mwir/compressed/spatial_sample_quality']).astype('int32')

    obs_data[('detectorSampleQualityLw', metaDataName)] = np.asarray(f['data/lwir/compressed/detector_sample_quality']).astype('int32')
    obs_data[('detectorSampleQualityMw', metaDataName)] = np.asarray(f['data/mwir/compressed/detector_sample_quality']).astype('int32')

    obs_data = subsample_and_flatten(
        obs_data,
        scan_shape=scan_shape,
        resolution=resolution,
        max_zenith=max_zenith,
        get_largest_pc=get_largest_pc,
        window=window,
        step_resolution=step_resolution,
        resolution_step=resolution_step,
    )
    obs_data = assign_WMO_ID(obs_data, f.platform)

    return all_wn, obs_data


def get_clearest_fov(clear_quantity, scan_shape, stride, window, fill=1e300):
    """
    Vectorized version of clearest based on modified spoc implementation.
    For a given dwell, find the indices with the "clearest" field of view.

    Parameters
    ----------
    clear_quantity : np.ndarray, shape (scan_shape, scan_shape)
        A value indicating how "clear" the FOV is.
    scan_shape : int
        Number of pixels along one axis of the square dwell grid.
    stride : int
        Size of each thinning block in pixels.
    window : int
        Size of the inner search window within each block. Must satisfy:
            1 <= window <= stride
            (stride - window) % 2 == 0
    fill : float, optional
        Values >= fill are treated as missing and will never be selected.

    Returns
    -------
    imax : np.ndarray, shape (n_blocks²,)
        Global row indices of the selected best pixel per block.
    jmax : np.ndarray, shape (n_blocks²,)
        Global col indices of the selected best pixel per block.
        Blocks where all values are fill get index -1.
    """
    assert 1 <= window <= stride, (
        f"window must be between 1 and {stride}, got {window}"
    )
    assert (stride - window) % 2 == 0, (
        f"window={window} cannot be symmetrically centered in a "
        f"{stride}x{stride} block"
    )

    margin = (stride - window) // 2
    n_blocks = scan_shape // stride

    # mask fill values so they are never selected
    masked = np.where(np.abs(clear_quantity) < fill, clear_quantity, -np.inf)

    # Step 1: reshape into non-overlapping stride x stride blocks
    # masked shape: (scan_shape, scan_shape)
    # after reshape:    (n_blocks, stride, n_blocks, stride)
    # after transpose:  (n_blocks, n_blocks, stride, stride)
    # so blocks[bi, bj, ri, ci] == masked[bi*stride + ri, bj*stride + ci]
    blocks = (
        masked
        .reshape(n_blocks, stride, n_blocks, stride)
        .transpose(0, 2, 1, 3)
    )

    # Step 2: slice the inner window x window patch using margin
    # inner[bi, bj, li, lj] == masked[bi*stride + margin + li,
    #                                  bj*stride + margin + lj]
    inner = blocks[:, :, margin:margin + window, margin:margin + window]
    # shape: (n_blocks, n_blocks, window, window)

    # Step 3: flatten blocks and find argmax within each patch
    inner_flat = inner.reshape(n_blocks * n_blocks, window * window)
    flat_idx = np.argmax(inner_flat, axis=1)              # (n_blocks²,)
    local_i, local_j = np.unravel_index(flat_idx, (window, window))

    # Step 4: map back to global indices
    # flat block k -> bi = k // n_blocks, bj = k % n_blocks
    k = np.arange(n_blocks * n_blocks)
    bi = k // n_blocks
    bj = k % n_blocks

    imax = bi * stride + margin + local_i
    jmax = bj * stride + margin + local_j

    # Step 5: mark all-fill blocks as -1
    all_fill_mask = np.all(inner_flat == -np.inf, axis=1)
    imax[all_fill_mask] = -1
    jmax[all_fill_mask] = -1

    return imax, jmax


def get_idx_by_stride(offset, scan_shape, stride):
    i_idx = np.arange(offset, scan_shape[0], stride)
    j_idx = np.arange(offset, scan_shape[1], stride)
    ii, jj = np.meshgrid(i_idx, j_idx, indexing='ij')

    return ii.ravel(), jj.ravel()


def subsample_and_flatten(
    obs_data,
    scan_shape=(160, 160),
    resolution=160,
    base_resolution=4,
    max_zenith=90,
    step_resolution=False,
    get_largest_pc=False,
    get_cloud_fraction=False,
    window=3,
    resolution_step=60
):

    """
    Downsamples observation grids based on resolution and
    flattens spatial dimensions for downstream processing.

    Inputs
    obs_data :  observation data dictionary
    resolution : desired output resolution in km
    base_resolution : assumed full resolution in km
    max_zenith: maximum allowed sensorZenithAngle
    step_resolution: true/false allow for a step decrease in resolution
                     to next lowest allowable resolution (increase in point density)
    resolution_step: minimum zenith angle where step_resolution is applied

    Output:
    obs_data_out : sampled and flattened arrays

    """
    # modify based on zenith angle of center
    if (step_resolution):
        nx, ny = obs_data[('sensorZenithAngle', metaDataName)].shape
        # if the center FOV's sensorZenithAngle is above resolution_zenith_angle_switch
        # switch to the next denser resolution
        xc, yc = int(nx/2), int(ny/2)
        if (obs_data[('sensorZenithAngle', metaDataName)][xc, yc] > resolution_step):
            resolution = resolution_pairs[resolution]

    stride = int(resolution / base_resolution)
    # Calculate the center of the stride block
    # For stride=1 (no thinning), offset is 0
    # For stride=40 (thinning), offset is 20
    offset = stride // 2
    obs_data_out = {}
    if (get_largest_pc):
        ix, iy = get_clearest_fov(obs_data[('principalComponentScore1', metaDataName)], scan_shape[0], stride, window)
    elif (get_cloud_fraction):
        ix, iy = get_clearest_fov(1.0-obs_data[('cloudFraction', metaDataName)], scan_shape[0], stride, window)
    else:
        ix, iy = get_idx_by_stride(offset, scan_shape, stride)
    # create array of points which meet zenith angle cutoff criteria
    valid = np.zeros(obs_data[('sensorZenithAngle', metaDataName)].shape)
    valid[np.where(obs_data[('sensorZenithAngle', metaDataName)] < max_zenith)] = 1

    # apply same slice as done to data to valid array
    valid_sliced = valid[ix, iy]

    for k, data in obs_data.items():
        # Case 1: 3D Data (e.g., [Channels, Rows, Cols])
        if data.ndim == 3 and ('brightnessTemperature' in str(k) or 'radiance' in str(k)):
            # Slicing with [0::1] is effectively a no-op, keeping logic consistent
            sliced = data[:, ix, iy]
            idx = np.where(valid_sliced > 0)
            obs_data_out[k] = sliced[:, idx].T
        # Case 2: 2D Data (e.g., [Rows, Cols] Lat/Lon grids)
        elif data.ndim == 2:
            obs_data_out[k] = data[ix, iy]
            idx = np.where(valid_sliced > 0)
            obs_data_out[k] = obs_data_out[k][idx]
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
        '--include_reconstructed_tb',
        help="include computation of reconstructed brightness temperature (requires --baseEV)",
        action='store_true',)
    optional.add_argument(
        '--apodize_reconstructed',
        help="add hamming apodization reconstructed radiances or brightnesss temperature (requires --baseEV)",
        action='store_true',)
    optional.add_argument(
        '--baseEV',
        help="full path to PC base to project over",
        type=str,
        default=None,)
    optional.add_argument(
        '--step_resolution',
        help="Allow for a step in resolution to next lowest resolution",
        action='store_true')
    optional.add_argument(
        '--resolution_step',
        type=int,
        default=60,
        help="sensorZenithAngle threshold to apply next lowest resolution for step_resolution")
    optional.add_argument(
        '--max_zenith',
        type=int,
        default=90,
        help="sensorZenithAngle cutoff")
    optional.add_argument(
        '--get_largest_pc',
        help="Do warmest FOV using PC score",
        action='store_true')
    optional.add_argument(
        '--get_cloud_fraction',
        help="Do clearest pixel search.",
        action='store_true')
    optional.add_argument(
        '--window',
        type=int,
        default=3,
        help="search window for largest PC score preference")
    optional.add_argument(
        '--max_dwells',
        type=int,
        default=73,
        help="Maximum Number of Dwells in coverage area.")

    args = parser.parse_args()
    # Check dependency: if flag is True, baseEV must not be None
    if args.include_reconstructed_radiance and args.baseEV is None:
        parser.error("--include_reconstructed_radiance requires --baseEV to be specified.")

    # Check dependency: if flag is True, baseEV must not be None
    if args.include_reconstructed_tb and args.baseEV is None:
        parser.error("--include_reconstructed_tb requires --baseEV to be specified.")

    # Check file existence: if baseEV is provided, verify the path
    if args.baseEV:
        if not os.path.isfile(args.baseEV):
            parser.error(f"The file specified in --baseEV does not exist: {args.baseEV}")

    # Check only one warmest FOV option
    if args.get_largest_pc and args.get_cloud_fraction:
        parser.error("Select only one. Either --get_largest_pc or --get_cloud_fraction. Not Both.")

    if args.date:
        try:
            args.date = datetime.strptime(args.date, "%Y%m%d%H")
        except ValueError:
            parser.error(f"Invalid date format: '{args.date}'. Expected YYYYMMDDHH")

    main(args)
