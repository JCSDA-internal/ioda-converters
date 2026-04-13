#!/usr/bin/env python

#
# (C) Copyright 2020-2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# This script reads in EUMETSAT BASEEV file for MTG-IRS and outputs
# a reconstructor operator file compatible with the UFO pcscore to
# radiance variable transform.
#


import netCDF4, h5py, os, argparse
import numpy as np


def main(args):
    infile = args.input
    outfile = args.output
    method = args.apodization_method if args.apodize else ''
    subset = args.subset

    RR, M = readMatrix(infile, method, subset)
    if args.apodize:
        print(f"{infile} Read, applied apodization method: {method}")
    else:
        print(f"{infile} Read, no apodization. Use --apodize to enable.")
    writeFile(outfile, RR, M, subset)
    print(f"Output written to: {outfile}")


def selectApod(spectrum, method=''):
    if method == 'hamming_moving_avg':
        if len(spectrum.shape) > 1:
            spectrum_out = np.zeros(spectrum.shape)
            for i in range(0, spectrum.shape[0]):
                spectrum_out[i, :] = apodizeHammingMovingAvg(spectrum[i, :])
            return spectrum_out
        else:
            return apodizeHammingMovingAvg(spectrum)
    elif method == 'hamming_matmul':
        return apodizeHammingMatmul(spectrum)
    else:
        return spectrum


def apodizeHammingMovingAvg(spectrum):
    weights = np.asarray([0.54, 0.23])
    spectrum_apodized = np.zeros(spectrum.shape)
    # take care of end points special case (2 points in window vs. 3)
    spectrum_apodized[0] = (spectrum[0]*weights[0]+spectrum[1]*weights[1])/(weights[0]+weights[1])
    spectrum_apodized[-1] = (spectrum[-1]*weights[0]+spectrum[-2]*weights[1])/(weights[0]+weights[1])
    for i in range(1, spectrum_apodized.shape[0]-1):
        spectrum_apodized[i] = spectrum[i-1]*weights[1] + spectrum[i]*weights[0] + spectrum[i+1]*weights[1]

    return spectrum_apodized


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


def readMatrix(f, method, subset):
    """
    Reads EUMETSAT reconstruction operator from EUMETSAT, and optionally apply hamming apodization on top of light.
    The netcdf format is as follows:

    group: lwir {
    dimensions:
        phony_dim_0 = 150 ;
        phony_dim_1 = 877 ;
        phony_dim_2 = 877 ;
    variables:
        double eigenvectors(phony_dim_0, phony_dim_1) ;
        double mean_spectrum(phony_dim_1) ;
        double noise_normalisation(phony_dim_1, phony_dim_2) ;
        double reconstruction_operator(phony_dim_0, phony_dim_1) ;
    } // group lwir

    group: mwir {
    dimensions:
        phony_dim_3 = 150 ;
        phony_dim_4 = 1076 ;
        phony_dim_5 = 1076 ;
    variables:
        double eigenvectors(phony_dim_3, phony_dim_4) ;
        double mean_spectrum(phony_dim_4) ;
        double noise_normalisation(phony_dim_4, phony_dim_5) ;
        double reconstruction_operator(phony_dim_3, phony_dim_4) ;
    } // group mwir
    }
    """

    ff = h5py.File(f, 'r')
    RRop1 = selectApod(np.asarray(ff['lwir/reconstruction_operator']), method)
    RRop2 = selectApod(np.asarray(ff['mwir/reconstruction_operator']), method)
    M1 = selectApod(np.asarray(ff['lwir/mean_spectrum']), method)
    M2 = selectApod(np.asarray(ff['mwir/mean_spectrum']), method)
    ff.close()
    npcs1 = RRop1.shape[0]
    npcs2 = RRop2.shape[0]
    npcs = npcs1 + npcs2
    nchan1 = RRop1.shape[1]
    nchan2 = RRop2.shape[1]
    nchan = nchan1 + nchan2
    RRop = np.zeros([npcs, nchan])
    M = np.zeros(nchan)
    M[0:nchan1] = M1[:]
    M[nchan1:nchan] = M2[:]
    RRop[0:npcs1, 0:nchan1] = RRop1[:, :]
    RRop[npcs1:npcs, nchan1:nchan] = RRop2[:, :]
    if isinstance(subset, list):
        idx = np.asarray(subset) - 1
        RRop = RRop[:, idx]
        M = M[idx]
        min_idx_plus_1 = idx.min() + 1
        max_idx_plus_1 = idx.max() + 1
        # all subset channels in first set of PCs
        if max_idx_plus_1 <= nchan1:
            RRop = RRop[0:npcs1, :]
        # all subset channels in second set of PCs
        elif min_idx_plus_1 > nchan1:
            RRop = RRop[npcs1:npcs2, :]
    return RRop, M


def writeFile(outfile, RR, M, subset):
    with netCDF4.Dataset(outfile, 'w', format='NETCDF4') as f:
        nchan = M.shape[0]
        npcs = RR.shape[0]
        f.createDimension('Channel', nchan)
        f.createDimension('Component', npcs)
        chn = f.createVariable('Channel', 'i4', ('Channel',))
        comp = f.createVariable('Component', 'i4', ('Component',))
        if isinstance(subset, list):
            chan = subset
        else:
            chan = np.arange(1, nchan+1)
        chn[:] = chan
        comp[:] = np.arange(1, npcs+1)

        g = f.createGroup('MetaData')
        vv = g.createVariable('sensorChannelNumber', 'i4', ('Channel'))
        vv[:] = chan
        gg = f.createGroup('PCScores')
        vvv = gg.createVariable('reconstructionOperator', 'f4', ('Component', 'Channel'))
        vvv[:, :] = RR[:, :]
        ggg = f.createGroup('PCmean')
        vvvv = ggg.createVariable('reconstructionOperator', 'f4', ('Channel'))
        vvvv[:] = M[:]


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description=(
            'Reads in EUMETSAT BASEEV file for MTG-IRS and outputs '
            ' a reconstructor operator file compatible with the UFO  '
            ' pcscore to radiance variable transform.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="Input BASEEV file from EUMETSAT",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="Output Filename for reconstruction operator",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--apodize',
        help='add apodization (default False)',
        action='store_true', required=False)
    optional.add_argument(
        '--apodization_method',
        type=str,
        choices=['hamming_matmul', 'hamming_moving_avg'],
        default='hamming_matmul',
        help="Method if --apodize is flagged (default: %(default)s) (choices: %(choices)s)")
    optional.add_argument(
        '--subset',
        type=int,
        nargs="+",
        default=np.arange(1, 1954).astype('int').tolist(),
        help="specify subset of channels.")

    args = parser.parse_args()
    main(args)
