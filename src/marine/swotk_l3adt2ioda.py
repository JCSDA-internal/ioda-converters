#!/usr/bin/env python3

#
# (C) Copyright 2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# This file has been modified for NASA GMAO by David Russell on Sep 30, 2026
# It was modified to work on AVISO L3 KaRIn data instead of PODAAC L2 KaRIn/nadir data
# It also builds in optional super-obbing to a grid and obs error assignment

import argparse
import netCDF4 as nc
import numpy as np
import os
from scipy.ndimage import gaussian_filter

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

# # Super-obbing parameters
# superOb = True
# nx_superob = 3
# ny_superob = 3

# # Obs error method
# obserr_method = 'weighted_std' #'parabolic' #'weighted_std'

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

obsvars = {
    'adt': 'adt',
}

AttrData = {
    'converter': os.path.basename(__file__)
}

DimDict = {
}

VarDims = {
    'adt': ['Location'],
}

#def downsample_1d_array(data, n_box=3):
#    """Trims a 1D array and averages it by a given factor."""
#    # Calculate a new size that divides evenly
#    n_tot = data.shape[0]
#    n_tot_new = (n_tot // n_box) * n_box
#
#    # Trim the array to the new size
#    trimmed = data[:n_tot_new]
#
#    # Reshape into a 2D grid and take the mean of each row
#    # This automatically ignores masked values!
#    reshaped = trimmed.reshape(-1, n_box)
#    return reshaped.mean(axis=1)

# UPDATE this function's signature to use standard integer defaults instead of the globals
def downsample_2d_array(data, nx_box=3, ny_box=3, method='mean', masked=False):
# def downsample_2d_array(data, nx_box=nx_superob, ny_box=ny_superob, method='mean', masked=False):
    """Trims a 2D array and averages it by given factors."""
    # Calculate new sizes that divide evenly
    nx_tot, ny_tot = data.shape
    nx_tot_new = (nx_tot // nx_box) * nx_box
    ny_tot_new = (ny_tot // ny_box) * ny_box
    
    # Trim the array
    trimmed = data[:nx_tot_new, :ny_tot_new]
    
    if method == 'sample':
        return trimmed[nx_box//2::nx_box, ny_box//2::ny_box]
    else:
        # Reshape into blocks
        reshaped = trimmed.reshape(
            nx_tot_new // nx_box, nx_box,
            ny_tot_new // ny_box, ny_box
        )
        
        # Average the blocks and return
        # Careful! .mean masks the mean over a fully-masked box
        # But the value being masked can change (from the fill value to 0)
        # In many netCDF datasets, the raw data array defaults to filling empty slots with 0.0 under the mask
        # When writing to netCDF, the underlying value seems to be written
        # So we have to either change it back manually or drop masked values (currently the latter)

        if method == 'mean':
            if not masked:
                return reshaped.mean(axis=(1, 3))
            else:
                # If averaging over non-masked pixels:
                #return reshaped.mean(axis=(1, 3))
                # If masking wherever any masked pixel is found:
                averaged_data = reshaped.mean(axis=(1, 3))
                strict_mask = reshaped.mask.any(axis=(1, 3))
                return(np.ma.array(averaged_data.data, mask=strict_mask))
        elif method == 'max':
            if not masked:
                return reshaped.max(axis=(1, 3))
            else:
                # If averaging over non-masked pixels:
                #return reshaped.max(axis=(1, 3))
                # If masking wherever any masked pixel is found:
                max_data = reshaped.max(axis=(1, 3))
                strict_mask = reshaped.mask.any(axis=(1, 3))
                return(np.ma.array(max_data.data, mask=strict_mask))

def weighted_std(ma):
    # 2D Gaussian-weighted standard deviation of masked array ma

    # Set up parameters
    sigma_val = 50 # Standard deviation in pixels
    distance = 32 # Desired cutoff distance in pixels

    # Calculate the matching truncation value
    truncate_val = distance / sigma_val

    # Track missing data and boundaries using the MaskedArray's mask
    # In np.ma, True means masked/invalid. We want to extract the boolean mask first.
    bool_mask = np.ma.getmaskarray(ma)
    
    # Create the weights array: Valid pixels = 1.0, Masked = 0.0
    weights = (~bool_mask).astype(float)
    
    # Extract the raw data as a standard numpy array, filling masked values with 0.0
    # so they don't corrupt the convolution sum.
    data = ma.filled(0.0)

    # Dynamic Normalization: Filter both the data and the weights
    # This automatically scales the weights dynamically when encountering masks/boundaries
    weighted_sum = gaussian_filter(data, sigma=sigma_val, truncate=truncate_val, mode="constant", cval=0.0)
    weighted_sum_sq = gaussian_filter(data**2, sigma=sigma_val, truncate=truncate_val, mode="constant", cval=0.0)
    sum_of_weights = gaussian_filter(weights, sigma=sigma_val, truncate=truncate_val, mode="constant", cval=0.0)

    # Compute Weighted Means (divide out the weight totals)
    # Use np.errstate to ignore warnings where sum_of_weights is 0 (extreme edges or fully-masked areas)
    with np.errstate(divide="ignore", invalid="ignore"):
        mean = weighted_sum / sum_of_weights
        mean_sq = weighted_sum_sq / sum_of_weights

        # Variance = E[X^2] - (E[X])^2
        variance = mean_sq - (mean**2)

        # Avoid minor negative float precision artifacts before taking the square root
        variance = np.clip(variance, 0, None)
        std_dev = np.sqrt(variance)

    # 5. Convert back into a MaskedArray using the original boolean mask
    weighted_std_ma = np.ma.masked_array(std_dev, mask=bool_mask)
    
    # Optional: mask out locations where there weren't enough valid adjacent points
    # weighted_std_ma.mask |= (sum_of_weights < 0.1)

    return(weighted_std_ma)

class swotk_l3adt2ioda(object):
    # Add the arguments to __init__
    def __init__(self, filename, superOb=False, nx_superob=3, ny_superob=3, obserr_method='weighted_std'):
        self.filename = filename
        self.superOb = superOb
        self.nx_superob = nx_superob
        self.ny_superob = ny_superob
        self.obserr_method = obserr_method
        
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):
        # Open the input file and read the relevant data
        ncd = nc.Dataset(self.filename, 'r')

        # Don't do 1D downsampling for time, just expand first and then do 2D
        time_var = ncd.variables['time']
        # chop off milliseconds, set the units
        time_units = time_var.units[:-2] + "Z"
        s = list(time_units)
        s[24] = "T"
        time_units = "".join(s)
        num_pixels = ncd.dimensions['num_pixels'].size #// nx_superob # do the downsampling later
        self.time = np.zeros((len(time_var), num_pixels), dtype=np.int64)
        #for t in range(len(time_var)): # inefficient
        #    for n in range(num_pixels):
        #        self.time[t, n] = np.round(time_var[t])
        self.time[:] = time_var[:][:, np.newaxis] # more efficient

        # Estimate ADT
        # [:] converts from netCDF4._netCDF4.Variable to numpy.ma.core.MaskedArray
        self.mdt = ncd.variables['mdt'][:]
        self.ssha = ncd.variables['ssha_filtered'][:]
        adt = self.mdt + self.ssha

        # Assign the observation error
        # For now, across-track parabola or GMAO S2S-3 ODAS style
        if self.obserr_method.casefold() == 'parabolic':
            x = np.linspace(-68, 68, 69)
            inst_err = np.square(np.minimum(np.abs(x-34), np.abs(x+34)))*0.01/576 + 0.02
            adt_err = np.zeros((len(time_var), num_pixels))
            adt_err[:] = inst_err[:][np.newaxis, :]
        elif self.obserr_method.casefold() == 'weighted_std':
            adt_err = weighted_std(adt)
            adt_err = adt_err.clip(min=0.1)
            adt_err = adt_err + 0.1*(np.array(ncd.variables['latitude'])/90)**4
            adt_err = 0.2*adt_err

        # Mask other variables according to ADT
        mask2d = np.ma.getmaskarray(adt)
        adt_err = np.ma.masked_array(adt_err, mask=mask2d)
        self.lons = np.ma.masked_array(ncd.variables['longitude'][:], mask=mask2d)
        self.lats = np.ma.masked_array(ncd.variables['latitude'][:], mask=mask2d)
        self.qcflag = np.ma.masked_array(ncd.variables['quality_flag'][:], mask=mask2d)
        self.time = np.ma.masked_array(self.time, mask=mask2d)

        # Super-ob (thin and average) data
        if self.superOb:
            adt = downsample_2d_array(adt, nx_box=self.nx_superob, ny_box=self.ny_superob, masked=True)
            adt_err = downsample_2d_array(adt_err, nx_box=self.nx_superob, ny_box=self.ny_superob, masked=True)
            self.lats = downsample_2d_array(self.lats, nx_box=self.nx_superob, ny_box=self.ny_superob, masked=True)
            self.qcflag = downsample_2d_array(self.qcflag, nx_box=self.nx_superob, ny_box=self.ny_superob, masked=True, method='max')
            self.time = downsample_2d_array(self.time, nx_box=self.nx_superob, ny_box=self.ny_superob, masked=True, method='max')

            # Handle the 0-360 problem properly for longitude
            lons_rad = np.deg2rad(self.lons)
            x = np.cos(lons_rad)
            y = np.sin(lons_rad)
            mean_x = downsample_2d_array(x, nx_box=self.nx_superob, ny_box=self.ny_superob, masked=True)
            mean_y = downsample_2d_array(y, nx_box=self.nx_superob, ny_box=self.ny_superob, masked=True)
            # Convert the mean vectors back to degrees            
            with np.errstate(invalid='ignore'): 
                mean_lon_rad = np.arctan2(mean_y, mean_x)
            mean_lon_deg = np.rad2deg(mean_lon_rad)
            # Ensure the result falls within the [0, 360) range
            self.lons = mean_lon_deg % 360

        # Flatten, drop masked values
        adt = adt.compressed()
        adt_err = adt_err.compressed()
        self.lons = self.lons.compressed()
        self.lats = self.lats.compressed()
        self.qcflag = self.qcflag.compressed()
        self.time = self.time.compressed()

        Fillvalue = ncd.variables['ssha_filtered']._FillValue
        units = ncd.variables['ssha_filtered'].units
        scale_factor = ncd.variables['ssha_filtered'].scale_factor
        err_Fillvalue = ncd.variables['mdt']._FillValue
        err_units = ncd.variables['mdt'].units
        err_scale_factor = ncd.variables['mdt'].scale_factor

        ncd.close()

        # I think this step becomes unnecessary with .compressed() since it also removes mask
        #adt = np.where(self.ssha == Fillvalue, Fillvalue, self.ssha + self.mdt)
        # DSR: I changed the above line because I believe it was not working as intended
        # self.ssha is a masked array 
        # Apparently np.where() not built for masked arrays, so it looks for fill values in the unmasked parts
        # They are all masked, so the condition is never met
        # So if the fill values under the mask were changed (e.g. by downsampling), they are not restored
        #adt = self.ssha + self.mdt
        #adt = adt.filled(Fillvalue) 

        # Set up variable names for IODA
        iodavar = 'absoluteDynamicTopography'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.var_mdata[iodavar, iconv.OvalName()]['units'] = units
        self.var_mdata[iodavar, iconv.OerrName()]['units'] = err_units
        self.var_mdata[iodavar, iconv.OvalName()]['_FillValue'] = Fillvalue
        self.var_mdata[iodavar, iconv.OerrName()]['_FillValue'] = err_Fillvalue

        # Map SWOT KaRIn ADT to IODA data structure
        self.outdata[('dateTime', 'MetaData')] = self.time
        self.var_mdata[('dateTime', 'MetaData')]['units'] = time_units
        self.outdata[('latitude', 'MetaData')] = self.lats.astype('float32')
        self.var_mdata[('latitude', 'MetaData')]['units'] = "degrees_north"
        self.outdata[('longitude', 'MetaData')] = self.lons.astype('float32')
        self.var_mdata[('longitude', 'MetaData')]['units'] = "degrees_east"
        self.outdata[self.varDict[iodavar]['valKey']] = adt.astype('float32') # started as int anyway  -DSR
        # No uncertainty values so setting error to 1 cm
        self.outdata[self.varDict[iodavar]['errKey']] = adt_err.astype('float32')#0.01*np.ones(np.shape(self.lons)).astype('float32') # started as int anyway  -DSR
        self.outdata[self.varDict[iodavar]['qcKey']] = self.qcflag.astype('int32')

        DimDict['Location'] = len(adt)
        

def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads SWOT L3 SSH data, estimates the ADT '
            'and converts into IODA formatted output files.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of SWOT L3 SSH observation netCDF input file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    #parser.add_argument(
    #    '--thin', action="store_true",
    #    help="Data will not be super-obbed (thinned) unless --thin is present")
    # UPDATED: Changed to --no-thin and action="store_false"
    parser.add_argument(
        '--no-thin', dest='thin', action="store_false",
        help="Disable super-obbing (thinning) of the data. Thinning is enabled by default.")

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-nx', '--thinx',
        help="Thinning factor (i.e. jump between obs used) in x-direction for"
        " super-obbing (default: %(default)s).",
        type=int, default=3)
    optional.add_argument(
        '-ny', '--thiny',
        help="Thinning factor (i.e. jump between obs used) in y-direction for"
        " super-obbing (default: %(default)s).",
        type=int, default=3)
    optional.add_argument(
        '-oe', '--obserr',
        help="Observation error method. 'weighted_std' gives a Gaussian-weighted"
        " standard deviation of ADT over nearby points, 'parabolic' gives a"
        " cross-track parabola with no along-track variability"
        " (default: %(default)s).",
        type=str, default='weighted_std')
    
    args = parser.parse_args()

    # Read in the ADT data
    adt = swotk_l3adt2ioda(
        args.input, 
        superOb=args.thin, 
        nx_superob=args.thinx, 
        ny_superob=args.thiny, 
        obserr_method=args.obserr
    )

    # Set up the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # Write everything out
    writer.BuildIoda(adt.outdata, VarDims, adt.var_mdata, AttrData)

if __name__ == '__main__':
    main()

