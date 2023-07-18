#!/usr/bin/env python3

'''
 This script converts Multi-radar, multi-sensor (MRMS) radar reflectivity found
 on the NOAA AWS S3 bucket
 https://noaa-mrms-pds.s3.amazonaws.com/index.html#CONUS/MergedReflectivityQC_01.50
 which are GRIB2 files into IODA's netCDF format.
'''

import sys 
import os
from datetime import datetime, timedelta
import logging

import pygrib
import numpy as np
import netCDF4 as nc

# These modules need the path to lib-python modules
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
import pyiodaconv.ioda_conv_engines as iconv

os.environ["TZ"] = "UTC"

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("height", "float", "m"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
]
meta_keys = [m_item[0] for m_item in locationKeyList]

# Add to the list below if other variables are desired following mrms_keys above
mrms_products = {
    '97-209-9-0': 'reflectivity',
}
obsvars_units = ['dBZ']
obserrlist = [3.5]

# In the product list above, the four groups of digits represent the following GRIB keys
grib_keys = ['generatingProcessIdentifier', 'discipline', 'parameterCategory', 'parameterNumber']

AttrData = {
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    'description': 'Multi-radar, multi-sensor (MRMS) radar reflectivity',
    'source': 'NOAA',
    'sourceFiles': ''
}

DimDict = {
}

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = -999.0    # or netCDF value,  nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value,
                'double': double_missing_value}

dtypes = {'string': object,
          'integer': np.int32,
          'long': np.int64,
          'float': np.float32,
          'double': np.float64}


def main(file_names, output_file):

    # Initialize
    varDict = defaultdict(lambda: DefaultOrderedDict(dict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    obs_data = {}          # The final outputs.
    data = {}              # Before assigning the output types into the above.

    obsvars = []
    for key in mrms_products.keys():
        vname =  mrms_products[key]
        obsvars.append(vname)

    for key in meta_keys:
        data[key] = []

    for key in obsvars:
        data[key] = []

    # Loop through input files, reading data.
    nlocs = 0
    for fname in file_names:
        AttrData['sourceFiles'] += ", " + fname

        dt, heights, lat, lon, vars_mrms = read_grib(fname, obsvars)

        time_offset = round((dt - epoch).total_seconds())

        for height in heights:
            nobs = len(lat)
            nlocs = nlocs + nobs
            logging.info(f" adding {nobs} data locations for total of {nlocs}")
            x = np.full(nobs, time_offset)
            data['dateTime'].extend(x.tolist())
            x = np.full(nobs, height)
            data['height'].extend(x.tolist())
            data['latitude'].extend(lat)
            data['longitude'].extend(lon)

            for key in vars_mrms.keys():
                data[key].extend(vars_mrms[key])

        vars_mrms.clear()

    AttrData['sourceFiles'] = AttrData['sourceFiles'][2:]
    logging.debug("All source files: " + AttrData['sourceFiles'])

    DimDict = {'Location': nlocs}

    # Set coordinates and units of the ObsValues.
    for n, iodavar in enumerate(obsvars):
        varDict[iodavar]['valKey'] = iodavar, obsValName
        varDict[iodavar]['errKey'] = iodavar, obsErrName
        varDict[iodavar]['qcKey'] = iodavar, qcName
        varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, qcName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsValName]['units'] = obsvars_units[n]
        varAttrs[iodavar, obsErrName]['units'] = obsvars_units[n]

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        dtypestr = locationKeyList[meta_keys.index(key)][1]
        if locationKeyList[meta_keys.index(key)][2]:
            varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
        obs_data[(key, metaDataName)] = np.array(data[key], dtype=dtypes[dtypestr])

    # Transfer from the 1-D data vectors and ensure output data (obs_data) types using numpy.
    for n, iodavar in enumerate(obsvars):
        obs_data[(iodavar, obsValName)] = np.array(data[iodavar], dtype=np.float32)
        obs_data[(iodavar, obsErrName)] = np.full(nlocs, obserrlist[n], dtype=np.float32)
        obs_data[(iodavar, qcName)] = np.full(nlocs, 2, dtype=np.int32)
        varAttrs[(iodavar, obsValName)]['_FillValue'] = float_missing_value

    VarDims = {}
    for vname in obsvars:
        VarDims[vname] = ['Location']

    logging.debug(f"Writing output file: {output_file}")

    # setup the IODA writer
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(obs_data, VarDims, varAttrs, AttrData)


def read_grib(input_file, obsvars):
    logging.debug(f"Reading file: {input_file}")
    grbs = pygrib.open(input_file)
    grbs.seek(0)

    dt = None
    heights = []
    mrms_data = {}
    for obsvar in obsvars:
        mrms_data[obsvar] = []

    for grb in grbs:
        product_id = ''
        for key in grib_keys:
            product_id = product_id + str(grb[key]) + '-'
        product_id = product_id[:-1]

        if product_id in mrms_products.keys():
            obsvar = mrms_products[product_id]
            dt = grb.validDate
            height = grb['level']
            heights.append(height)
            lats, lons = grb.latlons()
            nj = lats.shape[0]
            ni = lats.shape[1]
            logging.debug(f"  capturing data for variable {obsvar} [{product_id}]")
            Z = grb.values
            Z = Z.reshape(ni*nj)
            if obsvar == 'reflectivity':
                Z[Z<-25.0] = float_missing_value
                Z[Z>80.0] = float_missing_value
            Z = Z.tolist()
            mrms_data[obsvar].extend(Z)
            Z.clear()
        else:
            pass

    grbs.close()

    if dt is None:
        print("No GRIB messages match the requested product(s) in variable mrms_products")
        sys.exit()

    lats = lats.reshape(ni*nj)
    lats = lats.tolist()
    lons = lons.reshape(ni*nj)
    lons[lons>180.0] = lons - 360.0
    lons = lons.tolist()

    '''
    # Remove the missing value points entirely from the dataset.
    # This is FLAWED because there could be different missing values on different
    # height levels, or potentially different missing values depending on product.
    '''

    if len(heights) == 1:
        for obsvar in mrms_data.keys():
            mrms_data[obsvar][:] = [item for item in mrms_data[obsvar] if item != float_missing_value]
            lats[:] = [item for item in mrms_data[obsvar] if item != float_missing_value]
            lons[:] = [item for item in mrms_data[obsvar] if item != float_missing_value]

    return dt, heights, lats, lons, mrms_data


if __name__ == "__main__":

    import argparse

    parser = argparse.ArgumentParser(
        description=(
            'Read GRIB2 formatted MRMS file and convert into IODA output file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input-files', nargs='+', dest='file_names',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output-file', dest='output_file',
                          action='store', default=None, required=True,
                          help='output file')

    parser.set_defaults(debug=False)
    parser.set_defaults(verbose=False)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action='store_true',
                          help='enable debug messages')
    optional.add_argument('--verbose', action='store_true',
                          help='enable verbose debug messages')

    args = parser.parse_args()

    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)

    for file_name in args.file_names:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    main(args.file_names, args.output_file)
