#!/usr/bin/env python3

import sys
import os
import time
from datetime import datetime
import numpy as np
import logging

# These modules need the path to lib-python modules
import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

# these are the unique values in the raw input file
varDict = {'criticalFrequency': ['criticalFrequency', "float", 'm s-1'],
           'criticalFrequencyConfidence': ['criticalFrequencyConfidence', "float", 'fractional percent'],
           'height': ['height', "float", "m"],
           'electronDensity': ['electronDensity', "float", 'number / m^3'],
           'electronDensityConfidence': ['electronDensityConfidence', "float", 'number / m^3'],
           'ionosphericLayer': ['ionosphericLayer', "integer", 'IonosphericLayer']}              # F2   - this is index for layer (E=0; F1=1; F2=2)

# these are the MetaData common to each input
locationKeyList = [("latitude", "float", "degrees_north"),
                   ("longitude", "float", "degrees_east"),
                   ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
                   ("hmF2", "float", "height of the F2 peak electron density"),
                   ("foF2", "float", "critical plasma frequency of F2 region"),
                   ("foF2Uncertainty", "float", "uncertainty of the critical plasma frequency of F2 region"),
                   ("nmF2", "float", "peak electron density in F2 region"),
                   ("nmF2Uncertainty", "float", "uncertainty in the peak electron density in F2 region"),
                   ("stationIdentifier", "string", "")]

meta_keys = [m_item[0] for m_item in locationKeyList]

GlobalAttrs = {
    'converter': os.path.basename(__file__),
    'ioda_version': 3,
    'description': 'Ionosonde profiler',
}
#   'source': 'Unknown',  # is there network name?

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)
string_missing_value = '_'

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value}
dtypes = {'string': object,
          'integer': np.int32,
          'long': np.int64,
          'float': np.float32}


def main(args):

    file_names = args.input
    output_file = args.output
    ionospheric_model = args.model

    start_time = time.time()

    data = init_data_dict()
    any_data = False
    for fname in file_names:
        logging.info(f"Reading file:  {fname}")
        data, any_data = read_file(fname, data, any_data, model=ionospheric_model, qc_strict=args.qc_strict)
    if not any_data:
        logging.error("No data to write, stopping execution.")
        sys.exit()

    dtg = datetime.fromtimestamp(data['dateTime'][0])
    datetimeRef = dtg.isoformat() + "Z"

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs = {
        'sourceFiles': ", ".join(file_names),
        'datetimeReference': datetimeRef
    }

    nlocs = len(data['height'])
    logging.info(f" found a total of {nlocs} observations")
    DimDict = {'Location': nlocs}

    varDims = {}
    for key in varDict.keys():
        variable = varDict[key][0]
        varDims[variable] = ['Location']

    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        dtype = locationKeyList[meta_keys.index(key)][1]
        if locationKeyList[meta_keys.index(key)][2]:
            varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtype]
    for key in varDict.keys():
        if 'electronDensity' in key:
            continue
        dtype = varDict[key][1]
        units = varDict[key][2]
        if units:
            varAttrs[(key, metaDataName)]['units'] = units
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtype]

    # Set units and FillValue attributes for groups associated with observed variable.
    for key in ['electronDensity']:
        variable = varDict[key][0]
        dtype = varDict[key][1]
        units = varDict[key][2]
        varAttrs[(variable, obsValName)]['units'] = units
        varAttrs[(variable, obsErrName)]['units'] = units
        varAttrs[(variable, obsValName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, obsErrName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, qcName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, obsValName)]['_FillValue'] = missing_vals[dtype]
        varAttrs[(variable, obsErrName)]['_FillValue'] = missing_vals[dtype]
        varAttrs[(variable, qcName)]['_FillValue'] = int_missing_value

    # Fill the final IODA data:  MetaData then ObsValues, ObsErrors, and QC
    ioda_data = {}

    # repeat these MetaData by nlocs
    for key in meta_keys:
        dtype = locationKeyList[meta_keys.index(key)][1]
        ioda_data[(key, metaDataName)] = np.full(nlocs, data[key], dtype=dtypes[dtype])
    for key in varDict.keys():
        variable = varDict[key][0]
        dtype = varDict[key][1]
        if 'electronDensity' not in key:
            logging.info(f" the variable: {variable} will be placed into MetaData of ioda_data")
            # these MetaData are arrays nlocs long already
            ioda_data[(key, metaDataName)] = np.array(data[variable], dtype=dtypes[dtype])
        else:
            # this MetaData (electronDensityConfidence) is used as the ObsError
            if 'electronDensityConfidence' in key:
                continue
            variable = varDict[key][0]
            logging.info(f" the variable: {variable} will be placed into ObsValue of ioda_data")
            ioda_data[(variable, obsValName)] = np.array(data[variable], dtype=np.float32)
            ioda_data[(variable, obsErrName)] = np.array(data['electronDensityConfidence'], dtype=np.float32)
            qc_array_hack = apply_gross_quality_control(data, qc_strict=args.qc_strict)
            ioda_data[(variable, qcName)] = np.array(qc_array_hack, dtype=np.int32)  # how to interpret AQI ?

    logging.debug("Writing file: " + output_file)

    # setup the IODA writer and write everything out.
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)
    writer.BuildIoda(ioda_data, varDims, varAttrs, GlobalAttrs)

    logging.info("--- {:9.4g} total seconds ---".format(time.time() - start_time))


def read_file(file_name, data, any_data, qc_strict=True, model='ART'):

    local_data = init_data_dict()

    # Open the file
    with open(file_name, 'r') as file:
        # Create an iterator from the file object
        file_iterator = iter(file)

        local_data, header_read = get_header(file_iterator, local_data, model=model)
        logging.debug(f'header was read w/o error: {header_read}')

        while header_read and True:
            try:
                # Get the next line from the iterator
                line = next(file_iterator)
                local_data = populate_obsValue(line, local_data, model=model)
            except StopIteration:
                # If StopIteration is raised, break from the loop
                break

    # if the header was not sucessfully read do nothing
    if header_read:
        for key in set(varDict.keys()).union(meta_keys):
            # data[key] = np.concatenate(data[key], local_data[key]) # why not and gotta be much better way
            data[key] = np.append(data[key], local_data[key])

    any_data = any_data or header_read

    return data, any_data


def get_header(file_iterator, local_data, model='ART'):
    #####################################################
    # get header (4 lines with the first having data too)
    #####################################################
    # example MetaData contant per profile
    # First two lines of MetaData (example)
    #   hmf2    fof2    conf         nmf2         conf     lat     lon
    # 277.09    4.22    0.00   0.2213E+12   0.5119E+09   45.07  276.44

    # extended metaData in 2nd line (example)
    # AQI=3 AL945_2020275000730 Problem Flags.................. qualscan V2009.10

    # DB049_2020296024502 Problem Flags.Check  1 failed.  qualscan V2009.10

    # final metaData is appended at end of the first data row (example)
    # ART   POL   ART   POL   ART   POL Slope POLAN 1/2
    # 4.2   4.2 293.4 277.1  41.7  50.8 563.2        1

    header_read = False
    while True:
        try:
            line = next(file_iterator)  # comment line
            # read in large pile of MetaData
            line = next(file_iterator)
            try:
                hmf2, fof2, conf_fof2, nmf2, conf_nmf2, lat, lon, qual, launchID, _, _, software, version = line.split()
            except ValueError:
                break

            # get station ID?
            stationID, launchTime = launchID.split('_')
            _, qualityIndex = qual.split('=')

            # dateTime
            dtg = datetime.strptime(f"{launchTime}", '%Y%j%H%M%S')
            dateTime = np.int64(round((dtg - epoch).total_seconds()))

            line = next(file_iterator)  # comment line
            # read first line of data and remaining MetaData
            line = next(file_iterator)  # comment line
            try:
                height, freq, f_conf, density, density_conf, ARTScale, Layer, polanScale, polanLayer, \
                    ARTver, POLver, ARTparm1, POLparm1, ARTparm2, POLparm2, Slope, POLANL = line.split()

            except ValueError:
                break

            local_data['latitude'] = np.append(local_data['latitude'], lat)
            local_data['longitude'] = np.append(local_data['longitude'], lon)
            local_data['dateTime'] = np.append(local_data['dateTime'], dateTime)
            local_data['stationIdentifier'] = np.append(local_data['stationIdentifier'], stationID)
            local_data['hmF2'] = np.append(local_data['hmF2'], hmf2)
            local_data['foF2'] = np.append(local_data['foF2'], fof2)
            local_data['nmF2'] = np.append(local_data['nmF2'], nmf2)
            local_data['foF2Uncertainty'] = np.append(local_data['foF2Uncertainty'], conf_fof2)
            local_data['nmF2Uncertainty'] = np.append(local_data['nmF2Uncertainty'], conf_nmf2)
            # populate the first row of ObsValues
            local_data = populate_obsValue(line, local_data, model=model)

            header_read = True
        except StopIteration:
            # If StopIteration is raised, break from the loop
            break

    return local_data, header_read


def populate_obsValue(line, local_data, model='ART'):
    # get the electron density as a funtion of height/range and critical frequency
    # if can correctly parse all fields populate local_data else do nothing
    # ObsValue data row (example)
    # height    freq  f_conf      density density_conf  ARTIST   POLAN
    # 186.00  -99.00  -99.00  -0.9900E+02  -0.9900E+02  0.00 c   2.04 F

    try:
        height, freq, f_conf, density, density_conf, ARTScale, Layer, POLAN, polanLayer = line.split()

        local_data['height'] = np.append(local_data['height'], height)
        local_data['criticalFrequency'] = np.append(local_data['criticalFrequency'], freq)
        local_data['criticalFrequencyConfidence'] = np.append(local_data['criticalFrequencyConfidence'], f_conf)
        local_data['electronDensity'] = np.append(local_data['electronDensity'], density)
        local_data['electronDensityConfidence'] = np.append(local_data['electronDensityConfidence'], density_conf)
#       # the ionospheric layer/region is dependent on the model selected
        ionosphericLayer = get_layer(Layer, model=model)
        local_data['ionosphericLayer'].append(ionosphericLayer)
    except ValueError:
        pass

    return local_data


def get_layer(Layer, model='ART'):
    # this routine should define a consistent naming for the ionospheric region
    # a table representation of the layers may be beneficial to avoid storing strings

    # depending on use of 'ART' or 'POL' set ionospheric layer/region
    ionosphericLayer = int_missing_value
    match model:
        case 'ART':
            # do not know what T means
            if 'F1' in Layer:
                ionosphericLayer = 1
            elif 'F2' in Layer:
                ionosphericLayer = 2
            elif 'E' in Layer:
                ionosphericLayer = 0
            elif 'T' in Layer:
                ionosphericLayer = 3
            elif 'V' in Layer:
                ionosphericLayer = 4
        case 'POL':
            # here assuming number in header is the F-Layer for POL
            # do not know if E layer is ever provided
            # do not know what T means
            if 'F' in Layer:
                ionosphericLayer = 2
            elif 'E' in Layer:
                ionosphericLayer = 0
            elif 'T' in Layer:
                ionosphericLayer = 3
            elif 'V' in Layer:
                ionosphericLayer = 4

    return ionosphericLayer


def apply_gross_quality_control(data, qc_strict=False):
    # if strict quality-control is requested
    # apply using simple physical reality check on variables

    # initialize returned variable
    qc_array_hack = np.zeros_like(data['electronDensity'], dtype=np.int32)
    # is requested apply check
    if qc_strict:
        qc_array_hack = np.where(
            (data['electronDensity'].astype(float) < 0)
            | (data['height'].astype(float) < 0)
            | (data['criticalFrequency'].astype(float) < 0),
            1,
            0)
    return qc_array_hack


def init_data_dict():
    local_data = {}              # Before assigning the output types into the above.
    for key in set(varDict.keys()).union(meta_keys):
        local_data[key] = []
    return local_data


if __name__ == "__main__":

    from argparse import ArgumentParser

    parser = ArgumentParser(
        description=('Read a satwind AMV ascii/csv file from SSEC'
                     ' and convert into IODA output file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input', nargs='+',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output',
                          action='store', default=None, required=True,
                          help='output file')

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action='store_true', default=False,
                          help='enable debug messages')
    optional.add_argument('--ionospheric-model', default='ART',
                          choices=['ART', 'POL'], dest='model',
                          help='ionospheric model options: ART or POL')
    optional.add_argument('--quality-control', action='store_true',
                          default=False, dest='qc_strict',
                          help='add PreQC values')
    optional.add_argument('--verbose', action='store_true', default=False,
                          help='enable verbose debug messages')

    args = parser.parse_args()

    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)

    for file_name in args.input:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    main(args)
