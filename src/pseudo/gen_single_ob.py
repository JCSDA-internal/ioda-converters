#!/usr/bin/env python3
#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import numpy as np
from pathlib import Path
from datetime import datetime
import yaml
import sys

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import meteo_utils
import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict


class singleob(object):
    loctype = {
        float: 'float',
        str: 'string',
        int: 'integer',
    }

    def __init__(self, yamlfile):
        # read a YAML file and generate a single observation file
        # using the configuration in the YAML file
        # read the YAML config into a dictionary
        with open(yamlfile, 'r') as stream:
            yamlconfig = yaml.safe_load(stream)
        self.filename = yamlconfig['obsdataout']
        self.locKeyList = []
        self.AttrData = {
            'date_time_string': yamlconfig['lockeys']['datetime']
        }
        locKeys = []
        # set up location keys based on YAML
        for key in yamlconfig['lockeys'].keys():
            value = yamlconfig['lockeys'][key]
            self.locKeyList.append((key, self.loctype[type(value)]))
            locKeys.append(value)
        self.keyDict = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        self.writer = iconv.NcWriter(self.filename, self.locKeyList)

        # set up variable name
        key = 'oneob'
        value = yamlconfig['variable']['name']
        self.keyDict[key]['valKey'] = value, self.writer.OvalName()
        self.keyDict[key]['errKey'] = value, self.writer.OerrName()
        self.keyDict[key]['qcKey'] = value, self.writer.OqcName()
        # set up the data
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        locKey = tuple(locKeys)
        self.data[0][locKey][self.keyDict[key]['valKey']] = yamlconfig['variable']['obsvalue']
        self.data[0][locKey][self.keyDict[key]['errKey']] = yamlconfig['variable']['obserr']
        self.data[0][locKey][self.keyDict[key]['qcKey']] = yamlconfig['variable']['preqc']

        # call the IODA API and write the file
        (ObsVars, LocMdata, VarMdata) = self.writer.ExtractObsData(self.data)
        self.writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, self.AttrData)
        return


def main():

    desc = 'Generate single observation IODA file from YAML input'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-y', '--yaml', help='path to input YAML file', type=str, required=True)

    args = parser.parse_args()

    yamlfile = args.yaml

    singleob(yamlfile)


if __name__ == '__main__':
    main()
