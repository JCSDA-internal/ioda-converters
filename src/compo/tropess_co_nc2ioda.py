#!/usr/bin/env python3

#
# (C) Copyright 2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import os
from pathlib import Path

import xarray as xr
import math
from numpy import log as ln

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

# constants
HPA2PA = 1E2


class tropess(object):

    def __init__(self, filenames, userLevels, thin):

        self.filenames = filenames
        self.userLevels = userLevels
        self.thin = thin
        self.make_dictionaries()      # Set up variable names for IODA
        self.DimDict = {}
        self.read()    # Read data from file

    def read(self):

        # Loop through input filenames
        first = True
        for filename in self.filenames:

            try:
                dsCris = xr.open_dataset(filename)
            except IOError:
                raise IOError('%s file not found!' % self.filename)
            except Exception:
                raise Exception('Unknown error opening %s' % self.filename)

            # Read global attributes
            self.AttrData['sensor'] = dsCris.attrs['Instrument']
            self.AttrData['platform'] = dsCris.attrs['Platform']

            # Read lat lon
            lats = dsCris['latitude'].values
            lons = dsCris['longitude'].values

            # Read time and convert to ioda time format
            # note that datetime_utc variable in the intput is buggy
            # and has +10sec offset when compared time_tai93.
            times = dsCris['time'].dt.strftime("%Y-%m-%dT%H:%M:%SZ").values

            # Read pressure
            pressure = dsCris['pressure'].values  # hPa (nlocsxnlevs)
            # Nominal pressures is not part of the metadata but put in the userdoc..
            nom_pressure = [1040., 908., 681., 510., 383., 287., 215., 161., 121., 90.8, 51.0, 28.7, 4.64, 0.10]
            nlocs = pressure.shape[0]
            nlevs = pressure.shape[1]

            # add p=0 to the top of the atmopshere
            topPressure = np.zeros((nlocs, 1))
            fullPressure = np.concatenate((pressure, topPressure), axis=1)

            # Read other variables
            x = dsCris['x'].values  # dry_atmosphere_volume_mixing_ratio_of_carbon_monoxide (nlocsxnlevs)

            # open observation_ops group
            dsCris_observation_ops = xr.open_dataset(filename, group='observation_ops')

            # A priori state, as volume mixing ratio (VMR) relative to dry air , unit = 1
            xa = dsCris_observation_ops['xa'].values  # (nlocsxnlevs)

            # logarithmic_averaging_kernel
            averaging_kernel = dsCris_observation_ops['averaging_kernel'].values  # (nlocsxnlevsxnlevs)

            # logarithmic_observation_error
            log_obs_error = dsCris_observation_ops['observation_error'].values  # (nlocsxnlevsxnlevs)

            # The estimated state for target 0 based on x[0], xa[0],
            # and avg_kernel[0], as volume mixing ratio (VMR) relative to dry air
            x_test = dsCris_observation_ops['x_test'].values

            # Empty array for QA and missing flags
            # qa is not really used for now but can use dofs or other
            # quantities in the retrieval product. No qa/qc provided
            # in tropess cris product
            qa = np.zeros((nlocs), dtype=np.float32)
            nan_flag = np.full((nlocs), True)

            # Create a list of integers from a list of strings.
            userLevels = [int(lev) for lev in self.userLevels]

            # Keep track of the nominal levels
            nom = np.zeros((nlocs, len(userLevels)))

            # Calculate apriori term (ap) for each userLevel
            # do thinning and remove NaNs
            ap = np.zeros((nlocs, len(userLevels)), dtype=np.float32)
            flag = np.full((nlocs), True)
            thin = np.random.uniform(size=nlocs) > self.thin

            for lev, userLevel in enumerate(userLevels):
                ak = averaging_kernel[:, int(userLevel), :]
                this_term = ak * ln(xa)
                ap[:, lev] = ln(xa[:, lev]) - np.sum(this_term, axis=1)
                nom[:, lev] = nom_pressure[int(userLevel)]

                if(np.isnan(ap[:, lev]).any()):
                    nan_flag[np.argwhere(np.isnan(ap[:, lev]))] = False
                    flag = np.logical_and(nan_flag, flag)
            flag = np.logical_and(nan_flag, thin)

            # To keep the ufo part sane here we decide to add selected retrieval levels
            # sequentially. Implying R diagonal... There is no point of passing
            # all levels ot the DA as the info is very redundant (see DOFS for
            # each profile). Obs pre-proc could be done in the future using PCA/SVD
            # or other data compression techniques as seen in literature
            ulevs = len(userLevels)
            ap = ap.reshape(ulevs * nlocs)
            ap = ap.astype('float32')
            nom = nom.reshape(ulevs * nlocs)
            averaging_kernel = averaging_kernel[:, userLevels, :].reshape(ulevs * nlocs, nlevs)
            x = x[:, userLevels].reshape(ulevs * nlocs)
            xa = xa[:, userLevels].reshape(ulevs * nlocs)
            # Omit the off-diag terms in R for now...
            log_obs_error = log_obs_error[:, userLevels, userLevels].reshape(ulevs * nlocs)
            # TEMP FIX: Space of this error covariacne is mysterious, documentations doesn't
            # explain in which space it is... only says that error is about 7% of retrieval
            log_obs_error = 0.07 * x

            # Repeat ulevs times the coordinates
            flag = flag.repeat(ulevs)
            times = times.repeat(ulevs)
            lats = lats.repeat(ulevs)
            lons = lons.repeat(ulevs)
            fullPressure = fullPressure.repeat(ulevs, axis=0)
            qa = qa.repeat(ulevs)

            # ---- Write Metadata and data
            if first:

                # add metadata variables
                self.outData[('dateTime', 'MetaData')] = times[flag]
                self.outData[('latitude', 'MetaData')] = lats[flag]
                self.outData[('longitude', 'MetaData')] = lons[flag]

                # Write ap and xa
                varname_ap = ('apriori_term', 'RtrvlAncData')
                self.outData[varname_ap] = ap[flag]
                self.varAttrs[varname_ap]['coordinates'] = 'longitude latitude'
                self.varAttrs[varname_ap]['units'] = 'mol/mol'

                varname_np = ('nominalPressure', 'RtrvlAncData')
                self.outData[varname_np] = HPA2PA * nom[flag]
                self.varAttrs[varname_np]['coordinates'] = 'longitude latitude'
                self.varAttrs[varname_np]['units'] = 'Pa'

                # Write pressure and ak level by level
                for j in range(nlevs+1):
                    if j < nlevs:
                        varname_ak = ('averaging_kernel_level_'+str(j), 'RtrvlAncData')
                        self.outData[varname_ak] = averaging_kernel[:, j][flag]
                        self.varAttrs[varname_ak]['coordinates'] = 'longitude latitude'
                        self.varAttrs[varname_ak]['units'] = '1'

                    varname_pr = ('pressure_level_'+str(j), 'RtrvlAncData')
                    self.outData[varname_pr] = HPA2PA * fullPressure[:, j][flag]
                    self.varAttrs[varname_pr]['coordinates'] = 'longitude latitude'
                    self.varAttrs[varname_pr]['units'] = 'Pa'

                for var in self.obsvars:
                    self.outData[self.varDict[var]['valKey']] = \
                        x[flag]
                    self.outData[self.varDict[var]['errKey']] = \
                        log_obs_error[flag]
                    self.outData[self.varDict[var]['qcKey']] = \
                        qa[flag]

            # If not the first file concatenate
            else:
                self.outData[('dateTime', 'MetaData')] = np.concatenate(
                    (self.outData[('dateTime', 'MetaData')], times[flag]))
                self.outData[('latitude', 'MetaData')] = np.concatenate(
                    (self.outData[('latitude', 'MetaData')], lats[flag]))
                self.outData[('longitude', 'MetaData')] = np.concatenate(
                    (self.outData[('longitude', 'MetaData')], lons[flag]))

                varname_ap = ('apriori_term', 'RtrvlAncData')
                self.outData[varname_ap] = np.concatenate(
                    (self.outData[varname_ap], ap[flag]))

                varname_np = ('nominalPressure', 'RtrvlAncData')
                self.outData[varname_np] = np.concatenate(
                    (self.outData[varname_np], HPA2PA * nom[flag]))

                for j in range(nlevs+1):
                    if j < nlevs:
                        varname_ak = ('averaging_kernel_level_'+str(i), 'RtrvlAncData')
                        self.outData[varname_ak] = np.concatenate(
                            (self.outData[varname_ak], averaging_kernel[:, j][flag]))

                    varname_pr = ('pressure_level_'+str(i), 'RtrvlAncData')
                    self.outData[varname_pr] = np.concatenate(
                        (self.outData[varname_pr], HPA2PA * pressure[:, j][flag]))

                for var in self.obsvars:
                    self.outData[self.varDict[var]['valKey']] = np.concatenate(
                        (self.outData[self.varDict[var]['valKey']],
                         x[flag]))

                    self.outData[self.varDict[var]['errKey']] = np.concatenate(
                        (self.outData[self.varDict[var]['errKey']],
                         log_obs_error[flag]))

                    self.outData[self.varDict[var]['qcKey']] = np.concatenate(
                        (self.outData[self.varDict[var]['qcKey']],
                         qa[flag]))
                first = False

            self.DimDict['Location'] = len(self.outData[('dateTime', 'MetaData')])
            self.AttrData['Location'] = np.int32(self.DimDict['Location'])

    def make_dictionaries(self):
        """
        Make all the necessary dictionaries for this class object.
        """

        self.outData = defaultdict(lambda: DefaultOrderedDict(OrderedDict))

        self.make_obsVars()
        self.make_AttrData()
        self.make_varDict()
        self.make_varAttrs()

    def make_obsVars(self):
        """
        Make a dictionary of obsvars based on the levels specified by the user (command line args).
        """
        obsvars = {"carbonmonoxideColumn"}

        self.obsvars = obsvars

    def make_AttrData(self):
        """
        Make a dictionary of AttrData based on obsvars that is created using
        the levels specified by the user (command line args).
        """
        AttrData = {
            'converter': os.path.basename(__file__),
            'nvars': np.int32(len(self.obsvars))
        }
        self.AttrData = AttrData

    def make_varDict(self):
        """
        """
        self.varDict = defaultdict(lambda: defaultdict(dict))
        for item in self.obsvars:
            self.varDict[item]['valKey'] = item, iconv.OvalName()
            self.varDict[item]['errKey'] = item, iconv.OerrName()
            self.varDict[item]['qcKey'] = item, iconv.OqcName()

    def make_varAttrs(self):
        """
        """
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for item in self.obsvars:
            self.varAttrs[item, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[item, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[item, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[item, iconv.OvalName()]['units'] = 'mol/mol'
            self.varAttrs[item, iconv.OerrName()]['units'] = 'mol/mol'
            self.varAttrs[item, iconv.OqcName()]['units'] = 'unitless'

    def __str__(self):
        """
        Converts ingredients of the BaseCase to string for printing.
        """
        return "{}\n{}".format(
            str(self.__class__),
            "\n".join(
                (
                    "{} = {}".format(str(key), str(self.__dict__[key]))
                    for key in sorted(self.__dict__)
                )
            ),
        )


def get_parser():
    """
    Get the parser object for this script.
    Returns:
        parser (ArgumentParser): ArgumentParser which includes all the parser information.
    """

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads TROPESS CO netCDF files provided by NASA ??'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.'),
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.print_usage = parser.print_help

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of TROPESS L2 CO observation netCDF input file(s)",
        type=str,
        nargs='+',
        required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str,
        required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        "--levs", '--levels',
        help="User levels. [default: %(default)s] ",
        dest="userLevels",
        required=False,
        type=int,
        nargs='+',
        default=[0, 4])

    optional.add_argument(
        '-n', '--thin',
        help="percentage of random thinning from 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    return parser


def main():
    locationKeyList = [
        ("latitude", "float"),
        ("longitude", "float"),
        ("dateTime", "string")
    ]

    VarDims = {
        'x': ['Location']
    }
    # -- read command line arguments
    parser = get_parser()
    args = parser.parse_args()

    # Read in the CO data
    co = tropess(args.input, args.userLevels, args.thin)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, co.DimDict)

    # write everything out
    writer.BuildIoda(co.outData, VarDims, co.varAttrs, co.AttrData)


if __name__ == '__main__':
    main()
