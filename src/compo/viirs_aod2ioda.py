#!/usr/bin/env python3

#
# (C) Copyright 2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
from datetime import datetime, timezone
import netCDF4 as nc
import numpy as np
import os

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["TZ"] = "UTC"

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", iso8601_string),
    ("surfaceQualifier", "integer", ""),
]

obsvars = ["aerosolOpticalDepth"]
channels = [4]
# A dictionary of global attributes.  More filled in further down.
AttrData = {}
AttrData['ioda_object_type'] = 'AOD'

# A dictionary of variable dimensions.
DimDict = {}

# A dictionary of variable names and their dimensions.
VarDims = {
    'aerosolOpticalDepth': ['Location', 'Channel'],
    "surfaceQualifier": ['Location'],
}

# Get the group names we use the most.
metaDataName = iconv.MetaDataName()

varsKeyList = [('valKey', iconv.OvalName(), 'float', 'longitude latitude', '1'),
               ('errKey', iconv.OerrName(), 'float', 'longitude latitude', '1'),
               ('qcKey', iconv.OqcName(), 'integer', 'longitude latitude', None)]

float_missing_value = iconv.get_default_fill_val(np.float32)
double_missing_value = iconv.get_default_fill_val(np.float64)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)
string_missing_value = iconv.get_default_fill_val(np.str_)

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value,
                'double': double_missing_value}

# QC mapping array for NASA products (Dark Target and Deep Blue)
# Dark Target flags: 0 = Bad, 1 = Marginal, 2 = Good, 3 = Very Good
# Deep Blue flags: 0=no retrieval, 1=poor, 2=moderate, 3=good
qcmapping = {
    0: 3,
    1: 2,
    2: 1,
    3: 0,
}
nasa_flip_qc = np.array([qcmapping[k] for k in sorted(qcmapping)])


class AOD(object):
    def __init__(self, in_dict):
        self.filenames = in_dict['input']
        self.error_method = in_dict['error_method']
        self.thin = in_dict['thin']
        self.provider = in_dict['provider']
        self.retrieval_method = in_dict['retrieval_method']
        self.wbeg = np.datetime64(str(datetime.strptime(in_dict['date_range'][0], "%Y%m%d%H"))).astype(np.int64)
        self.wend = np.datetime64(str(datetime.strptime(in_dict['date_range'][1], "%Y%m%d%H"))).astype(np.int64)
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.setDicts()
        self.read()

        DimDict['Location'] = len(self.outdata[('latitude', metaDataName)])
        DimDict['Channel'] = np.array(channels)

    def setDicts(self):
        meta_keys = [m_item[0] for m_item in locationKeyList]
        # Set units of the MetaData variables and all _FillValues.
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            if locationKeyList[meta_keys.index(key)][2]:
                self.varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
            self.varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]

        var_keys = [v_item[0] for v_item in varsKeyList]
        # set up variable names for IODA
        for iodavar in obsvars:
            for key in var_keys:
                varGroupName = varsKeyList[var_keys.index(key)][1]
                dtypestr = varsKeyList[var_keys.index(key)][2]
                coord = varsKeyList[var_keys.index(key)][3]
                self.varDict[iodavar][key] = iodavar, varGroupName
                self.varAttrs[iodavar, varGroupName]['coordinates'] = coord
                self.varAttrs[iodavar, varGroupName]['_FillValue'] = missing_vals[dtypestr]
                if varsKeyList[var_keys.index(key)][4]:
                    self.varAttrs[iodavar, varGroupName]['units'] = varsKeyList[var_keys.index(key)][4]

    def get_platform_sensor_names(self):
        if self.provider == 'noaa':
            satellite = self.glb_attrs["satellite_name"]
            sensor = self.glb_attrs["instrument_name"]
        elif self.provider == 'nasa':
            satellite = self.glb_attrs["platform"]
            sensor = self.glb_attrs["instrument"]

        if 'NPP' in satellite:
            AttrData["platform"] = "suomi_npp"
            AttrData["sensor"] = "v.viirs-m_npp"
        elif satellite == 'NOAA-20':
            AttrData["platform"] = "noaa_20"
            AttrData["sensor"] = "v.viirs-m_j1"
        elif satellite == 'NOAA-21':
            AttrData["platform"] = "noaa_21"
            AttrData["sensor"] = "v.viirs-m_j2"

    def get_s_e_time(self):
        if self.provider == 'noaa':
            timeformat = '%Y-%m-%dT%H:%M:%SZ'
        elif self.provider == 'nasa':
            timeformat = '%Y-%m-%dT%H:%M:%S.000Z'

        # Special time consideration. Get min/max of all times being converted for output attribute data.
        this_starttime = datetime.strptime(self.glb_attrs["time_coverage_start"], timeformat)
        this_starttime = this_starttime.replace(tzinfo=timezone.utc)
        self.s_time = round((this_starttime - epoch).total_seconds())

        this_endtime = datetime.strptime(self.glb_attrs["time_coverage_end"], timeformat)
        this_endtime = this_endtime.replace(tzinfo=timezone.utc)
        self.e_time = round((this_endtime - epoch).total_seconds())

    def get_noaa_data(self):
        # For NOAA EPS
        self.lons = self.ncd.variables['Longitude'][:].ravel()
        self.lats = self.ncd.variables['Latitude'][:].ravel()
        self.vals = self.ncd.variables['AOD550'][:].ravel()
        self.errs = self.ncd.variables['Residual'][:].ravel()
        self.qcfs = self.ncd.variables['QCAll'][:].ravel().astype('int32')

        # Keep valid data points only
        valid_pts = ~self.vals.mask
        self.lons = self.lons[valid_pts]
        self.lats = self.lats[valid_pts]
        self.vals = self.vals[valid_pts]
        self.errs = self.errs[valid_pts]
        self.qcfs = self.qcfs[valid_pts]
        self.lsfs = np.zeros_like(self.lats, dtype=np.int32)
        if np.count_nonzero(valid_pts) == 0:
            return np.count_nonzero(valid_pts)

        # QCPath is the flag for retrieval path. The valid range is 0-127 in the
        # ATBD: https://www.star.nesdis.noaa.gov/jpss/documents/ATBD/ATBD_EPS_Aerosol_AOD_v3.4.pdf.
        # QCPath's valid range in the input file is not correct, so we define the valid range here.
        qcpath = self.ncd.variables['QCPath'][:].data.ravel()[valid_pts]
        qcpath = np.ma.masked_array(qcpath, np.logical_or(qcpath < 0, qcpath > 127))
        # bit 0: retrieval over water; bit 2: over glint water; other bits are over land
        water_pts = ((qcpath >> 0 & 1) == 1) | ((qcpath >> 2 & 1) == 1)
        self.lsfs[water_pts] = 0
        self.lsfs[~water_pts] = 1

        # Define pixel-level uncertainty estimates (PUE) based on surface type
        if self.error_method == "pue":
            AttrData['errorMethod'] = 'Pixel-level Uncertainty Estimates (PUE)'
            self.errs = 0.111431 + 0.128699 * self.vals    # over land (dark)
            self.errs[qcpath % 2 == 1] = 0.00784394 + 0.219923 * self.vals[qcpath % 2 == 1]  # over ocean
            self.errs[qcpath % 4 == 2] = 0.0550472 + 0.299558 * self.vals[qcpath % 4 == 2]   # over bright land
        return np.count_nonzero(valid_pts)

    def get_nasa_dt_data(self):
        # For NASA Dark Target
        self.lons = self.ncd.groups['geolocation_data'].variables['longitude'][:].ravel()
        self.lats = self.ncd.groups['geolocation_data'].variables['latitude'][:].ravel()
        self.lsfs = self.ncd.groups['geophysical_data'].variables['Land_Sea_Flag'][:].ravel()
        self.vals = self.ncd.groups['geophysical_data'].variables['Optical_Depth_Land_And_Ocean'][:].ravel()
        self.qcfs = self.ncd.groups['geophysical_data'].variables['Land_Ocean_Quality_Flag'][:].ravel()

        # Based on Dark Target ATBD (March 2024), assign expected error (EE)
        # https://darktarget.gsfc.nasa.gov/sites/default/files/users/user9/ATBD_DarkTarget_April3.pdf
        AttrData['errorMethod'] = 'Expected Error (EE)'
        land_pts = self.lsfs == 1
        self.errs = np.where(land_pts, np.add(0.05, np.multiply(0.2, self.vals)),
                             np.add(0.05, np.multiply(0.15, self.vals)))

        # Keep valid data points only
        valid_pts = ~self.vals.mask
        self.lons = self.lons[valid_pts]
        self.lats = self.lats[valid_pts]
        self.lsfs = self.lsfs[valid_pts]
        self.vals = self.vals[valid_pts]
        self.errs = self.errs[valid_pts]

        # Flip QC flags for PreQC (0->3, 3->0)
        self.qcfs = nasa_flip_qc[self.qcfs[valid_pts].astype(np.int32)]
        return np.count_nonzero(valid_pts)

    def get_nasa_db_data(self):
        # For NASA Deep Blue
        self.lons = self.ncd.variables['Longitude'][:].ravel()
        self.lats = self.ncd.variables['Latitude'][:].ravel()
        # Only QC=3 pixels are retained with Aerosol_Optical_Thickness_550_Land_Ocean_Best_Estimate
        self.vals = self.ncd.variables['Aerosol_Optical_Thickness_550_Land_Ocean_Best_Estimate'][:].ravel()
        eu_land = self.ncd.variables['Aerosol_Optical_Thickness_550_Expected_Uncertainty_Land'][:].ravel()
        eu_ocean = self.ncd.variables['Aerosol_Optical_Thickness_550_Expected_Uncertainty_Ocean'][:].ravel()

        # Keep valid data points only
        nan_mask = (np.isnan(self.vals) | np.isnan(eu_land) | np.isnan(eu_ocean))
        valid_pts = (~self.vals.mask & (~(eu_land < 0) | ~(eu_ocean < 0))) & (~nan_mask)
        self.lons = self.lons[valid_pts]
        self.lats = self.lats[valid_pts]
        self.vals = self.vals[valid_pts]
        self.lsfs = np.zeros_like(self.lats, dtype=np.int32)
        if np.count_nonzero(valid_pts) == 0:
            return np.count_nonzero(valid_pts)

        npts_land = self.ncd.variables['Number_Of_Pixels_Used_Land'][:].ravel()
        npts_ocean = self.ncd.variables['Number_Of_Pixels_Used_Ocean'][:].ravel()
        land_pts = np.logical_and(npts_land[valid_pts] > 0, npts_ocean[valid_pts] == 0)
        ocean_pts = np.logical_and(npts_ocean[valid_pts] > 0, npts_land[valid_pts] == 0)
        mix_pts = np.logical_and(npts_land[valid_pts] > 0, npts_ocean[valid_pts] > 0)
        # Assign land sea flag: Ocean=0, Land=1, Mix(Coastal)=2
        self.lsfs[ocean_pts] = 0
        self.lsfs[land_pts] = 1
        self.lsfs[mix_pts] = 2

        mix_land_pts = np.logical_and(mix_pts, npts_land[valid_pts] > npts_ocean[valid_pts])
        mix_ocean_pts = np.logical_and(mix_pts, npts_land[valid_pts] < npts_ocean[valid_pts])
        mix_equal_pts = np.logical_and(mix_pts, npts_land[valid_pts] == npts_ocean[valid_pts])

        # VIIRS Deep Blue Pixel-level Uncertainty Estimates (PUE)
        # Lee et al. (2024): https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2023JD040082?af=R
        # PUE should fit for DA purpose better according to
        # Hsu et al. (2018): https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018JD029688
        eu_land = eu_land[valid_pts]
        eu_ocean = eu_ocean[valid_pts]

        self.errs = np.ones_like(eu_land)
        qaf_land = self.ncd.variables['Aerosol_Optical_Thickness_QA_Flag_Land'][:].ravel()[valid_pts]
        qaf_ocean = self.ncd.variables['Aerosol_Optical_Thickness_QA_Flag_Ocean'][:].ravel()[valid_pts]
        self.qcfs = np.ones_like(qaf_land)

        if np.count_nonzero(land_pts) > 0:
            self.errs[land_pts] = eu_land[land_pts]
            self.qcfs[land_pts] = qaf_land[land_pts]
        if np.count_nonzero(ocean_pts) > 0:
            self.errs[ocean_pts] = eu_ocean[ocean_pts]
            self.qcfs[ocean_pts] = qaf_ocean[ocean_pts]
        if np.count_nonzero(mix_land_pts) > 0:
            self.errs[mix_land_pts] = eu_land[mix_land_pts]
            self.qcfs[mix_land_pts] = qaf_land[mix_land_pts]
        if np.count_nonzero(mix_ocean_pts) > 0:
            self.errs[mix_ocean_pts] = eu_ocean[mix_ocean_pts]
            self.qcfs[mix_ocean_pts] = qaf_ocean[mix_ocean_pts]
        if np.count_nonzero(mix_equal_pts) > 0:
            self.errs[mix_equal_pts] = 0.5 * eu_land[mix_equal_pts] + 0.5 * eu_ocean[mix_equal_pts]
            self.qcfs[mix_equal_pts] = np.where(qaf_land[mix_equal_pts] < qaf_ocean[mix_equal_pts],
                                                qaf_land[mix_equal_pts], qaf_ocean[mix_equal_pts])
        # Flip QC flags for PreQC (0->3, 3->0)
        self.qcfs = nasa_flip_qc[self.qcfs.astype(np.int32)]

        AttrData['errorMethod'] = 'Pixel-level Uncertainty Estimates (PUE)'
        if self.error_method != "pue":
            # VIIRS DeepBlue Expected Error (https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018JD029688)
            self.errs = np.add(0.05, np.multiply(0.2, self.vals))
            AttrData['errorMethod'] = 'Expected Error (EE)'
        return np.count_nonzero(valid_pts)

    def read(self):
        # Make empty lists for the output vars
        self.outdata[('latitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('longitude', metaDataName)] = np.array([], dtype=np.float32)
        self.outdata[('dateTime', metaDataName)] = np.array([], dtype=np.int64)
        self.outdata[('surfaceQualifier', metaDataName)] = np.array([], dtype=np.int32)
        for iodavar in obsvars:
            self.outdata[self.varDict[iodavar]['valKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['errKey']] = np.array([], dtype=np.float32)
            self.outdata[self.varDict[iodavar]['qcKey']] = np.array([], dtype=np.int32)

        # Define get_data function based on provider and retrieval method (NASA only)
        if self.provider == 'nasa':
            if self.retrieval_method == 'DarkTarget':
                get_viirs_data = self.get_nasa_dt_data
            elif self.retrieval_method == 'DeepBlue':
                get_viirs_data = self.get_nasa_db_data
            AttrData['retrievalMethod'] = self.retrieval_method
        elif self.provider == 'noaa':
            get_viirs_data = self.get_noaa_data
            AttrData['retrievalMethod'] = 'EPS'

        min_time = -int_missing_value
        max_time = int_missing_value

        # loop through input filenamess
        for n, f in enumerate(self.filenames):
            self.ncd = nc.Dataset(f, 'r')
            self.glb_attrs = {attr: getattr(self.ncd, attr) for attr in self.ncd.ncattrs()}

            # Get the coverage start and end time
            self.get_s_e_time()
            min_time = min(self.s_time, min_time)
            max_time = max(self.e_time, max_time)

            # Get the platform and sensor name
            self.get_platform_sensor_names()

            # Get VIIRS data and skip the file if no valid data
            nvalid_pts = get_viirs_data()
            if nvalid_pts == 0:
                print(f"  No valid pixels, skip {f}")
                continue

            # assign the observation time based on time coverage
            self.obs_time = np.full(np.shape(self.lons), round(0.5*(self.s_time + self.e_time)), dtype=np.int64)

            # apply thinning mask
            if self.thin > 0.0:
                mask_thin = np.random.uniform(size=len(self.lons)) > self.thin
                self.lons = self.lons[mask_thin]
                self.lats = self.lats[mask_thin]
                self.lsfs = self.lsfs[mask_thin]
                self.vals = self.vals[mask_thin]
                self.errs = self.errs[mask_thin]
                self.qcfs = self.qcfs[mask_thin]
                self.obs_time = self.obs_time[mask_thin]

            # after the thinning above apply a mask based on time window
            winmsk = ((self.obs_time >= self.wbeg) & (self.obs_time <= self.wend))

            # Write out data
            self.outdata[('latitude', metaDataName)] = np.append(
                self.outdata[('latitude', metaDataName)], np.array(self.lats[winmsk], dtype=np.float32))
            self.outdata[('longitude', metaDataName)] = np.append(
                self.outdata[('longitude', metaDataName)], np.array(self.lons[winmsk], dtype=np.float32))
            self.outdata[('dateTime', metaDataName)] = np.append(
                self.outdata[('dateTime', metaDataName)], np.array(self.obs_time[winmsk], dtype=np.int64))
            self.outdata[('surfaceQualifier', metaDataName)] = np.append(
                self.outdata[('surfaceQualifier', metaDataName)], np.array(self.lsfs[winmsk], dtype=np.int32))
            for iodavar in obsvars:
                self.outdata[self.varDict[iodavar]['valKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['valKey']], np.array(self.vals[winmsk], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['errKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['errKey']], np.array(self.errs[winmsk], dtype=np.float32))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.append(
                    self.outdata[self.varDict[iodavar]['qcKey']], np.array(self.qcfs[winmsk], dtype=np.int32))

            self.ncd.close()

        AttrData['datetimeRange'] = np.array([datetime.fromtimestamp(min_time).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                              datetime.fromtimestamp(max_time).strftime("%Y-%m-%dT%H:%M:%SZ")], dtype=object)
        print(f"Processed data for datetimeRange: {AttrData['datetimeRange']}")


def main():

    # get command line arguments
    # Usage: python viirs_aod2ioda.py -i /path/to/obs/2021060801.nc /path/to/obs/2021060802.nc ... -o /path/to/ioda/20210608.nc
    # --provider [noaa/nasa] --retieval_method [DarkTarget/DeepBlue] --error_method [pue]
    # where the input obs could be for any desired interval to concatenated together.
    parser = argparse.ArgumentParser(
        description=('Read VIIRS aerosol optical depth file(s) and Converter'
                     ' of native NetCDF format for observations of optical'
                     ' depth from VIIRS AOD550 to IODA-V2 netCDF format.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of viirs aod input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="name of ioda-v2 output file",
        type=str, required=True)
    required.add_argument(
        '--provider',
        help="data source, noaa/nasa",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--retrieval_method',
        help="specify the retrieval method when provider is nasa, DarkTarget/DeepBlue",
        type=str, default=None)
    optional.add_argument(
        '--error_method',
        help="calculation error method: pue/default, default is none for NOAA, Expected Error for NASA product",
        type=str, default=None)
    optional.add_argument(
        '-n', '--thin',
        help="percentage of random thinning fro 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)
    optional.add_argument(
        '--date_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHH YYYYMMDDHH",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    args = parser.parse_args()

    args_in_dict = {
        'input': args.input,
        'error_method': args.error_method,
        'provider': args.provider,
        'retrieval_method': args.retrieval_method,
        'thin': args.thin,
        'date_range': args.date_range,
    }

    # setup the IODA writer

    # Read in the AOD data
    aod = AOD(args_in_dict)

    # write everything out

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(aod.outdata, VarDims, aod.varAttrs, AttrData)


if __name__ == '__main__':
    main()
