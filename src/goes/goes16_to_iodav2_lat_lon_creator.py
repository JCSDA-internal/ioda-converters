#!/usr/bin/env python

import argparse
import math
import os
import sys
import numpy
from netCDF4 import Dataset


class Goes16ToIodav2LatLonCreator:

    def __init__(self, args):
        self._input_file_path = args.input_file_path
        self._output_file_path = args.output_file_path
        self._overwrite = args.overwrite
        self._check_arguments()
        self._create_datasets()
        self._create_groups()

    def _check_arguments(self):
        good_args = True
        if not os.path.exists(self._input_file_path):
            print("ERROR: Input GOES16 file does not exist: " + self._input_file_path)
            good_args = False
        if os.path.exists(self._output_file_path):
            if self._overwrite:
                print("WARNING: Overwriting IODAv2 lat lon file: " + self._output_file_path)
            else:
                print("ERROR: Specified IODAv2 lat lon file already exists: " + self._output_file_path)
                print("ERROR: Use -o or --overwrite option to overwrite.")
                good_args = False
        if not good_args:
            sys.exit(2)

    def _create_datasets(self):
        self._input_dataset = Dataset(self._input_file_path, 'r')
        self._output_dataset = Dataset(self._output_file_path, 'w', format='NETCDF4', engine='netcdf4')

    def _close_datasets(self):
        self._input_dataset.close()
        self._output_dataset.close()

    def _create_group(self, group):
        self._output_dataset.createGroup(group)

    def _create_groups(self):
        self._create_group('MetaData')

    def _create_netcdf_variable(self, name, dims, values):
        dtype = Goes16ToIodav2LatLonCreator._numpy_to_netcdf_dtype(values.dtype)
        self._output_dataset.createVariable(name, dtype, dims)
        self._output_dataset[name][:] = values

    def convert_goes16_to_iodav2_lat_lon(self):
        self._create_groups()
        self._input_dataset.set_auto_scale(True)

        earth_locs = ~numpy.ma.getmaskarray(self._input_dataset.variables['DQF'][:])

        abi_x = (self._input_dataset.variables['x'][:]).data
        abi_y = (self._input_dataset.variables['y'][:]).data

        grid_x, grid_y = numpy.meshgrid(abi_x, abi_y, indexing='xy')

        selected_x = grid_x[earth_locs]
        selected_y = grid_y[earth_locs]

        (lat, lon) = self._calc_lat_lon(selected_x, selected_y)

        n_locs_size = len(selected_x)
        self._output_dataset.createDimension('nlocs', n_locs_size)
        self._output_dataset.createVariable('nlocs', 'int32', ('nlocs',))
        self._output_dataset.variables['nlocs'].setncattr('suggested_chunk_dim', n_locs_size)
        self._output_dataset.variables['nlocs'][:] = numpy.arange(1, n_locs_size + 1, 1, dtype='int32')

        scan_angle = selected_x * 180.0 / numpy.pi
        elev_angle = selected_y * 180.0 / numpy.pi

        self._create_netcdf_variable("/MetaData/Scan_Angle", 'nlocs', scan_angle)
        self._create_netcdf_variable("/MetaData/Elevation_Angle", 'nlocs', elev_angle)
        self._create_netcdf_variable("/MetaData/latitude", 'nlocs', lat)
        self._create_netcdf_variable("/MetaData/longitude", 'nlocs', lon)

        variables = self._input_dataset.variables['geospatial_lat_lon_extent']
        nadir_lon = variables.geospatial_lon_nadir

        #Add nadir_lon attribute and others later on here

        self._close_datasets()

    def _calc_lat_lon(self, selected_x, selected_y):

        rad_to_deg = 180.0 / numpy.pi

        variables = self._input_dataset.variables['goes_imager_projection']

        r_eq = variables.semi_major_axis
        r_pol = variables.semi_minor_axis
        h = variables.perspective_point_height + variables.semi_major_axis
        lon_0 = variables.longitude_of_projection_origin

        h_sqr = numpy.power(h, 2.0)
        r_eq_sqr = numpy.power(r_eq, 2.0)
        r_pol_sqr = numpy.power(r_pol, 2.0)

        sin_x = numpy.sin(selected_x)
        cos_x = numpy.cos(selected_x)
        sin_y = numpy.sin(selected_y)
        cos_y = numpy.cos(selected_y)

        sin_x_sqr = numpy.power(sin_x, 2.0)
        cos_x_sqr = numpy.power(cos_x, 2.0)
        sin_y_sqr = numpy.power(sin_y, 2.0)
        cos_y_sqr = numpy.power(cos_y, 2.0)

        a = sin_x_sqr + (cos_x_sqr * (cos_y_sqr + (r_eq_sqr * sin_y_sqr / r_pol_sqr)))
        b = -2.0 * h * cos_x * cos_y
        c = h_sqr - r_eq_sqr

        arg = numpy.power(b, 2.0) - (4.0 * a * c)
        r_s = ((-1.0 * b) - numpy.sqrt(arg)) / (2.0 * a)

        s_x = r_s * cos_x * cos_y
        s_y = (-1.0 * r_s) * sin_x
        s_z = r_s * cos_x * sin_y

        h_minus_s_x = h - s_x
        lat = (numpy.arctan2((r_eq_sqr * s_z),
                             (r_pol_sqr * numpy.sqrt(h_minus_s_x * h_minus_s_x + s_y * s_y)))) * rad_to_deg
        lon = (lon_0 - numpy.arctan2(s_y, h_minus_s_x)) * rad_to_deg

        lat = numpy.nan_to_num(lat, nan=91.0)
        lon = numpy.nan_to_num(lon, nan=361.0)

        yaw_flip_flag = self._input_dataset.variables['yaw_flip_flag'][0]
        if not yaw_flip_flag:
            lat = lat[::-1]

        return lat, lon

    @staticmethod
    def _numpy_to_netcdf_dtype(numpy_dtype):
        numpy_to_netcdf_dtype = {numpy.dtype('float64'): 'f8',
                                 numpy.dtype('float32'): 'f4',
                                 numpy.dtype('int64'): 'i8',
                                 numpy.dtype('int32'): 'i4',
                                 numpy.dtype('S1'): 'c',
                                 numpy.dtype('object'): 'str'}
        return numpy_to_netcdf_dtype[numpy_dtype]


def parse_arguments():
    ap = argparse.ArgumentParser()
    ap.add_argument("input_file_path", type=str, help="Path to Input GOES16 File")
    ap.add_argument("output_file_path", type=str, help="Path to Output IODAv2 Lat Lon File")
    ap.add_argument("-o", "--overwrite", action="store_true", help="Allow Overwrite of Output IODAv2 Lat Lon File")
    return ap.parse_args()


if __name__ == '__main__':
    args = parse_arguments()
    goes16ToIodav2LatLonCreator = Goes16ToIodav2LatLonCreator(args)
    goes16ToIodav2LatLonCreator.convert_goes16_to_iodav2_lat_lon()
