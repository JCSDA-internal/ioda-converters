#!/usr/bin/env python

import argparse
import os
import sys
import numpy
import datetime
import pytz
from netCDF4 import Dataset


class Goes16ToIodav2:

    def __init__(self, args):
        self._input_file_path = args.input_file_path
        self._latlon_file_path = args.latlon_file_path
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
                print("WARNING: Overwriting IODAv2 file: " + self._output_file_path)
            else:
                print("ERROR: Specified IODAv2 file already exists: " + self._output_file_path)
                print("ERROR: Use -o or --overwrite option to overwrite.")
                good_args = False
        if not good_args:
            sys.exit(2)

    def _create_datasets(self):
        self._input_dataset = Dataset(self._input_file_path, 'r')
        self._latlon_dataset = Dataset(self._latlon_file_path, 'r')
        self._output_dataset = Dataset(self._output_file_path, 'w', format='NETCDF4', engine='netcdf4')

    def _close_datasets(self):
        self._input_dataset.close()
        self._latlon_dataset.close()
        self._output_dataset.close()

    def _create_group(self, group):
        self._output_dataset.createGroup(group)

    def _create_groups(self):
        self._create_group('MetaData')
        self._create_group('ObsValue')
        self._create_group('ObsError')
        self._create_group('PreQC')

    def _create_netcdf_variable(self, name, dims, values):
        dtype = Goes16ToIodav2._numpy_to_netcdf_dtype(values.dtype)
        if dtype == 'c':
            all_dims = dims + ('nstring',)
        else:
            all_dims = dims
        self._output_dataset.createVariable(name, dtype, all_dims)
        self._output_dataset[name][:] = values

    def _import_latlon_data(self):
        nlocs_data_array = self._latlon_dataset['nlocs'][:].real
        nlocs = len(nlocs_data_array)

        self._output_dataset.createDimension('nlocs', nlocs)
        self._output_dataset.createVariable('nlocs', 'int32', ('nlocs',))
        self._output_dataset.variables['nlocs'].setncattr('suggested_chunk_dim', nlocs)
        self._output_dataset.variables['nlocs'][:] = nlocs_data_array

        latitude_data_array = self._latlon_dataset['MetaData'].variables['latitude'][:].real
        longitude_data_array = self._latlon_dataset['MetaData'].variables['longitude'][:].real
        elevation_angle_data_array = self._latlon_dataset['MetaData'].variables['Elevation_Angle'][:].real
        scan_angle_data_array = self._latlon_dataset['MetaData'].variables['Scan_Angle'][:].real

        self._output_dataset.createVariable('/MetaData/latitude', 'f4', 'nlocs')
        self._output_dataset.createVariable('/MetaData/longitude', 'f4', 'nlocs')
        self._output_dataset.createVariable('/MetaData/Elevation_Angle', 'f4', 'nlocs')
        self._output_dataset.createVariable('/MetaData/Scan_Angle', 'f4', 'nlocs')

        self._output_dataset['/MetaData/latitude'][:] = latitude_data_array
        self._output_dataset['/MetaData/longitude'][:] = longitude_data_array
        self._output_dataset['/MetaData/Elevation_Angle'][:] = elevation_angle_data_array
        self._output_dataset['/MetaData/Scan_Angle'][:] = scan_angle_data_array

    def convert_goes16_to_iodav2(self):
        self._create_groups()
        self._import_latlon_data()
        self._input_dataset.set_auto_scale(True)

        earth_locs = ~numpy.ma.getmaskarray(self._input_dataset.variables['DQF'][:])

        t_epoch = datetime.datetime(2000, 1, 1, 12, 0, 0, 0, pytz.UTC)
        t_start = t_epoch + datetime.timedelta(
            seconds=int(round(float(self._input_dataset.variables['time_bounds'][0]))))
        t_mid = t_epoch + datetime.timedelta(seconds=int(round(float(self._input_dataset.variables['t'][0]))))
        t_end = t_epoch + datetime.timedelta(seconds=int(round(float(self._input_dataset.variables['time_bounds'][1]))))
        t_refdate = t_mid.replace(minute=0, second=0, microsecond=0)

        if t_mid.minute >= 30:
            t_refdate = t_refdate + datetime.timedelta(hours=1)

        t_s = (t_start - t_refdate).total_seconds()
        t_e = (t_end - t_refdate).total_seconds()

        timesteps = numpy.linspace(t_s, t_e, num=earth_locs.size, dtype=int).reshape(earth_locs.shape)
        timesteps = timesteps[earth_locs]
        offset_time = timesteps.astype(float) / 3600.0

        nlocs = self._output_dataset.dimensions['nlocs'].size
        nvars = 1
        nrecs = 1
        nobs = nvars * nlocs

        self._output_dataset.createDimension('nvars', nvars)
        self._output_dataset.createDimension('nrecs', nrecs)
        self._output_dataset.createDimension('nobs', nobs)

        date_time = (t_refdate.year * 1000000 + t_refdate.month * 10000 + t_refdate.day * 100 + t_refdate.hour)
        self._output_dataset.setncattr("date_time", date_time)
        self._create_netcdf_variable("/MetaData/time", 'nlocs', offset_time)

        var = self._input_dataset.variables['Rad'][:][earth_locs]
        self._create_netcdf_variable("/ObsValue/radiance", 'nlocs', var)

        var = self._input_dataset.variables['DQF'][:].astype(int)[earth_locs]
        self._create_netcdf_variable("/PreQC/radiance", 'nlocs', var)

        err_stddev = self._input_dataset.variables['std_dev_radiance_value_of_valid_pixels'][0]
        var = numpy.full((nlocs,), err_stddev ** 2, dtype='f4')
        self._create_netcdf_variable("/ObsError/radiance", 'nlocs', var)
        self._close_datasets()

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
    ap.add_argument("latlon_file_path", type=str, help="Path to Input GOES16 Lat/Lon File")
    ap.add_argument("output_file_path", type=str, help="Path to Output IODAv2 File")
    ap.add_argument("-o", "--overwrite", action="store_true", help="Allow Overwrite of Output IODAv2 File")
    return ap.parse_args()


if __name__ == '__main__':
    args = parse_arguments()
    goes16ToIodav2 = Goes16ToIodav2(args)
    goes16ToIodav2.convert_goes16_to_iodav2()
