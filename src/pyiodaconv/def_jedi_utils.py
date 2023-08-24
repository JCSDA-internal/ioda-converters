#!/usr/bin/env python3

#
# (C) Copyright 2019-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import numpy as np
import pyiodaconv.ioda_conv_engines as iconv

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

iso8601_string = "seconds since 1970-01-01T00:00:00Z"
epoch = datetime.fromisoformat(iso8601_string[14:-1])
ioda_float_type = 'float32'
ioda_int_type = 'int32'
float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)


def concat_obs_dict(obs_data, append_obs_data):
    # For now we are assuming that the obs_data dictionary has the "golden" list
    # of variables. If one is missing from append_obs_data, the obs_data variable
    # will be extended using fill values.
    #
    # Use the first key in the append_obs_data dictionary to determine how
    # long to make the fill value vector.
    append_keys = list(append_obs_data.keys())
    append_length = len(append_obs_data[append_keys[0]])
    for gv_key in obs_data.keys():
        if gv_key in append_keys:
            obs_data[gv_key] = np.append(obs_data[gv_key], append_obs_data[gv_key])
        else:
            if obs_data[gv_key].dtype == float:
                fill_data = np.repeat(float_missing_value, append_length, dtype=ioda_float_type)
            elif obs_data[gv_key].dtype == int:
                fill_data = np.repeat(int_missing_value, append_length, dtype=ioda_int_type)
            elif obs_data[gv_key].dtype == object:
                # string type, extend with empty strings
                fill_data = np.repeat("", append_length, dtype=object)
            obs_data[gv_key] = np.append(obs_data[gv_key], fill_data)


def set_metadata_attributes(VarAttrs):
    VarAttrs[('sensorZenithAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('sensorViewAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('solarZenithAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('sensorAzimuthAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('solarAzimuthAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
    VarAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value

    return VarAttrs


def set_obspace_attributes(VarAttrs):
    VarAttrs[('brightnessTemperature', obsValName)]['units'] = 'K'
    VarAttrs[('brightnessTemperature', obsErrName)]['units'] = 'K'

    VarAttrs[('brightnessTemperature', obsValName)]['_FillValue'] = float_missing_value
    VarAttrs[('brightnessTemperature', obsErrName)]['_FillValue'] = float_missing_value
    VarAttrs[('brightnessTemperature', qcName)]['_FillValue'] = int_missing_value

    return VarAttrs


def compute_scan_angle(instr_scan_ang, sensor_altitude, sensor_zenith, qc_flag=[None]):

    # should come from standard table
    earth_mean_radius_km = 6378.1370  # WGS84

    # example values sensor_altitude
    # iss_altitude_km = 408.
    # tropics_altitude_km = 550.
    # sensor_altitude_km = tropics_altitude_km

    d2r = np.pi/180.
    r2d = 180./np.pi

    # do we need a missing here
    ratio = np.empty_like(sensor_altitude)

    # compute scan angle
    if not qc_flag[0]:
        qc_flag = np.zeros_like(sensor_altitude)
    good = qc_flag[:] == 0
    if sum(good) > 0:
        ratio[good] = earth_mean_radius_km/(earth_mean_radius_km + sensor_altitude[good]/1000.)

    # γ = arcsin(R / (R + h) * sin(theta)),h: sat alt; theta: sat zenith angle
    scanang = np.arcsin(ratio*np.sin(abs(sensor_zenith)*d2r))*r2d

    return scanang
