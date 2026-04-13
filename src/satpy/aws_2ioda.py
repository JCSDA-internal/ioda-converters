
#!/usr/bin/env python3

#
# (C) Copyright 2020-2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# author: Benjamin Ruston
# This script will work with native EUMETSAT MeteoSat SEVIRI Native Level 1B files
#

from datetime import datetime, timedelta
import numpy as np
import pyproj
from pyproj import CRS
import pyresample
from pyresample.kd_tree import resample_nearest
import re
from satpy.scene import Scene
from satpy.readers import seviri_l1b_native

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import (
    compute_scan_angle,
    concat_obs_dict,
    epoch,
    ioda_float_type,
    ioda_int_type,
    set_metadata_attributes,
    set_obspace_attributes,
)

# globals
Meteosat08_WMO_sat_ID = 55
Meteosat09_WMO_sat_ID = 56
Meteosat10_WMO_sat_ID = 57
Meteosat11_WMO_sat_ID = 70
Meteosat12_WMO_sat_ID = 71
Meteosat13_WMO_sat_ID = 72
Meteosat16_WMO_sat_ID = 75

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

