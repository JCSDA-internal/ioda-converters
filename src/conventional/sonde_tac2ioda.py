###########################################################################
# These functions decode WMO format soundings which contain at least the
# mandatory levels (TTAA, TTCC), and also can include significant temperature
# (TTBB, TTDD) and wind (PPBB, PPDD) sections. The sections can be merged into
# a single profile, which interpolates any missing values if possible.
# The code is based on Fortran code originally developed by Peter Neilley, NCAR/RAP.
###########################################################################

import re
import logging
import math
import os
import sys
import time
import json
from datetime import datetime, timedelta
import dateutil.parser
from pathlib import Path

import numpy as np
import netCDF4 as nc
from cartopy import geodesic
from copy import deepcopy as dcop

# set path to ioda_conv_engines module
IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

# These modules need the path to lib-python modules
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict
import ioda_conv_engines as iconv
import meteo_utils
import meteo_sounding_utils

os.environ["TZ"] = "UTC"

logger = logging.getLogger("decodeSounding")

# Abbreviations for pressure levels found in the WMO datasets
LEVEL_CODES = {
    '99': 0,
    '00': 1000.0,
    '92': 925.0,
    '85': 850.0,
    '70': 700.0,
    '50': 500.0,
    '40': 400.0,
    '30': 300.0,
    '25': 250.0,
    '20': 200.0,
    '15': 150.0,
    '10': 100.0,
    '07': 70.0
}

# List of sounding data sections
TYPES = ['TTAA', 'TTBB', 'PPBB', 'TTCC', 'TTDD', 'PPDD']

STATIONS = {}

# The outgoing IODA MetaData variables, their data type, and units.
MetaDataKeyList = [
    ("station_id", "string", ""),
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("station_elevation", "float", "m"),
    ("height", "float", "m"),
    ("air_pressure", "float", "Pa"),
    ("launch_time", "string", ""),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
]
meta_keys = [m_item[0] for m_item in MetaDataKeyList]

# The outgoing IODA variables (ObsValues), their units, and assigned constant ObsError.
obsvars = ['air_temperature',
           'specific_humidity',
           'virtual_temperature',
           'eastward_wind',
           'northward_wind']
obsvars_units = ['K', 'kg kg-1', 'K', 'm s-1', 'm s-1']
obserrlist = [1.2, 0.75E-3, 1.5, 1.7, 1.7]

VarDims = {
    'air_temperature': ['nlocs'],
    'specific_humidity': ['nlocs'],
    'virtual_temperature': ['nlocs'],
    'eastward_wind': ['nlocs'],
    'northward_wind': ['nlocs']
}

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

AttrData = {
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    'description': 'Radiosonde observations converted from text (TAC) format',
    'source': 'LDM at NCAR-RAL',
    'sourceFiles': ''
}

DimDict = {
}

float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'

iso8601_string = MetaDataKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

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

geod = geodesic.Geodesic()  # generate ellipsoid, defaults to Earth WGS84 parameters


def loadStations(stationfile, skipIfLoaded=True):
    """
    Load station info from a .json file into the global station list. This info is needed
    when parsing WMO data to determine surface elevation at each site
    :param stationfile: The filename to load
    :param skipIfLoaded: If True, don't load the station file if one has already been loaded
    :return: True on success, False on error
    """

    global STATIONS

    if skipIfLoaded and len(STATIONS) > 0:
        return True

    try:
        fh = open(stationfile, "r")
        data = fh.read()
        fh.close()
        STATIONS = json.loads(data)
        return True
    except Exception as e:
        logger.error("Could not read station info from json file '%s': %s" % (stationfile, e))
        return False


def getStationInfo(icaoId=None, synopId=None):
    """
    Get info for a station given either an icaoId or a synopId
    :param icaoId:
    :param synopId:
    :return: A dict containing station info, or None
    """
    if icaoId is None and synopId is None:
        return None

    if len(STATIONS) == 0:
        logger.warning("Station file not loaded")
        return None

    station = None
    if synopId:
        station = STATIONS[synopId] if synopId in STATIONS else None
    else:
        for synop in STATIONS:
            st = STATIONS[synop]
            if st['id'].lower() == icaoId.lower():
                station = st
                synopId = synop
                break

    if station:
        station['synop'] = synopId

    return station


def getProfile(filename, synopId, year, month):
    """
    Get a parsed profile for the given station from the given RAOBS file
    :param filename:
    :param synopId:
    :param year:
    :param month:
    :return: A parsed sounding dict, or None
    """
    sections = getSections(filename, [synopId])
    if not sections:
        return None

    s = []
    for type in sections[synopId].keys():
        sc = decode(sections[synopId][type], year, month)
        if sc is not None:
            s.append(sc)
    fullyMerged = mergeSections(s)
    if fullyMerged is None:
        logger.error(f"Can't merge sections - missing mandatory levels! at site: {synopId}")

    return fullyMerged


def getSections(filename, stationList=None):
    """
    Pull each section from the given file.
    :param stationList: A list of synop IDs to pull. If omitted, pulls all stations
    :return: A dict keyed by synop ID, containing the unparsed sections
    """
    sections = {}
    line = None
    try:
        fh = open(filename, 'rb')
        while True:
            line = fh.readline()
            if not line:
                break

            # get rid of non-ASCII characters
            line = ''.join(chr(i) for i in line if i < 128)

            (type, tokens, id) = getTokens(line)
            if type == "":
                continue
            if stationList is not None and id not in stationList:
                continue
            if id not in sections.keys():
                sections[id] = {}

            soundingStr = line
            if "NIL=" in soundingStr:
                continue

            while True:
                line = fh.readline()
                # get rid of non-ASCII characters
                line = ''.join(chr(i) for i in line if i < 128)

                if line:
                    line = line.replace(chr(13), "").replace(chr(10), " ")
                    if line == "" or line == " ":
                        continue
                    soundingStr += line
                    if "=" in line:
                        break
                else:
                    break

            if "NIL=" not in soundingStr:
                sections[id][type] = soundingStr

        fh.close()
        return sections

    except Exception as e:
        logger.error("Problem reading '%s': %s: '%s'" % (filename, e, line))
        return None


def decode(soundingStr, year, month):
    """
    Decode a sounding string containing TT* or PP* data
    :param soundingStr: The complete sounding string
    :return: The decoded data, as a dict
    """
    # clean up the data first
    str = soundingStr.replace("^M", chr(13))
    (type, tokens, id) = getTokens(str)
    if type not in TYPES:
        return None
    decoded = None

    if len(STATIONS) == 0:
        logger.warning("Station file not loaded")
        return None

    if id not in STATIONS:
        logger.error("No station id found for '%s' in the stations file." % id)
        return None
    stationAlt = STATIONS[id]['elev']

    if type == "TTAA":
        decoded = decodeMandatory(type, tokens, stationAlt, year, month, above100=False)
    elif type == "TTBB":
        decoded = decodeSignificant(type, tokens, year, month, above100=False)
    elif type == "PPBB":
        decoded = decodeWinds(type, tokens, stationAlt)
    elif type == "TTCC":
        decoded = decodeMandatory(type, tokens, stationAlt, year, month, above100=True)
    elif type == "TTDD":
        decoded = decodeSignificant(type, tokens, year, month, above100=True)
    elif type == "PPDD":
        decoded = decodeWinds(type, tokens, stationAlt)

    return decoded


def decodeMandatory(type, tokens, stationAlt, year, month, above100=False):
    """
    Decode the TTAA or TTBB section of the sounding
    :param tokens: The parsed tokens making up the section
    :return: A dict containing the mandatory levels and section metadata
    """
    index = 0
    if tokens[index] != type:
        index += 1
    index += 1
    (day, hour) = getDayHour(tokens[index])
    index += 1

    mandatory = {
        'type': type,
        'id': tokens[index],
        'year': year,
        'month': month,
        'day': day,
        'hour': hour,
        'levels': {}
    }

    index += 1
    last_dewdep = None
    while index < len(tokens):
        if tokens[index] == "51515":
            break

        p = tokens[index][:2]

        if p == "88":
            break

        if p in LEVEL_CODES.keys():
            pressure = LEVEL_CODES[p]
            if above100:
                pressure /= 10
            try:
                height = int(tokens[index][2:5])
            except Exception:
                index += 3
                continue

            if pressure == 1000 and height >= 500:
                height = (height - 500) * -1
            elif pressure == 0:
                pressure = height
                if pressure < 500:
                    pressure += 1000
                height = stationAlt
            elif pressure == 850:
                height += 1000
            elif pressure == 700:
                if height > 500:
                    height += 2000
                else:
                    height += 3000
            elif pressure in [500, 400, 300]:
                height *= 10
            elif pressure <= 20:
                if height > 500:
                    height = (2000 + height) * 10
                else:
                    height = (3000 + height) * 10
            elif pressure <= 80:
                if height > 500:
                    height = (1000 + height) * 10
                else:
                    height = (2000 + height) * 10
            elif pressure <= 250:
                height = (1000 + height) * 10

            index += 1
            if index < len(tokens):
                (temp, dew) = getTempDew(tokens[index], last_dewdep)
                if temp is not None and dew is not None:
                    last_dewdep = temp - dew
            else:
                (temp, dew) = (None, None)
            index += 1
            if index < len(tokens):
                (wspd, wdir, u, v) = getWind(tokens[index])
            else:
                (wspd, wdir, u, v) = (None, None, None, None)

            mandatory['levels'][pressure] = {
                'height': height,
                'temp': temp,
                'dew': dew,
                'wspd': wspd,
                'wdir': wdir,
                'u': u,
                'v': v,
                'section': type
            }
        else:
            index += 2  # skip this level and the temp and wind fields

        index += 1

    return mandatory


def decodeSignificant(type, tokens, year, month, above100=False):
    """
    Decode the TTBB or TTDD sections of the data
    :param tokens: The parsed tokens making up the section
    :return: A dict containing the significant levels and section metadata
    """
    index = 0
    if tokens[index] != type:
        index += 1
    index += 1
    (day, hour) = getDayHour(tokens[index])
    index += 1
    significant = {
        'type': type,
        'id': tokens[index],
        'year': year,
        'month': month,
        'day': day,
        'hour': hour,
        'levels': {}
    }

    index += 1
    last_dewdep = None
    while index < len(tokens):
        if tokens[index] in ["21212", "31313", "41414", "51515", "61616"]:
            break
        if tokens[index] == "/////":
            index += 2
            continue

        try:
            pl = int(tokens[index][2:5])
        except Exception:
            index += 2
            continue

        if above100:
            pl /= 10
        elif pl < 100:
            pl += 1000
        index += 1
        if index < len(tokens):
            (temp, dew) = getTempDew(tokens[index], last_dewdep)
            if temp is not None and dew is not None:
                last_dewdep = temp - dew

            index += 1

            significant['levels'][pl] = {
                'temp': temp,
                'dew': dew,
                'section': type
            }

    return significant


def decodeWinds(type, tokens, stationAlt):
    index = 0
    if tokens[index] != type:
        index += 1
    index += 1
    (day, hour) = getDayHour(tokens[index])
    index += 1

    winds = {
        'type': type,
        'id': tokens[index],
        'day': day,
        'hour': hour,
        'heights': {}
    }

    index += 1
    while index < len(tokens):
        try:
            iadd = 0
            t = tokens[index]
            if t[0] == "1":
                iadd = 100000
            baseht = int(t[1]) * 10

            heights = []
            heighttok = tokens[index]
            for h in heighttok[2:5]:
                if h == "/":
                    continue
                height = (baseht + int(h)) * 1000 + iadd  # in feet
                height *= 0.3048  # to meters
                if height == 0:
                    height = stationAlt
                heights.append(height)

            index += 1

            # sometimes the number of tokens that should follow isn't right...
            nexttoks = []
            while index < len(tokens) and tokens[index][0] != '9':
                nexttoks.append(tokens[index])
                index += 1

            # do we have more tokens than heights? then figure out what tokens to use
            if len(nexttoks) > len(heights):
                i = 0
                for h in heighttok[2:5]:
                    if h == '/':
                        nexttoks.pop(i)
                        if len(nexttoks) == len(heights):
                            break
                    else:
                        i += 1

            # do we have less tokens than heights?
            while len(nexttoks) < len(heights):
                heights = heights[:-1]

            for i in range(0, len(heights)):
                if heights[i] not in winds['heights']:
                    (wspd, wdir, u, v) = getWind(nexttoks[i])
                    if heights[i] >= stationAlt:
                        winds['heights'][heights[i]] = {'wspd': wspd, 'wdir': wdir, 'u': u, 'v': v, 'section': type}
        except Exception:
            break

    return winds


def mergeSections(sections):
    """
    Merge all sections into a single profile, interpolating when necessary
    :param sections: A list of parsed data sections
    :return: A merged dict, or None
    """
    sects = {}
    types = []
    for s in sections:
        sects[s['type']] = s
        types.append(s['type'])

    if 'TTAA' not in sects:
        return None

    # merge mandatory levels
    merged = sects['TTAA']
    if 'TTCC' in sects:
        merge(merged, sects['TTCC'])
    levels = {}
    for lv in merged['levels']:
        levels[lv] = merged['levels'][lv]

    # merge in optional levels, which have either
    # pressure or height data (but not both) so get those too
    if 'TTBB' in sects:
        getHeights(sects['TTBB'], levels)
        merge(merged, sects['TTBB'])
    if 'TTDD' in sects:
        getHeights(sects['TTDD'], levels)
        merge(merged, sects['TTDD'])
    if 'PPBB' in sects:
        getPressureLevels(sects['PPBB'], levels)
        merge(merged, sects['PPBB'])
    if 'PPDD' in sects:
        getPressureLevels(sects['PPDD'], levels)
        merge(merged, sects['PPDD'])

    # Fill in missing obs data
    merged['levels'] = interpolateWindAndTemp(merged['levels'])

    # fill in metadata
    synop = merged['id']
    station = STATIONS[synop]
    merged.update(station)
    merged.pop("type", None)
    merged['sections'] = types
    merged['synop'] = synop

    return merged


def merge(dest, src):
    """
    Merge src levels in if they don't already exist in dest
    :param dest: The section dict to merge into
    :param src: The section dict to merge from
    :return: A new set of levels, with levels removed where wind or temp couldn't be determined
    """
    for pressure in src['levels'].keys():
        if pressure not in dest['levels']:
            dest['levels'][pressure] = src['levels'][pressure]


def interpolateWindAndTemp(levels):
    """
    Interpolate temp/dew and wind/dir values where needed
    :param levels: A dict containing levels. Must include pressure info for each level
    :return: A new set of levels, with levels removed where wind or temp couldn't be determined
    """
    pl = sorted(levels.keys(), reverse=True)
    newlevels = {}

    # go through each pressure level
    for i in range(0, len(pl)):
        thispres = pl[i]
        thislevel = levels[thispres]
        haveWind = 'u' in thislevel
        haveTemp = 'temp' in thislevel

        # If we have both, skip this level
        if (haveWind and haveTemp):
            newlevels[thispres] = thislevel
            continue
        # if we have neither, dump the level
        if not (haveWind or haveTemp):
            continue

        # whch variable set do we need?
        missing = ['temp', 'dew'] if haveWind else ['u', 'v']

        # find a level above and below that contain the missing data field
        # so we can interpolate
        i1 = i
        below = None
        while i1 >= 0:
            ll = levels[pl[i1]]
            found = True
            for m in missing:
                if m not in ll or ll[m] is None:
                    found = False
                    break
            if found:
                below = ll
                break
            i1 -= 1
        if below is None:
            continue

        i2 = i
        above = None
        while i2 < len(pl):
            ul = levels[pl[i2]]
            found = True
            for m in missing:
                if m not in ul or ul[m] is None:
                    found = False
                    break
            if found:
                above = ul
                break
            i2 += 1
        if above is None:
            continue

        # get the percentage, i.e. how close this level is to the lower level versus the upper level
        belowpres = pl[i1]
        abovepres = pl[i2]
        ratio = (belowpres - thispres) / (belowpres - abovepres)

        # if we're missing wind, interpolate using the vector values
        if not haveWind:
            u = below['u'] + (above['u'] - below['u']) * ratio
            v = below['v'] + (above['v'] - below['v']) * ratio
            wspd = math.sqrt(u ** 2 + v ** 2)
            wdir = math.atan2(u, v) * (180 / math.pi) + 180
            thislevel['u'] = u
            thislevel['v'] = v
            thislevel['wspd'] = wspd
            thislevel['wdir'] = wdir
        else:
            # for temp and dew, use a straight linear interpolation
            temp = below['temp'] + (above['temp'] - below['temp']) * ratio
            dew = below['dew'] + (above['dew'] - below['dew']) * ratio
            thislevel['temp'] = temp
            thislevel['dew'] = dew

        newlevels[thispres] = thislevel

    return newlevels


def getTokens(soundingStr):
    """
    Return tokens and section type from the given string
    :param soundingStr: The string to parse
    :return: (type, tokens, stationId)
    """
    str = soundingStr.replace(chr(13), "").replace(chr(10), " ")
    str = str.strip()
    toks = re.split(r"\s+", str)
    tokens = []
    for t in toks:
        t = t.replace("=", "")
        if len(t) >= 3:
            tokens.append(t)

    type = ""
    stationId = ""
    if len(tokens) >= 3:
        if tokens[0] in TYPES:
            type = tokens[0]
            stationId = tokens[2]
        elif tokens[1] in TYPES:
            type = tokens[1]
            stationId = tokens[3]

    return (type, tokens, stationId)


def getDayHour(str):
    """
    Parse day of month and hour from the given string
    :param str: The string to parse
    :return: (day, hour)
    """
    try:
        day = int(str[:2]) - 50
        if day < 0:
            day += 50
        hour = int(str[2:4])
        return (day, hour)
    except Exception:
        return (None, None)


def getWind(str):
    """
    Parse wind speed (ms) and wind direction (deg)
    :param str: The string to parse
    :return: (wspd,wdir,u,v)
    """
    wspd = None
    wdir = None
    u = None
    v = None

    try:
        wdir = int(str[0:3])
        wspd = int(str[3:5])
        mid = int(str[2])
        if mid == 1 or mid == 6:
            wdir -= 1
            wspd += 100

        # wspd *= 0.51444, convert to m/s

        # calculate u and v
        md = 270 - wdir
        if md < 0:
            md += 360
        theta = md * (math.pi / 180)
        u = wspd * math.cos(theta)
        v = wspd * math.sin(theta)
    except Exception:
        pass

    return (wspd, wdir, u, v)


def getTempDew(str, last_dewdep):
    """
    Parse temperature and dewpoint from the given string
    :param str: The string to parse
    :param last_dewdep: The last dewpoint depression, in case it's missing
    :return: (temp,dew)
    """
    temp = None
    dew = None

    if str != "/////":
        try:
            temp = int(str[0:2])
            dec = int(str[2])
            temp += 0.1 * dec
            if dec % 2 != 0:
                temp *= -1
        except Exception:
            pass

        try:
            dewdep = int(str[3:5])
            if dewdep <= 50:
                dewdep /= 10
            else:
                dewdep -= 50

            # calculate dewpoint from dewpoint depression
            dew = temp - dewdep
        except Exception:
            # dewpoint is missing or unparesable, so calculate it
            if temp and last_dewdep:
                dew = temp - last_dewdep

    return (temp, dew)


def getPressureLevels(section, levels):
    """
    Interpolate missing pressure levels using surrounding levels containing both height and pressure info.
    Section dict is appended in-place
    :param section: The sounding section to interpolate
    :param levels: A dict of levels containing both height and pressure at each level (and hopefully temperature)
    :return:
    """
    section['levels'] = {}
    pressures = sorted(levels.keys(), reverse=True)

    # loop through each height and try to find surrounding heights with defined
    # levels to interpolate or extrapolate pressure level from

    for height in section['heights'].keys():
        pressurelo = pressures[0]
        pressureup = pressures[-1]
        pressure = None

        if height < levels[pressurelo]['height']:
            pressure = meteo_sounding_utils.pext_down(pressurelo, levels[pressurelo]['temp'], levels[pressurelo]['height'], height)
        elif height > levels[pressureup]['height']:
            pressure = meteo_sounding_utils.pext_up(pressureup, levels[pressureup]['temp'], levels[pressureup]['height'], height)
        else:
            p = 0
            while p < len(pressures) - 1 and levels[pressurelo]['height'] <= height:
                p += 1
                pressurelo = pressures[p]
            pressureup = pressurelo
            pressurelo = pressures[0] if p == 0 else pressures[p-1]

            templo = levels[pressurelo]['temp']
            tempup = levels[pressureup]['temp']
            heightlo = levels[pressurelo]['height']
            heightup = levels[pressureup]['height']
            pressure = meteo_sounding_utils.p_interp(templo, tempup, pressurelo, pressureup, heightlo, heightup, height)

        if pressure is not None:
            level = section['heights'][height]
            level['height'] = height
            section['levels'][pressure] = level


def getHeights(section, levels):
    """
    Interpolate missing heights using surrounding levels containing both height and pressure info.
    Section dict is appended in-place
    :param section: The sounding section to interpolate
    :param levels: A dict of levels containing both height and pressure at each level (and hopefully temperature)
    :return:
    """
    pressures = sorted(levels.keys(), reverse=True)

    for pressure in sorted(section['levels'].keys(), reverse=True):
        pressurelo = pressures[0]
        pressureup = pressures[-1]
        height = None

        if pressure > pressurelo and levels[pressurelo]['height'] is not None \
                and levels[pressurelo]['temp'] is not None:
            height = meteo_sounding_utils.zext_down(pressure, pressurelo, levels[pressurelo]['temp'],
                                                    levels[pressurelo]['height'])
        elif pressure < pressureup and levels[pressureup]['height'] is not None \
                and levels[pressureup]['temp'] is not None:
            height = meteo_sounding_utils.zext_up(pressureup, pressure, levels[pressureup]['temp'],
                                                  levels[pressureup]['height'])
        else:
            p = 0
            while p < len(pressures) - 1 and pressures[p] > pressure:
                p += 1
            pressureup = pressures[p]
            pressurelo = pressures[0] if p == 0 else pressures[p - 1]

            templo = levels[pressurelo]['temp']
            tempup = levels[pressureup]['temp']
            heightlo = levels[pressurelo]['height']
            heightup = levels[pressureup]['height']
            height = meteo_sounding_utils.z_interp(templo, tempup, pressurelo, pressureup, pressure, heightlo, heightup)

        if height is not None:
            section['levels'][pressure]['height'] = height


def printProfile(profile, output=sys.stdout):
    """
    Print the profile to the given output stream
    :param profile: The profile to print
    :param output: The output stream. Defauts to stdout
    :return:
    """
    # print header
    print()
    print("%-15s %s" % ("WMO number:", profile['synop']))
    print("%-15s %s" % ("ICAO ID:", profile['id']))
    print("%-15s %s" % ("Name:", profile['name']))
    print("%-15s %s" % ("Latitude:", profile['lat']))
    print("%-15s %s" % ("Longitude:", profile['lon']))
    print("%-15s %s m" % ("Elevation:", profile['elev']))
    print("%-15s %s/%sz" % ("Day/Hour:", profile['day'], profile['hour']))
    print("%-15s %s" % ("Decoded parts:", ",".join(profile['sections'])))
    print("%8s %6s %6s %6s %6s %6s %6s %10s %10s %6s" %
          ("Pres", "Temp", "Dew", "Wspd", "Wdir", "U", "V", "Height", "Height", "Section"), file=output)
    print("%8s %6s %6s %6s %6s %6s %6s %10s %10s %6s" %
          ("(mb)", "(C)", "(C)", "(m/s)", "(deg)", "", "", "(m)", "(ft)", ""), file=output)
    print("-------- ------ ------ ------ ------ ------ ------ ---------- ---------- ------", file=output)

    levels = profile['levels']

    for pressure in sorted(levels.keys(), reverse=True):
        level = round(pressure, ndigits=1)
        temp = "NULL" if 'temp' not in levels[pressure] or levels[pressure]['temp'] is None \
            else round(levels[pressure]['temp'], ndigits=1)
        dew = "NULL" if 'dew' not in levels[pressure] or levels[pressure]['dew'] is None \
            else round(levels[pressure]['dew'], ndigits=1)
        wspd = "NULL" if 'wspd' not in levels[pressure] or levels[pressure]['wspd'] is None \
            else round(levels[pressure]['wspd'], ndigits=1)
        wdir = "NULL" if 'wdir' not in levels[pressure] or levels[pressure]['wdir'] is None \
            else round(levels[pressure]['wdir'], ndigits=0)
        height = "NULL" if 'height' not in levels[pressure] or levels[pressure]['height'] is None \
            else round(levels[pressure]['height'], ndigits=1)
        u = "NULL" if 'u' not in levels[pressure] or levels[pressure]['u'] is None \
            else round(levels[pressure]['u'], ndigits=1)
        v = "NULL" if 'v' not in levels[pressure] or levels[pressure]['v'] is None \
            else round(levels[pressure]['v'], ndigits=1)
        section = levels[pressure]['section']

        print("%8s %6s %6s %6s %6s %6s %6s %10s %6s" %
              ("%.1f" % level, temp, dew, wspd, wdir, u, v, height, section), file=output)


def change_vars(profile):
    met_utils = meteo_utils.meteo_utils()
    new_profile = {}
    for var_name in meta_keys:
        new_profile[var_name] = []
    for var_name in obsvars:
        new_profile[var_name] = []

    # SPECIAL:  Most soundings launched 50-55 minutes prior to stated synoptic time, so a 12Z
    # launch is usually initiated close to 11:05Z.
    this_datetime = datetime(profile['year'], profile['month'], profile['day'], profile['hour'], 0, 0)
    launch_time = this_datetime - timedelta(seconds=55*60)
    previous_time = launch_time

    heightKm1 = profile['elev']
    levels = profile['levels']
    for pressure in sorted(levels.keys(), reverse=True):
        pres = pressure*100.0
        height = float_missing_value
        temp = float_missing_value
        dewp = float_missing_value
        tvirt = float_missing_value
        spfh = float_missing_value
        u = float_missing_value
        v = float_missing_value
        if 'temp' in levels[pressure] and levels[pressure]['temp'] is not None:
            temp = levels[pressure]['temp'] + 273.15
        if 'dew' in levels[pressure] and levels[pressure]['dew'] is not None:
            dewp = levels[pressure]['dew'] + 273.15
        if (temp > 75 and temp < 355 and dewp > 50 and dewp < 325 and dewp <= temp*1.05 and pres > 100 and pres < 109900):
            spfh = met_utils.specific_humidity(dewp, pres)
            qvapor = max(1.0e-12, spfh/(1.0-spfh))
            tvirt = temp*(1.0 + 0.61*qvapor)
        if 'height' in levels[pressure] and levels[pressure]['height'] is not None:
            height = levels[pressure]['height']
            dz = height - heightKm1
            # Legacy soundings produce null values at mandatory level below ground.
            if (dz < 1.0):
                this_datetime = previous_time
            else:
                # Typical radiosonde ascent rate is 5 m/s
                this_datetime = previous_time + timedelta(seconds=dz*0.2)
            heightKm1 = height
        else:
            this_datetime = previous_time

        if 'u' in levels[pressure] and levels[pressure]['u'] is not None:
            u = levels[pressure]['u']
        if 'v' in levels[pressure] and levels[pressure]['v'] is not None:
            v = levels[pressure]['v']

        time_offset = round((this_datetime - epoch).total_seconds())
        previous_time = this_datetime

        new_profile['station_id'].append(profile['synop'])
        new_profile['latitude'].append(profile['lat'])
        new_profile['longitude'].append(profile['lon'])
        new_profile['station_elevation'].append(profile['elev'])
        new_profile['launch_time'].append(launch_time.strftime("%Y-%m-%dT%H:%M:%SZ"))
        new_profile['dateTime'].append(time_offset)
        new_profile['air_pressure'].append(pres)
        new_profile['height'].append(height)
        new_profile['air_temperature'].append(temp)
        new_profile['virtual_temperature'].append(tvirt)
        new_profile['specific_humidity'].append(spfh)
        new_profile['eastward_wind'].append(u)
        new_profile['northward_wind'].append(v)

    """
    Based on height and time and the wind componenents, predict the lat, lon positions
    as the balloon ascends.  Generally the balloon ascends at 5 m/s, which was already
    assumed in the creation of each timestamp.
    """

    previous_idx = 0
    location = [profile['lon'], profile['lat'], None]
    previous_loc = [profile['lon'], profile['lat'], None]
    delta_t = np.diff(new_profile['dateTime'])

    for idx in range(1, len(delta_t)):

        if (new_profile['eastward_wind'][idx-1] != float_missing_value and new_profile['northward_wind'][idx-1] != float_missing_value):
            # move north-south
            d_north = new_profile['northward_wind'][idx-1] * delta_t[idx-1]
            location = geod.direct(points=previous_loc[:2], azimuths=0., distances=d_north)[0]
            new_profile['latitude'][idx] = location[1]
            # move east-west
            d_east = new_profile['eastward_wind'][idx-1] * delta_t[idx-1]
            location = geod.direct(points=location[:2], azimuths=90., distances=d_east)[0]
            new_profile['longitude'][idx] = location[0]
        else:
            new_profile['latitude'][idx] = new_profile['latitude'][idx-1]
            new_profile['longitude'][idx] = new_profile['longitude'][idx-1]

        # store location for next step calculations
        previous_loc = dcop(location)

    # Be sure to delete the prior location info before exiting
    del location
    del previous_loc

    return new_profile


def append_ioda_data(in_profile, obs_data):
    """
    Append each profile to the end of obs_data.
    """

    for var_name in meta_keys:
        obs_data[var_name].extend(in_profile[var_name])
    for var_name in obsvars:
        obs_data[var_name].extend(in_profile[var_name])

    return obs_data


if __name__ == "__main__":

    import argparse

    start_time = time.time()
    today = datetime.today()

    parser = argparse.ArgumentParser(
        description=(
            'Read legacy radiosonde text file (WMO TEMPO format) and convert into IODA output file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input-files', nargs='+', dest='file_names',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output-file', dest='output_file',
                          action='store', default=None, required=True,
                          help='output file')
    required.add_argument('-t', '--station-file', dest='station_file',
                          action='store', default=None, required=True,
                          help='station table file')

    parser.set_defaults(debug=False)
    parser.set_defaults(verbose=False)
    parser.set_defaults(netCDF=False)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('-y', '--year', dest='year',
                          action='store', default=None, help='year')
    optional.add_argument('-m', '--month', dest='month',
                          action='store', default=None, help='month')
    optional.add_argument('--debug', action='store_true',
                          help='enable debug messages')
    optional.add_argument('--verbose', action='store_true',
                          help='enable verbose debug messages')
    optional.add_argument('--netcdf', action='store_true',
                          help='enable netCDF output file (IODA/JEDI Data Conventions)')

    args = parser.parse_args()

    if args.year:
        year = int(args.year)
    else:
        year = today.year
    if args.month:
        month = int(args.month)
    else:
        month = today.month

    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)

    """
    #---------------------------------------------------------------------------------------------
    Options captured, now ingest the station list and cycle the stations to get each profile.
    #---------------------------------------------------------------------------------------------
    """

    if not os.path.isfile(args.station_file):
        parser.error('Station table (-t option) file: ', args.station_file, ' does not exist')

    loadStations(args.station_file)

    # If using netcdf output option, set up data structures (IODA) for outputs.
    if args.netcdf:
        varDict = defaultdict(lambda: DefaultOrderedDict(dict))
        varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        obs_data = {}          # The final outputs.
        for var_name in meta_keys:
            obs_data[var_name] = []
        for var_name in obsvars:
            obs_data[var_name] = []

    # Loop through input files and decode/convert.
    ntotal = 0
    for file_name in args.file_names:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')
        if args.netcdf:
            AttrData['sourceFiles'] += ", " + file_name
        logging.debug(f"Reading input file: {file_name}")

        nstations = 0
        for n, station in enumerate(STATIONS.keys()):
            logging.debug(f"\n seeking data from station {station} within {file_name} for year:{year} and month:{month}")
            profile = getProfile(file_name, station, year, month)

            if profile:
                nstations += 1
                nlevels = len(profile['levels'])
                if nlevels > 1:
                    ntotal += nlevels
                    logging.debug(f"   found sounding with {nlevels} levels")
                    if args.verbose:
                        printProfile(profile)
                    if args.netcdf:
                        new_profile = change_vars(profile)
                        obs_data = append_ioda_data(new_profile, obs_data)
                else:
                    logging.debug(f"  skipping sounding {station} with 1 or fewer ({nlevels}) levels")

    if args.netcdf:
        ioda_data = {}
        DimDict = {'nlocs': ntotal}
        AttrData['sourceFiles'] = AttrData['sourceFiles'][2:]
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
            dtypestr = MetaDataKeyList[meta_keys.index(key)][1]
            if MetaDataKeyList[meta_keys.index(key)][2]:
                varAttrs[(key, metaDataName)]['units'] = MetaDataKeyList[meta_keys.index(key)][2]
            varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
            ioda_data[(key, metaDataName)] = np.array(obs_data[key], dtype=dtypes[dtypestr])

        # Transfer from the 1-D data vectors and ensure output data (ioda_data) types using numpy.
        for n, iodavar in enumerate(obsvars):
            ioda_data[(iodavar, obsValName)] = np.array(obs_data[iodavar], dtype=np.float32)
            ioda_data[(iodavar, obsErrName)] = np.full(ntotal, obserrlist[n], dtype=np.float32)
            ioda_data[(iodavar, qcName)] = np.full(ntotal, 2, dtype=np.int32)

        logging.debug("Writing output file: " + args.output_file)
        # setup the IODA writer
        writer = iconv.IodaWriter(args.output_file, MetaDataKeyList, DimDict)
        # write everything out
        writer.BuildIoda(ioda_data, VarDims, varAttrs, AttrData)

    print(f" wrote data for {nstations} number of stations")
    logging.info("--- {:9.4f} total seconds ---".format(time.time() - start_time))
