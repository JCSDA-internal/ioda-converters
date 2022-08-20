# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
# Set of routines to do interpolation/extrapolation of P/Z/T sounding data.
# These routines should allow one to compute a full set of P/Z/T data given
# the various sounding parts (e.g. TTAA (mandatory P,Z,T), TTBB (significant
# P,T) and PPBB (winds given height alone).
#
# Peter Neilley, NCAR/RAP 8/93
#
# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

import math

# Constants
Rd = 287.05
Cp = 1004.0
G = 9.8
XMS2KTS = 1.94
RCP = Rd /Cp
CPR = Cp /Rd
CTOK = 273.15

def pext_down(pressure_upper,temperature,height_upper,height_lower):
    """
    Returns pressure of a lower height given a
    temperature (assume T=constant) and heights.
    :param pressure_upper: Pressure of upper level (mb)
    :param temperature: Constant temperature (C)
    :param height_upper: Height of upper layer (m)
    :param height_lower: Height of lower layer (m)
    :return: Extrapolated pressure (mb)
    """
    if pressure_upper <= 0 or temperature is None or height_upper is None or height_lower is None:
        return None

    return pressure_upper * math.exp( (G / (Rd * (temperature + CTOK))) * (height_upper - height_lower) )

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

def pext_up(pressure_lower,temperature,height_upper,height_lower):
    """
    Returns pressure of an upper height given a
    temperature (assume T=constant) and heights.
    :param pressure_upper: Pressure of upper level (mb)
    :param temperature: Constant temperature (C)
    :param height_upper: Height of upper layer (m)
    :param height_lower: Height of lower layer (m)
    :return: Extrapolated pressure (mb)
    """
    if pressure_lower <= 0 or temperature is None or height_upper is None or height_lower is None:
        return None

    return pressure_lower * math.exp( (G / (Rd * (temperature + CTOK))) * (height_lower - height_upper) )

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

def zext_up(pressure_lower,pressure_upper,temperature,height_lower):
    """
    Returns height of an upper pressure given a
    temperature (assume T=constant) and pressures.
    :param pressure_lower: Pressure of lower level (mb)
    :param pressure_upper: Pressure of upper level (mb)
    :param temperature: Constant temperature (C)
    :param height_lower: Height of lower layer (m)
    :return: Extrapolated height (m)
    """
    return height_lower + (Rd * (temperature + CTOK) / G) * math.log(pressure_lower / pressure_upper)

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

def zext_down(pressure_lower,pressure_upper,temperature,height_upper):
    """
    Returns height of a lower pressure given a
    temperature (assume T=constant) and pressures.
    :param pressure_lower: Pressure of lower level (mb)
    :param pressure_upper: Pressure of upper level (mb)
    :param temperature: Constant temperature (C)
    :param height_upper: Height of upper layer (m)
    :return: Extrapolated height (m)
    """
    return height_upper - (Rd * (temperature + CTOK) / G) * math.log(pressure_lower / pressure_upper)

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

def p_interp(temperature_lower,temperature_upper,pressure_lower,pressure_upper,height_lower,height_upper,height):
    """
    Returns the pressure level of an intermediate height, given pressure,temperature and height at
    two surrounding levels. Make an upward and downward integration of the hydrostatic equation to 
    the desired level, and then use the mean of the two computation for the result.
    Assumes temperature varies linearly with log(P).
    :param temperature_lower: Temperature of lower layer (C)
    :param temperature_upper: Temperature of upper layer (C)
    :param pressure_lower: Pressure of lower layer (mb)
    :param pressure_upper: Pressure of upper layer (mb)
    :param height_lower: Height of lower layer (m)
    :param height_upper: Height of upper layer (m)
    :param height: Height of target layer (m)
    :return: Interpolated pressure (mb)
    """
    if pressure_lower <= 0 or pressure_upper > 1200 or pressure_upper <= 0 or pressure_upper > 1200 \
        or height_lower <= -1000 or height_lower > 40000 or height_upper <= -1000 or height_upper > 40000 \
        or temperature_lower is None or temperature_upper is None:
        return None

    # Speed up if t structure simple.
    if temperature_upper == temperature_lower:
       s = G / (Rd * ( temperature_lower + CTOK) )
       pm1 = pressure_lower * math.exp( s * (height_lower - height) )
       pm2 = pressure_upper * math.exp( s * (height_upper - height) )
    else:
       tl = temperature_lower + CTOK
       tu = temperature_upper + CTOK
       s = (tu - tl) / (height_upper - height_lower)
       t0 = tl - s * height_lower
       a1 = math.log( (t0 + s * height) / (t0 + s * height_lower) )
       a2 = math.log( (t0 + s * height_upper) / (t0 + s * height) )
       b1 = G * a1 / (s * Rd)
       b2 = G * a2 / (s * Rd)
       pm1 = pressure_lower / math.exp(b1)
       pm2 = pressure_upper * math.exp(b2)

    return (pm1 + pm2) / 2

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

def z_interp(temperature_lower,temperature_upper,pressure_lower,pressure_upper,pressure,height_lower,height_upper):
    """
    Returns the height of an intermediate pressure, given pressure,temperature and height at
    two surrounding levels. Make an upward and downward integration of the hydrostatic equation to
    the desired level, and then use the mean of the two computation for the result.
    Assumes temperature varies linearly with log(P).
    :param temperature_lower: Temperature of lower layer (C)
    :param temperature_upper: Temperature of upper layer (C)
    :param pressure_lower: Pressure of lower layer (mb)
    :param pressure_upper: Pressure of upper layer (mb)
    :param pressure: Target pressure (mb)
    :param height_lower: Height of lower layer (m)
    :param height_upper: Height of upper layer (m)
    :return: Interpolated height (m)
    """
    if pressure_lower <= 0 or pressure_upper > 1200 or pressure_upper <= 0 or pressure_upper > 1200 \
        or height_lower <= -1000 or height_lower > 40000 or height_upper <= -1000 or height_upper > 40000 \
        or temperature_lower is None or temperature_upper is None:
        return None
        
    if temperature_lower == temperature_upper:
        s = Rd * (temperature_lower + CTOK) / G
        z1 = height_lower - s * math.log(pressure / pressure_lower)
        z2 = height_upper + s * math.log(pressure_upper / pressure)
    else:
        tl = temperature_lower + CTOK
        tu = temperature_upper + CTOK
        s = (tu - tl) / (height_upper - height_lower)
        f1 = (s * Rd / G) * math.log(pressure_lower / pressure)
        f2 = (s * Rd / G) * math.log(pressure / pressure_upper)
        t0 = tl - s * height_lower
        z1 = ( (t0 + s * height_lower) * math.exp(f1)  - t0) / s
        z2 = ( (t0 + s * height_upper) * math.exp(-f2) - t0) / s

    return ( z1 + z2 ) / 2
