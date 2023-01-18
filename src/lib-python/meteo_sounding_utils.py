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
RCP = Rd/Cp
CPR = Cp/Rd
CTOK = 273.15


def pext_down(pres_upper, temperature, hght_upper, hght_lower):
    """
    Returns pressure of a lower height given a
    temperature (assume T=constant) and heights.
    :param pres_upper: Pressure of upper level (mb)
    :param temperature: Constant temperature (C)
    :param hght_upper: Height of upper layer (m)
    :param hght_lower: Height of lower layer (m)
    :return: Extrapolated pressure (mb)
    """
    if pres_upper <= 0 or temperature is None or hght_upper is None or hght_lower is None:
        return None

    return pres_upper * math.exp((G / (Rd * (temperature + CTOK))) * (hght_upper - hght_lower))

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


def pext_up(pres_lower, temperature, hght_upper, hght_lower):
    """
    Returns pressure of an upper height given a
    temperature (assume T=constant) and heights.
    :param pres_upper: Pressure of upper level (mb)
    :param temperature: Constant temperature (C)
    :param hght_upper: Height of upper layer (m)
    :param hght_lower: Height of lower layer (m)
    :return: Extrapolated pressure (mb)
    """
    if pres_lower <= 0 or temperature is None or hght_upper is None or hght_lower is None:
        return None

    return pres_lower * math.exp((G / (Rd * (temperature + CTOK))) * (hght_lower - hght_upper))

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


def zext_up(pres_lower, pres_upper, temperature, hght_lower):
    """
    Returns height of an upper pressure given a
    temperature (assume T=constant) and pressures.
    :param pres_lower: Pressure of lower level (mb)
    :param pres_upper: Pressure of upper level (mb)
    :param temperature: Constant temperature (C)
    :param hght_lower: Height of lower layer (m)
    :return: Extrapolated height (m)
    """
    return hght_lower + (Rd * (temperature + CTOK) / G) * math.log(pres_lower / pres_upper)

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


def zext_down(pres_lower, pres_upper, temperature, hght_upper):
    """
    Returns height of a lower pressure given a
    temperature (assume T=constant) and pressures.
    :param pres_lower: Pressure of lower level (mb)
    :param pres_upper: Pressure of upper level (mb)
    :param temperature: Constant temperature (C)
    :param hght_upper: Height of upper layer (m)
    :return: Extrapolated height (m)
    """
    return hght_upper - (Rd * (temperature + CTOK) / G) * math.log(pres_lower / pres_upper)

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


def p_interp(temp_lower, temp_upper, pres_lower, pres_upper, hght_lower, hght_upper, height):
    """
    Returns the pressure level of an intermediate height, given pressure,temperature and height at
    two surrounding levels. Make an upward and downward integration of the hydrostatic equation to
    the desired level, and then use the mean of the two computation for the result.
    Assumes temperature varies linearly with log(P).
    :param temp_lower: Temperature of lower layer (C)
    :param temp_upper: Temperature of upper layer (C)
    :param pres_lower: Pressure of lower layer (mb)
    :param pres_upper: Pressure of upper layer (mb)
    :param hght_lower: Height of lower layer (m)
    :param hght_upper: Height of upper layer (m)
    :param height: Height of target layer (m)
    :return: Interpolated pressure (mb)
    """
    if pres_lower <= 0 or pres_upper > 1200 or pres_upper <= 0 or pres_upper > 1200 \
            or hght_lower <= -1000 or hght_lower > 40000 or hght_upper <= -1000 or hght_upper > 40000 \
            or temp_lower is None or temp_upper is None:
        return None

    # Speed up if t structure simple.
    if temp_upper == temp_lower:
        s = G / (Rd * (temp_lower + CTOK))
        pm1 = pres_lower * math.exp(s * (hght_lower - height))
        pm2 = pres_upper * math.exp(s * (hght_upper - height))
    else:
        tl = temp_lower + CTOK
        tu = temp_upper + CTOK
        s = (tu - tl) / (hght_upper - hght_lower)
        t0 = tl - s * hght_lower
        a1 = math.log((t0 + s * height) / (t0 + s * hght_lower))
        a2 = math.log((t0 + s * hght_upper) / (t0 + s * height))
        b1 = G * a1 / (s * Rd)
        b2 = G * a2 / (s * Rd)
        pm1 = pres_lower / math.exp(b1)
        pm2 = pres_upper * math.exp(b2)

    return (pm1 + pm2) / 2

# --+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


def z_interp(temp_lower, temp_upper, pres_lower, pres_upper, pressure, hght_lower, hght_upper):
    """
    Returns the height of an intermediate pressure, given pressure,temperature and height at
    two surrounding levels. Make an upward and downward integration of the hydrostatic equation to
    the desired level, and then use the mean of the two computation for the result.
    Assumes temperature varies linearly with log(P).
    :param temp_lower: Temperature of lower layer (C)
    :param temp_upper: Temperature of upper layer (C)
    :param pres_lower: Pressure of lower layer (mb)
    :param pres_upper: Pressure of upper layer (mb)
    :param pressure: Target pressure (mb)
    :param hght_lower: Height of lower layer (m)
    :param hght_upper: Height of upper layer (m)
    :return: Interpolated height (m)
    """
    if pres_lower <= 0 or pres_upper > 1200 or pres_upper <= 0 or pres_upper > 1200 \
            or hght_lower <= -1000 or hght_lower > 40000 or hght_upper <= -1000 or hght_upper > 40000 \
            or temp_lower is None or temp_upper is None:
        return None

    if temp_lower == temp_upper:
        s = Rd * (temp_lower + CTOK) / G
        z1 = hght_lower - s * math.log(pressure / pres_lower)
        z2 = hght_upper + s * math.log(pres_upper / pressure)
    else:
        tl = temp_lower + CTOK
        tu = temp_upper + CTOK
        s = (tu - tl) / (hght_upper - hght_lower)
        f1 = (s * Rd / G) * math.log(pres_lower / pressure)
        f2 = (s * Rd / G) * math.log(pressure / pres_upper)
        t0 = tl - s * hght_lower
        z1 = ((t0 + s * hght_lower) * math.exp(f1) - t0) / s
        z2 = ((t0 + s * hght_upper) * math.exp(-f2) - t0) / s

    return (z1 + z2) / 2
