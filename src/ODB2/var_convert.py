# It is against OOPS/JEDI design principals to convert variables as they
# are being imported from the data source. Nonetheless, it is being
# done for expediency. This code will hopefully one day be removed.

from math import exp


def ConvertRelativeToSpecificHumidity(rh, rh_err, t, p):
    T_KELVIN = 273.15
    ES_ALPHA = 6.112
    ES_BETA = 17.67
    ES_GAMMA = 243.5
    GAS_CONSTANT = 287.0
    GAS_CONSTANT_V = 461.6
    HUNDRED = 100.0

    rdOverRv = GAS_CONSTANT / GAS_CONSTANT_V
    rdOverRv1 = 1.0 - rdOverRv
    t_celcius = t - T_KELVIN
    # p = p / HUNDRED # Convert from Pa to hPa

    # Calculate saturation vapor pressure
    es = ES_ALPHA * exp(ES_BETA * t_celcius / (t_celcius + ES_GAMMA))
    # Calculate saturation specific humidity
    qs = rdOverRv * es / (p - rdOverRv1 * es)
    # Calculate specific humidity
    q = qs * rh / HUNDRED
    q_err = qs * rh_err / HUNDRED
    return q, q_err
