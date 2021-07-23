#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# author: Greg Thompson gthompsn AT ucar DOT edu
#

import math


class meteo_utils(object):

    # Constructor
    def __init__(self):
        # Define some constants used for some variable conversions

        self.PI = math.pi
        self.DEG_2_RAD = self.PI/180.
        self.C_2_K = 273.15                      # Zero Centigrade to Kelvin
        self.MS_2_KTS = 1.94                     # Meters per second to knots
        self.KTS_2_MS = 1./self.MS_2_KTS         # Knots to meters per second
        self.inHg_2_Pa = 3386.39                 # Inches of mercury to Pascals
        self.STP_P = 29.92 * self.inHg_2_Pa      # Standard pressure, convert to Pascals
        self.STP_T = 15.0 + self.C_2_K           # Standard temperature, convert to Kelvin
        self.STP_rho_air = self.STP_P/(287.0*self.STP_T)  # Air density at STP (standard temperature and pressure)
        self.FT_2_M = 0.3048                     # Feet to meters
        self.R = 287.04                          # Gas constant
        self.Cp = 1004.0                         # Specific heat capacity
        self.LAPSE = 0.0065                      # Standard lapse rate 6.5C per kilometer
        self.g = 9.80665                         # Gravity (m/s^2)
        self.RHO_WATER = 1000.                   # Density of water (kg/m^3)
#
# --+---+---+---+

    def specific_humidity(self, dewp_K, pres_Pa):

        '''
        Function to compute specific humidity
        Required input variables are dewpoint (K) and surface pressure (Pa)
        From dewpoint a simple water vapor mixing ratio is calculated, then
        specific humidity is r/(1.+r)
        '''

        r = self.r_sub_s(pres_Pa, dewp_K)
        sh = r / (1.0 + r)

        return sh
#
# --+---+---+---+

    def r_sub_s(self, pres_Pa, temp_K):

        '''
        compute saturation mixing ratio (kg/kg) by calling function
        to calculate saturation vapor pressure over water.
        pres_Pa - pressure (pa)
        temp_K  - temperature (k)
        '''

        es = self.e_sub_s(temp_K)

        # Even at P=1050hPa and T=55C, sat. vap. pres only contributes to ~15% of total pressure.
        # The following MIN statement is needed for insanely high altitude global model tops like 1hPa.

        es = min(es, pres_Pa*0.15)

        rs = 0.622*es/(pres_Pa-es)

        return rs
#
# --+---+---+---+

    def e_sub_s(self, temp_K):

        '''
        compute saturation vapor pressure (Pa) over liquid with
        polynomial fit of Goff-Gratch (1946) formulation. (Walko, 1991)
        '''

        c = [610.5851, 44.40316, 1.430341, 0.2641412e-1, 0.2995057e-3, 0.2031998e-5, 0.6936113e-8, 0.2564861e-11, -0.3704404e-13]
        x = max(-80., temp_K-self.C_2_K)
        es = c[0]+x*(c[1]+x*(c[2]+x*(c[3]+x*(c[4]+x*(c[5]+x*(c[6]+x*(c[7]+x*c[8])))))))

        '''
        ALTERNATIVE (costs more CPU, more accurate than Walko, 1991)
        Source: Murphy and Koop, Review of the vapour pressure of ice and
              supercooled water for atmospheric applications, Q. J. R.
              Meteorol. Soc (2005), 131, pp. 1539-1565.

        es = math.exp(54.842763 - 6763.22 / temp_K - 4.210 * math.alog(temp_K) + 0.000367 * temp_K
                       + math.tanh(0.0415 * (temp_K - 218.8)) * (53.878 - 1331.22
                       / temp_K - 9.44523 * math.alog(temp_K) + 0.014025 * temp_K))

        ALTERNATIVE: Classical formula from Rogers and Yau (1989; Eq2.17)

        es = 1000.*0.6112*math.exp(17.67*(temp_K-self.C_2_K)/(temp_K-29.65))
        '''

        return es
#
# --+---+---+---+

    def r_sub_i(self, pres_Pa, temp_K):

        '''
        compute saturation mixing ratio (kg/kg) by calling function
        to calculate saturation vapor pressure over ice.
        pres_Pa - pressure (pa)
        temp_K  - temperature (k)
        '''

        esi = self.e_sub_i(temp_K)

        # Even at P=1050hPa and T=55C, sat. vap. pres only contributes to ~15% of total pressure.
        # The following MIN statement is needed for insanely high altitude global model tops like 1hPa.

        esi = min(esi, pres_Pa*0.15)

        ri = 0.622*esi/(pres_Pa-esi)

        return ri
#
# --+---+---+---+

    def e_sub_i(self, temp_K):

        '''
        compute saturation vapor pressure (Pa) over ice with
        polynomial fit of Goff-Gratch (1946) formulation. (Walko, 1991)
        '''

        c = [.609868993E03, .499320233E02, .184672631E01, .402737184E-1, .565392987E-3, .521693933E-5, .307839583E-7, .105785160E-9, .161444444E-12]
        x = max(-80., temp_K-self.C_2_K)
        esi = c[0]+x*(c[1]+x*(c[2]+x*(c[3]+x*(c[4]+x*(c[5]+x*(c[6]+x*(c[7]+x*c[8])))))))

        '''
        ALTERNATIVE (costs more CPU, more accurate than Walko, 1991)
        Source: Murphy and Koop, Review of the vapour pressure of ice and
              supercooled water for atmospheric applications, Q. J. R.
              Meteorol. Soc (2005), 131, pp. 1539-1565.

        esi = math.exp(9.550426 - 5723.265/temp_K + 3.53068*math.alog(temp_K) - 0.00728332*temp_K)

        ALTERNATIVE from Rogers and Yau (1989; Eq2.17)

        esi = 1000.*0.6112*math.exp(21.8745584*(temp_K-self.C_2_K)/(temp_K-7.66))
        '''

        return esi
#
# --+---+---+---+

    def std_atmos(self, pres_Pa):

        '''
        standard atmos height in meters is returned for given p in Pascals
        '''
        pr = pres_Pa*0.01
        height = 44307.692 * (1.0 - (pr/1013.25)**0.190)

        return height
#
# --+---+---+---+

    def std_atmos_p(self, height):

        '''
        standard atmos pressure in Pascals is returned for given height in meters
        '''

        pr = math.exp(math.log(1.0-height/44307.692)/0.19)*101325.0

        return pr
#
# --+---+---+---+

    def precipitable_water(self, pres_Pa, w_non):

        '''
        Input is a column ordered with lowest index is physically lower in the atmosphere (pressure decreasing as the index increases).
        pres_Pa = Pressure in Pascals
        w_non   = mixing ratio (non-dimensional = kg/kg)
        returned precipitable water value in meters only below 150mb
        '''

        sum = 0.
        k = 1
        while k < len(pres_Pa):
            if (pres_Pa[k] > 15000.0):
                sum = sum + ((w_non[k]+w_non[k-1])*0.5) * abs(pres_Pa[k]-pres_Pa[k-1])
            k = k + 1

        answer = sum/(self.g*self.RHO_WATER)

        return answer
#
# --+---+---+---+

    def dir_speed_2_uv(self, wdir, wspd):

        '''
        From wind direction and speed, compute u,v wind components
        '''

        u = -wspd * math.sin(wdir*self.DEG_2_RAD)
        v = -wspd * math.cos(wdir*self.DEG_2_RAD)

        return u, v
#
# --+---+---+---+

    def altim_2_sfcPressure(self, altim, elev):

        '''
        From altimeter (inches of mercury, Hg), and station elevation (m),
        compute surface pressure returned in Pascals.
        '''

        psfc = altim * ((self.STP_T - self.LAPSE*elev) / self.STP_T)**5.2561
        psfc = psfc * self.inHg_2_Pa

        return psfc
#
# --+---+---+---+

    def theta_e(self, pres_Pa, temp_K, w_non, tlcl_K):

        '''
        The following code was based on Bolton (1980) eqn #43
         and claims to have 0.3 K maximum error within -35 < T < 35 C
            pres_Pa = Pressure in Pascals
            temp_K  = Temperature in Kelvin
            w_non   = mixing ratio (non-dimensional = kg/kg)
            tlcl_K  = Temperature at Lifting Condensation Level (K)
        '''

        pp = pres_Pa
        tt = temp_K
        rr = w_non + 1.e-8
        tlc = tlcl_K

        power = (0.2854*(1.0 - (0.28*rr)))
        xx = tt * (100000.0/pp)**power

        p1 = (3.376/tlc) - 0.00254
        p2 = (rr*1000.0) * (1.0 + 0.81*rr)

        thetae = xx * math.exp(p1*p2)

        return thetae
#
# --+---+---+---+

    def t_lcl(self, temp_K, tdew_K):

        '''
        The following code was based on Bolton (1980) eqn #15
         and claims to have 0.1 K maximum error within -35 < T < 35 C
            temp_K  = Temperature in Kelvin
            tdew_K  = Dewpoint T at Lifting Condensation Level (K)
        '''

        tt = temp_K
        tttd = tdew_K
        denom = (1.0/(tttd-56.0)) + (math.log(tt/tttd)/800.)
        tlcl = (1.0/denom) + 56.0

        return tlcl
#
# --+---+---+---+

    def t_dew(self, pres_Pa, w_non):

        '''
        I cannot recall the original source of this function
            pres_Pa = Pressure in Pascals
            w_non   = mixing ratio (non-dimensional = kg/kg)
        '''

        p = pres_Pa
        rr = w_non+1e-8
        es = p*rr/(.622+rr)
        esln = math.log(es)
        tdew = (35.86*esln-4947.2325)/(esln-23.6837)

        return tdew
#
# --+---+---+---+

    def theta_wetb(self, thetae_K):

        '''
        Eqn below was gotten from polynomial fit to data in
         Smithsonian Meteorological Tables showing Theta-e
         and Theta-w
        '''

        c = [-1.00922292e-10, -1.47945344e-8, -1.7303757e-6, -0.00012709, 1.15849867e-6, -3.518296861e-9, 3.5741522e-12]
        d = [0.0, -3.5223513e-10, -5.7250807e-8, -5.83975422e-6, 4.72445163e-8, -1.13402845e-10, 8.729580402e-14]

        x = min(475.0, thetae_K)

        if (x <= 335.5):
            answer = c(0)+x*(c(1)+x*(c(2)+x*(c(3)+x*(c(4)+x*(c(5)+x*c(6))))))
        else:
            answer = d(0)+x*(d(1)+x*(d(2)+x*(d(3)+x*(d(4)+x*(d(5)+x*d(6))))))

        th_wetb = answer + self.C_2_K

        return th_wetb
#
# --+---+---+---+

    def compT_fr_The(self, thelcl_K, pres_Pa):

        '''
        pres_Pa = Pressure in Pascals
        thelcl  = Theta-e at LCL (units in Kelvin)
        Temperature (K) is returned given Theta-e at LCL
        and a pressure.  This describes a moist-adiabat.
        This temperature is the parcel temp at level Pres
        along moist adiabat described by theta-e.
        '''

        guess = (thelcl_K - 0.5 * (max(thelcl_K-270., 0.))**1.05) * (pres_Pa/100000.0)**.2
        epsilon = 0.01

        iter = 1
        while iter < 100:
            w1 = self.r_sub_s(pres_Pa, guess)
            w2 = self.r_sub_s(pres_Pa, guess+1.)
            tenu = self.theta_e(pres_Pa, guess, w1, guess)
            tenup = self.theta_e(pres_Pa, guess+1, w2, guess+1.)
            cor = (thelcl_K - tenu) / (tenup - tenu)
            guess = guess + cor
            if((cor < epsilon) and (-cor < epsilon)):
                answer = guess
                return answer
            # endif

            iter = iter + 1
        # endwhile

        '''
        If we come out of the iteration without a solution, then it did not converge. This is not good.
        '''

        print("WARNING: solution did not converge in compT_fr_The, " + str(thelcl_K) + ", " + str(pres_Pa))

        thwlcl_K = self.theta_wetb(thelcl_K)
        answer = thwlcl_K*((pres_Pa/100000.0)**0.286)
        return answer
#
# --+---+---+---+
