#Planck function 
import numpy as np       
def rad2bt(wavnum, radiance):

    H_PLANCK = 6.62606957 * 1e-34  # SI-unit = [J*s]
    K_BOLTZMANN = 1.3806488 * 1e-23  # SI-unit = [J/K]
    C_SPEED = 2.99792458 * 1e8  # SI-unit = [m/s]
    
    const1 = H_PLANCK * C_SPEED / K_BOLTZMANN
    const2 = 2 * H_PLANCK * C_SPEED**2
    bt = const1 * wavnum / np.log(np.divide(const2 * wavnum**3, radiance) + 1.0)
    return bt 
