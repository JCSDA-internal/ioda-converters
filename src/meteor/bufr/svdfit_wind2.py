import numpy as np
import copy

def funcwind(x1, x2, ma):
    afunc = np.zeros(ma)
    afunc[0] = np.cos(x1)*np.sin(x2)
    afunc[1] = np.sin(x1)*np.sin(x2)
    afunc[2] = np.cos(x2)
    return afunc

def svdfit_wind(x1, x2, y, sig, ndata, ma, np_, reg):
    NMAX, MMAX, Tol, alpha = 300000, 50, 1e-8, 8e-2
    u, covar = np.zeros((ndata+100,ma)), np.zeros((3,3)) 
    v, w = np.zeros((3,3)), np.zeros(3)
    b = np.zeros(NMAX)
    a = reg.copy()

    for i in range(ndata):
        afunc = funcwind(x1[i], x2[i], ma)
        tmp = 1.0 / sig[i]
        for j in range(ma):
            u[i, j] = afunc[j] * tmp
        b[i] = y[i] * tmp

    u[ndata, 0] = 0
    u[ndata, 1] = 0
    u[ndata, 2] = 1.0 * alpha
    b[ndata] = 1.0 * alpha

    if abs(reg[0]) <= 999.0:
        u[ndata+1, 0] = alpha
        u[ndata+1, 1] = 0.0
        u[ndata+1, 2] = 0.0
        u[ndata+2, 0] = 0.0
        u[ndata+2, 1] = alpha
        u[ndata+2, 2] = 0.0
        u[ndata+3, 0] = 0.0
        u[ndata+3, 1] = 0.0
        u[ndata+3, 2] = alpha  # Fixed typo in Fortran code
        b[ndata+1] = reg[0] * alpha
        b[ndata+2] = reg[1] * alpha
        b[ndata+3] = reg[2] * alpha
        ndata += 4
    else:
        ndata += 1

    # Singular Value Decomposition (SVD)
    u, s, vh = np.linalg.svd(u[:ndata, :ma], full_matrices=False)
    w[:len(s)] = s
    try:
        v[:np_, :np_] = vh.T
    except ValueError:
        return 0, 0, 0
    
    wmax = np.max(w)
    thresh = Tol * wmax
    w[w < thresh] = 0.0

    if u.shape[1] != w.shape[0]:
        print(f"Mismatch between u rows and w: {u.shape[0]} vs {w.shape[0]}")
        return 0, 0, 0
    # Solving the least squares problem
    a[:] = np.linalg.lstsq(u @ np.diag(w) @ v.T, b[:ndata], rcond=None)[0]
    
    # Compute covariance matrix
    covar[:ma, :ma] = np.linalg.inv(v @ np.diag(w**2) @ v.T)
    
    if abs(reg[0]) <= 999.0:
        ndata -= 4
    else:
        ndata -= 1

    chisq = 0.0
    for i in range(ndata):
        afunc = funcwind(x1[i], x2[i], ma)
        sum_ = np.dot(a, afunc)
        chisq += ((y[i] - sum_) / sig[i]) ** 2

    return a, chisq, covar

