import tspack
from numpy import array, zeros


def hval(xp, x, y, yp, sigma):
    """Function which evaluates a Hermite interpolatory ten-
    sion spline at a specified point.

    Parameters
    ----------
    xp : numpy array or list
        xp is the values which hval interpolate to.
    x : numpy array or list
        x is the original values of the array.
    y : numpy array or list
        y is the predicted value of the array.
    sigma : array
        Description of parameter `sigma`.

    Returns
    -------
    yp : numpy array
        Predicted y values at xp

    """
    xp = array(xp)
    x = array(x)
    y = array(y)
    sigma = array(sigma)
    y_out = [tspack.hval(xi, x, y, yp, sigma, 1) for xi in xp]
    return y_out


def tspsi(x, y, ncd=1, slopes=None, curvs=None, per=0, tension=None):
    """Subroutine which constructs a shape-preserving or
      unconstrained interpolatory function.  Refer to
      TSVAL1.

    Parameters
    ----------
    x : numpy array or list
        x is the original values of the array.
    y : type
        Description of parameter `y`.
    ncd : type
        Description of parameter `ncd`.
    slopes : type
        Description of parameter `slopes`.
    curvs : type
        Description of parameter `curvs`.
    per : type
        Description of parameter `per`.
    tension : type
        Description of parameter `tension`.

    Returns
    -------
    type
        Description of returned object.

    """
    # Figure out the parameters that need to go into the FORTRAN procedure.
    # First, what iendc is:
    yp = 0.0 * x
    sigma = 0.0 * x

    if slopes is not None and curvs is not None:
        raise ValueError("You can't constrain both the slopes and curvs at the endpoints")
    if slopes is not None:
        if type(slopes) is not type([]):
            raise TypeError("slopes must be a list:  [slope0,slope1]")
        iendc = 1
        yp[0] = slopes[0]
        yp[1] = slopes[1]
    elif curvs is not None:
        if type(curvs) is not type([]):
            raise TypeError("curvs must be a list:  [curv1,curv2]")
        iendc = 2
        yp[0] = curvs[0]
        yp[1] = curvs[1]
    else:
        iendc = 0

    # Now, are we using a uniform tension?
    if tension is not None:
        sigma[0] = tension
        unifrm = 1
    else:
        unifrm = 0

    # Setup the working space
    if ncd == 1:
        lwk = 1
    else:
        if per:
            if unifrm:
                lwk = 2 * len(x)
            else:
                lwk = 3 * len(x)
        else:
            if unifrm:
                lwk = len(x)
            else:
                lwk = 2 * len(x)
    wk = zeros((lwk,), 'd')

    wk, yp, sigma, ier = tspack.tspsi(x, y, ncd, iendc, per, unifrm, wk, yp, sigma)

    if ier >= 0:
        return ((x, y, yp, sigma))
    elif ier == -1:
        raise RuntimeError("Error, N, NCD or IENDC outside valid range")
    elif ier == -2:
        raise RuntimeError("Error, workspace allocated too small")
    elif ier == -3:
        raise RuntimeError("Error, tension outside its valid range")
    elif ier == -4:
        raise RuntimeError("Error, x-values are not strictly increasing")


def tspss(x, y, w, per=0, tension=None, s=None, stol=None, full_output=0):
    # Figure out the parameters that need to go into the FORTRAN procedure.
    # First, what iendc is:
    yp = 0.0 * x
    ys = 0.0 * x
    sigma = 0.0 * x

    # Now, are we using a uniform tension?
    if tension is not None:
        sigma[0] = tension
        unifrm = 1
    else:
        unifrm = 0

    # Setup the working space
    if per:
        if unifrm:
            lwk = 10 * len(x)
        else:
            lwk = 11 * len(x)
    else:
        if unifrm:
            lwk = 6 * len(x)
        else:
            lwk = 7 * len(x)
    wk = zeros((lwk,), 'd')

    wk, sigma, ys, yp, nit, ier = tspack.tspss(x, y, per, unifrm, w, s, stol, wk, sigma, ys, yp)

    if ier == 0:
        mesg = "No errors and constraint is satisfied:  chisquare ~ s"
        xyds = (x, ys, yp, sigma)
    elif ier == 1:
        mesg = "No errors, but constraint not satisfied:  chisquare !~ s"
        xyds = (x, ys, yp, sigma)
    elif ier == -1:
        raise RuntimeError("Error, N, NCD or IENDC outside valid range")
    elif ier == -2:
        raise RuntimeError("Error, workspace allocated too small")
    elif ier == -3:
        raise RuntimeError("Error, tension outside its valid range")
    elif ier == -4:
        raise RuntimeError("Error, x-values are not strictly increasing")
    if full_output:
        return(xyds, nit, mesg)
    else:
        return(xyds)


def tsval1(x, xydt, degree=0, verbose=0):

    if type(xydt) is not type(()):
        raise TypeError("xydt must be a 4-tuple:  x, y, yp, sigma")
    if len(xydt) != 4:
        raise TypeError("xydt must be a 4-tuple:  x, y, yp, sigma")
    xx, yy, yp, sigma = xydt

    y, ier = tspack.tsval1(xx, yy, yp, sigma, degree, x)
    if ier == 0:
        return y
    elif ier > 0 and verbose:
        print("Warning:  extrapolation required for %d points" % ier)
        return y
    elif ier > 0:
        return y
    elif ier == -1:
        raise RuntimeError("degree is not valid")
    elif ier == -2:
        raise ValueError("x values are not strictly increasing")
