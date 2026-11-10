import numpy as np

MISSING = 9999

def write_winds_text(filename, yr, mo, da, hr, height, u, u_err, v, v_err):
    """
    Write arrays to a tab-delimited text file, one line per (time, height).
    Time arrays may be [ntimes, nheights] or [ntimes]; height may be
    [ntimes, nheights] or [nheights]. NaN (or masked) values in the float
    columns are written as 9999.
    """
    HEADER = ["Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9"]
    u = np.ma.filled(np.ma.asarray(u, dtype=float), np.nan)
    ntimes, nheights = u.shape

    def full(a, as_int=False):
        a = np.ma.filled(np.ma.asarray(a, dtype=float), np.nan)
        if a.ndim == 1 and a.shape[0] == ntimes:    # per-time array
            a = a[:, None]
        # a 1-D array of length nheights broadcasts along rows automatically
        a = np.broadcast_to(a, (ntimes, nheights)).ravel()
        return a.astype(int) if as_int else a

    ints = [full(x, True) for x in (yr, mo, da, hr)]
    floats = [full(x) for x in (height, u, u_err, v, v_err)]

    def fmt_float(x, spec):
        return str(MISSING) if not np.isfinite(x) else format(x, spec)

    specs = ["g", ".2f", ".2f", ".2f", ".2f"]

    with open(filename, "w") as f:
        f.write("\t".join(HEADER) + "\n")  # header first line
        for i in range(ntimes * nheights):
            fields = [str(c[i]) for c in ints]
            fields += [fmt_float(c[i], s) for c, s in zip(floats, specs)]
            f.write("\t".join(fields) + "\n")
