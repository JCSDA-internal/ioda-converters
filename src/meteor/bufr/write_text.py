import numpy as np

def write_winds(filename, yr, mo, da, hr, height, u, u_err, v, v_err):
    """
    Write arrays to a tab-delimited text file, one line per (time, height).
    Time arrays may be [ntimes, nheights] or [ntimes]; height may be
    [ntimes, nheights] or [nheights]. All are broadcast to [ntimes, nheights].
    """
    u = np.asarray(u)
    ntimes, nheights = u.shape

    def full(a, as_int=False):
        a = np.asarray(a)
        if a.ndim == 1 and a.shape[0] == ntimes:    # per-time array
            a = a[:, None]
        # a 1-D array of length nheights broadcasts along rows automatically
        a = np.broadcast_to(a, (ntimes, nheights)).ravel()
        return a.astype(int) if as_int else a.astype(float)

    columns = [
        full(yr, True), full(mo, True), full(da, True), full(hr, True),
        full(height), full(u), full(u_err), full(v), full(v_err),
    ]

    # Row-major flatten: for each time, all heights in order
    # Columns are float-typed in savetxt, so format ints with %d (works on floats)
    fmt = ["%d", "%d", "%d", "%d", "%g", "%.2f", "%.2f", "%.2f", "%.2f"]
    np.savetxt(filename, np.column_stack(columns), fmt=fmt, delimiter="\t")


#if __name__ == "__main__":
#    ntimes, nheights = 3, 4
#    yr = np.full(ntimes, 2019)
#    mo = np.full(ntimes, 6)
#    da = np.full(ntimes, 25)
#    hr = np.array([13, 14, 15])
#    height = np.array([72, 144, 216, 288], dtype=float)   # [nheights]
#    rng = np.random.default_rng(0)
#    u, v, u_err, v_err = (rng.normal(size=(ntimes, nheights)) * 10 for _ in range(4))

#    write_winds("winds.txt", yr, mo, da, hr, height, u, u_err, v, v_err)
