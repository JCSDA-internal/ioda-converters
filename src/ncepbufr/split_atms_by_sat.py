#!/usr/bin/env python3
"""Split an NCEP ATMS BUFR dump by satellite (SAID), copying messages unchanged.
 
BUFR messages are routed whole with ``copymg``, so the outputs
are byte-identical to the input messages and stay compressed; all outputs are
written in one pass.
 
usage: split_atms_by_sat.py [-c] <input.bufr_d> <output_prefix> [npp|n20|n21]
 
  no sat arg  : writes <prefix>.npp.bufr_d, <prefix>.n20.bufr_d, <prefix>.n21.bufr_d
  with sat arg: writes only <prefix>.<sat>.bufr_d
  -c          : check SAID of every subset (detects mixed-satellite messages;
                much slower). Default reads SAID from the first subset only.
 
exit status: 0 = ok
             1 = file error, 2 = usage error (argparse)
             3 = the satellite named in the sat argument had no messages
             4 = (-c only) mixed-satellite messages were found and skipped
 
SAID (WMO common code table C-5): 224 = Suomi-NPP, 225 = NOAA-20, 226 = NOAA-21
 
NCEP dump header messages (0 subsets: dump center/dump time) and the DX table
go to every output. A message whose SAID cannot be decoded is skipped and
counted, never routed with a leftover value.
 
Requires: NCEPLIBS-bufr built with Python support (``import ncepbufr``).
"""
 
import argparse
import os
import sys
 
import ncepbufr
import numpy as np
 
SATS = {'npp': 224, 'n20': 225, 'n21': 226}
SAID_TO_NAME = {v: k for k, v in SATS.items()}
 
 
def get_said(bufr):
    """SAID of the currently loaded subset, or -1 if absent or missing."""
    vals = bufr.read_subset('SAID')
    if vals.size == 0 or np.ma.is_masked(vals[0, 0]):
        return -1
    val = float(vals[0, 0])
    if val < 0 or val >= 1.0e9:          # BUFRLIB missing is ~1e10
        return -1
    return int(round(val))
 
 
def split(infile, prefix, only=None, check_all=False):
    names = [only] if only else list(SATS)
 
    bufr = ncepbufr.open(infile)
    outs = {n: ncepbufr.open(f'{prefix}.{n}.bufr_d', 'w', table=bufr) for n in names}
 
    nmsg = dict.fromkeys(names, 0)
    nobs = dict.fromkeys(names, 0)
    nhdr = nother = nnosaid = nmixed = 0
 
    while bufr.advance() == 0:
        if bufr.subsets == 0:                 # NCEP dump header message
            for out in outs.values():
                out.copy_message(bufr)
            nhdr += 1
            continue
 
        first, mixed = -1, False
        if check_all:
            while bufr.load_subset() == 0:
                s = get_said(bufr)
                if s < 0:
                    mixed = True              # undecodable SAID: don't guess
                elif first < 0:
                    first = s
                elif s != first:
                    mixed = True
        elif bufr.load_subset() == 0:
            first = get_said(bufr)
 
        if mixed:
            nmixed += 1
            continue
        if first < 0:
            nnosaid += 1
            continue
        name = SAID_TO_NAME.get(first)
        if name is None:
            nother += 1
            continue
        if name not in outs:
            continue
 
        outs[name].copy_message(bufr)         # byte-for-byte copy
        nmsg[name] += 1
        nobs[name] += bufr.subsets
 
    for out in outs.values():
        out.close()
    bufr.close()
 
    for n in names:
        print(f'{n}: {nmsg[n]:8d} messages {nobs[n]:10d} obs')
    print(f'header (0-subset) messages copied to each output: {nhdr:8d}')
    print(f'other satellites skipped: {nother:8d} messages')
    print(f'messages with no decodable SAID skipped: {nnosaid:8d}')
    if check_all:
        print(f'mixed-satellite messages skipped: {nmixed:8d}')
    else:
        print('(mixed-satellite check not run; use -c to verify)')
 
    if only and nmsg[only] == 0:
        return 3
    if nmixed > 0:
        print('WARNING: mixed-satellite messages skipped; subset-level split needed',
              file=sys.stderr)
        return 4
    return 0
 
 
def main():
    p = argparse.ArgumentParser(description='Split an NCEP ATMS BUFR dump by satellite.')
    p.add_argument('-c', action='store_true', help='check SAID of every subset')
    p.add_argument('infile')
    p.add_argument('prefix')
    p.add_argument('sat', nargs='?', choices=list(SATS))
    a = p.parse_args()
    if not os.path.isfile(a.infile):
        print(f'ERROR: cannot open {a.infile}', file=sys.stderr)
        return 1
    return split(a.infile, a.prefix, a.sat, a.c)
 
 
if __name__ == '__main__':
    sys.exit(main())
