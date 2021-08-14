#!/usr/bin/env python3

from __future__ import print_function
import ncepbufr
import numpy as np
import sys
import os
import netCDF4
from netCDF4 import Dataset
import struct

# Grab input arguemnts
ScriptName = os.path.basename(sys.argv[0])
UsageString = "USAGE: {0:s} <prepbufr>".format(ScriptName)

if len(sys.argv) != 2:
    print("ERROR: must supply exactly 1 arguments")
    print(UsageString)
    sys.exit(1)

PbFname = sys.argv[1]

print("Testing BUFR functions")
print("  Input BUFR file: {0:s}".format(PbFname))
  
# open file and read through contents
bufr = ncepbufr.open(PbFname)

#bufr.print_table()

# prepBUFR mnemonics
Mnemonics = "TOB"
#Mnemonics = "POB QOB TOB ZOB UOB VOB PWO CAT PRSS TDO PMO XDR YDR HRDR"

# BUFR mnemonics
#Mnemonics = "TMDB"
#Mnemonics = "TMDB TMDP WDIR WSPD QMAT QMWN CLAT CLON FLVL YEAR MNTH DAYS HOUR MINU"

while (bufr.advance() == 0):
    print("  MSG: {0:d} {1:s} {2:d} ({3:d})".format(
        bufr.msg_counter,bufr.msg_type,bufr.msg_date,bufr._subsets()))

    isub = 0
    while (bufr.load_subset() == 0):
        isub += 1
        Vals = bufr.read_subset(Mnemonics, events=True).data
        print("    SUBSET: {0:d}: MNEMONIC VALUES: ".format(isub), Vals)

        #bufr.print_subset()

# clean up
bufr.close()

