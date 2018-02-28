#!/usr/bin/env python

from __future__ import print_function
import ncepbufr
import numpy as np
import sys
import os
import netCDF4
from netCDF4 import Dataset
import struct
from io import StringIO

# Capture stdout from function calls
class Capturing(list):
    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self
    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        del self._stringio    # free up some memory
        sys.stdout = self._stdout


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
#Mnemonics = "TOB"
#Mnemonics = "POB QOB TOB ZOB UOB VOB PWO CAT PRSS TDO PMO XDR YDR HRDR"

# BUFR mnemonics
#Mnemonics = "TMDB"
Mnemonics = "TMDB TMDP WDIR WSPD QMAT QMWN CLAT CLON FLVL YEAR MNTH DAYS HOUR MINU"

while (bufr.advance() == 0):
    print("  MSG: {0:d} {1:s} {2:d} ({3:d})".format(
        bufr.msg_counter,bufr.msg_type,bufr.msg_date,bufr._subsets()))

    isub = 0
    while (bufr.load_subset() == 0):
        isub += 1
        #Vals = bufr.read_subset(Mnemonics).data
        #print("    SUBSET: {0:d}: MNEMONIC VALUES: ".format(isub), Vals)
        #with Capturing() as Subset:
        #bak_stdout = sys.stdout
        #Subset = StringIO()
        #sys.stdout = Subset

        # save the current file descriptors for stdout, stderr
        save = os.dup(1), os.dup(2)
        # open two file descriptors that point to /dev/null
        #null_fds = [os.open(os.devnull, os.O_RDWR) for x in range(2)]
        # put /dev/null fds on 1 and 2
        #os.dup2(null_fds[0], 1)
        #os.dup2(null_fds[1], 2)
        # duplicate two file descriptors for capturing output
        my_stdout = StringIO()
        dir(my_stdout)
        os.dup2(my_stdout.fileno(), 1)
        os.dup2(my_stdout.fileno(), 2)

        bufr.print_subset()

        #sys.stdout = bak_stdout

        # restore file descriptors so I can print the results
        os.dup2(save[0], 1)
        os.dup2(save[1], 2)
        # close the temporary fds
        #os.close(null_fds[0])
        #os.close(null_fds[1])

        #print("    SUBSET ({0:d}) : ".format(isub), Subset.getvalue())
        print("    SUBSET ({0:d}) : ".format(isub), my_stdout)

        # clean up saved descriptors
        os.close(save[0])
        os.close(save[1])

# clean up
bufr.close()

