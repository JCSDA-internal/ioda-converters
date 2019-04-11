#!/usr/bin/env python

from __future__ import print_function


###########################################################################
# SUBROUTINES
###########################################################################

###########################################################################
# CONSTANTS
###########################################################################

# Some handy constants. These become global variables in this script. Using the
# naming convention of all caps to remind us that these are not to be changed.

# MAX_STRING_LEN is good with 10 characters. This is the length of the long format
# for date and time. Most of the id labels are 6 or 8 characters.
MAX_STRING_LEN = 10

# MAX_EVENTS will usually be limited to 255 due to an array size in the Fortran
# interface. In practice, there are typically a handful of events (4 or 5) since
# the events are related to the steps that are gone through to convert a raw
# BUFR file to a prepBUFR file (at NCEP). It is generally accepted that 20 is
# a safe limit (instead of 255) for the max number of events, so set MaxEvents
# to 20 to help conserve file space.
MAX_EVENTS = 20

# MAX_LEVELS should be limited to 255 by a Fortran array size. This may need to
# change in the future since this number corresponds to the number of atmospheric
# levels in an observation.
MAX_LEVELS = 255

# BUFR file types
BFILE_UNDEF = 0
BFILE_BUFR = 1
BFILE_PREPBUFR = 2

# BUFR types
BTYPE_UNDEF = 0
BTYPE_HEADER = 1
BTYPE_DATA = 2
BTYPE_EVENT = 3
BTYPE_REP = 4

# Data types
DTYPE_UNDEF = 0
DTYPE_STRING = 1   # for CCITT IA5 units in the BUFR table
DTYPE_INTEGER = 2   # for CODE TABLE, FLAG TABLE units in the BUFR table
DTYPE_FLOAT = 3   # for all other units in the BUFR table
DTYPE_UINT = 4   # for dimension coordinates
DTYPE_DOUBLE = 5   # temporary: for strings that are expected to be double
#            in downstream flows (GSI)
