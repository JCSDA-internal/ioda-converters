#!/usr/bin/env python

# ObsList holds the list of message types for a given obs type. The format 
# for each element of the list is:
#
#  <obs_type> : [ <number_of_levels>, <list_of_bufr_message_types>,
#                 <list_of_bufr_data_types>, <list_of_bufr_event_types> ]
#
ObsList = {
    # Aircraft with conventional obs from prepBUFR file
    # Specs are from GSI read_prepbufr.f90
    'Aircraft_prep': [
        # Number of data levels
        1,
        # BUFR message types (regular expression)
        'AIRC[AF][RT]',

        # BUFR message header types 
        [ 'SID',  'ACID', 'XOB',  'YOB',  'DHR',  'TYP',  'ELV',  'SAID', 'T29'], 

        # BUFR data types
        [ 'POB',  'QOB',  'TOB',  'ZOB',  'UOB',  'VOB',  'PWO',
          'MXGS', 'HOVI', 'CAT',  'PRSS', 'TDO',  'PMO',
          'POE',  'QOE',  'TOE',  'WOE',  'PWE',
          'PQM',  'QQM',  'TQM',  'ZQM',  'WQM',  'PWQ',  'PMQ',
          'XDR',  'YDR',  'HRDR', 'POAF', 'IALR' ],

        # BUFR event types
        [ 'TPC',  'TOB',  'TQM' ]
        ],

    # Aircraft with conventional obs from BUFR file
    'Aircraft': [
        # Number of data levels
        1,
        # BUFR message types
        '^NC004001',
# I don't know where the reader is foe this. Not sure I have the split right
        # BUFR message header types 
        [ 'YEAR', 'MNTH', 'DAYS', 'HOUR', 'MINU', 'SEQNUM' ], 

        # BUFR data types
        [ 'BUHD', 'BORG', 'BULTIM', 'BBB',
          'RPID', 'CORN', 'CLAT', 'CLON', 'FLVL', 'QMAT', 'TMDB', 'QMDD', 'TMDP',
          'REHU', 'QMWN', 'WSPD', 'WDIR', 'ACID' ],

        # BUFR event types
        [ ],

        ],

    'Sondes': [
        # Number of levels (255 max allowed in .f90 reader)
        255,
        # BUFR message types (regular expression)
        'ADPUPA',

        # BUFR message header
        [ 'SID',  'XOB',  'YOB',  'DHR',  'TYP',  'ELV',  'T29'], 
        # BUFR data types
        # THIS LIST IS NOT EXHAUSTIVE!!!!
        # it is based on dumping a  few messages, then screening for vars read in by the gsi
        # 1. Header, 2. Obs types, 3. quality markers, 4. error ests., 5. location info?
          ['POB',  'QOB',  'TOB',  'ZOB',  'UOB',  'VOB',  'PWO', 'TDO',
          'PQM',  'QQM',  'TQM',  'ZQM',  'WQM',  'PWQ',  'PMQ',
          'POE',  'QOE',  'TOE',  'WOE',  'PWE',
          'XDR',  'YDR',  'HRDR'], 
        # BUFR event types
        []
        ],
# PREPBUFR FILES INCLUDE (BUT NOT READ BY GSI): 
#          'TSB',  'ITP',  'SQN','PROCN',  'RPT', 'TCOR', 'SIRC',
# EVENTS VARS? *PC, *RC, *FC , TVO
    }

# DataTypes maps the mnemonic to its associated data type
# 
# For now the allowed types are:
#   'string'     for CCITT IA5 units in the BUFR table
#   'integer'    for CODE TABLE units in the BUFR table
#   'float'      all other units in the BUFR table
#
# Keep these types in sync with the ReadBufrData() routine
DataTypes = {
    'SID'    : 'string',
    'ACID'   : 'string',
    'XOB'    : 'float',
    'YOB'    : 'float',
    'DHR'    : 'float',
    'TYP'    : 'integer',
    'ELV'    : 'float',
    'SAID'   : 'integer',
    'T29'    : 'integer',
    'POB'    : 'float',
    'QOB'    : 'float',
    'TOB'    : 'float',
    'ZOB'    : 'float',
    'UOB'    : 'float',
    'VOB'    : 'float',
    'PWO'    : 'float',
    'MXGS'   : 'float',
    'HOVI'   : 'float',
    'CAT'    : 'integer',
    'PRSS'   : 'float',
    'TDO'    : 'float',
    'PMO'    : 'float',
    'POE'    : 'float',
    'QOE'    : 'float',
    'TOE'    : 'float',
    'WOE'    : 'float',
    'PWE'    : 'float',
    'PQM'    : 'integer',
    'QQM'    : 'integer',
    'TQM'    : 'integer',
    'ZQM'    : 'integer',
    'WQM'    : 'integer',
    'PWQ'    : 'integer',
    'PMQ'    : 'integer',
    'XDR'    : 'float',
    'YDR'    : 'float',
    'HRDR'   : 'float',
    'POAF'   : 'integer',
    'IALR'   : 'float', 
    'TPC'    : 'integer',
    'TOB'    : 'float',
    'TQM'    : 'integer',
    'YEAR'   : 'integer',
    'MNTH'   : 'integer',
    'DAYS'   : 'integer',
    'HOUR'   : 'integer',
    'MINU'   : 'integer',
    'SEQNUM' : 'string',
    'BUHD'   : 'string',
    'BORG'   : 'string',
    'BULTIM' : 'string',
    'BBB'    : 'string',
    'RPID'   : 'string',
    'CORN'   : 'integer',
    'CLAT'   : 'float',
    'CLON'   : 'float',
    'FLVL'   : 'float',
    'QMAT'   : 'integer',
    'TMDB'   : 'float',
    'QMDD'   : 'integer',
    'TMDP'   : 'float',
    'REHU'   : 'float',
    'QMWN'   : 'integer',
    'WSPD'   : 'float',
    'WDIR'   : 'float',
    }
