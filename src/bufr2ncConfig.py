#!/usr/bin/env python

# ObsList holds the list of message types for a given obs type. The format 
# for each element of the list is:
#
#  <obs_type> : [ <list_of_bufr_message_types>,
#                 <list_of_bufr_data_types>, <list_of_bufr_event_types> ]
#
ObsList = {
    # Aircraft with conventional obs from prepBUFR file
    # Specs are from GSI read_prepbufr.f90
    'Aircraft_prep': [
        # BUFR message types (regular expression)
        'AIRC[AF][RT]',

        # BUFR data types
        [ 'SID',  'ACID', 'XOB',  'YOB',  'DHR',  'TYP',  'ELV',  'SAID', 'T29',
          'POB',  'QOB',  'TOB',  'ZOB',  'UOB',  'VOB',  'PWO',
          'MXGS', 'HOVI', 'CAT',  'PRSS', 'TDO',  'PMO',
          'POE',  'QOE',  'TOE',  'WOE',  'PWE',
          'PQM',  'QQM',  'TQM',  'ZQM',  'WQM',  'PWQ',  'PMQ',
          'XDR',  'YDR',  'HRDR', 'POAF', 'IALR' ],

        # BUFR event types
        [ 'TPC',  'TOB',  'TQM' ]
        ],

    # Aircraft with conventional obs from BUFR file
    'Aircraft': [
        # BUFR message types
        '^NC00400[1-4]',

        # BUFR data types
        [ 'YEAR', 'MNTH', 'DAYS', 'HOUR', 'MINU', 'SEQNUM', 'BUHD', 'BORG', 'BULTIM', 'BBB',
          'RPID', 'CORN', 'CLAT', 'CLON', 'FLVL', 'QMAT', 'TMDB', 'QMDD', 'TMDP',
          'REHU', 'QMWN', 'WSPD', 'WDIR', 'ACID' ],

        # BUFR event types
        [ ],

        ],

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
