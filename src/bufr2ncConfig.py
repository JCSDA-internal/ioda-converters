#!/usr/bin/env python
import numpy as np

MissingInt = -9999
MissingFloat = np.nan

# ObsTypes holds the specifications of message types to extract for a given obs type.
# The structure is a nested dictionary (two levels). The outer dictionary holds entries
# for each observation type. The format for each element in the outer dictionary is:
#
#  <obs_type> : { <message_mnemonic_dictionary> }
#
#  <message_mnemonic_dictionary> has the following entries:
#     'bufr_type' : 'prepBUFR'  # input file is prepBUFR format
#                   'BUFR'      # input file is raw BUFR format
#
#     'num_levels' : 1  # single level data (eg, aircraft, surface obs)
#                    n  # multi-level data (eg, sonde) 
#
#     'msg_type_re' : regular expression to match all message types with <obs_type>
# 
#     'hdr_list' : list of mnemonics for header information
#
#     'obs_list' : list of mnemonics for observation data
#
#     'qm_list' : list of mnemonics for quality marks
#
#     'err_list' : list of mnemonics for observation error values
#
#     'misc_list' : list of mnemonics for miscelaneous data
#
#     'evn_list' : list of mnemonics for event data
#
ObsTypes = {
    #################### prepBUFR obs types #############################

    # Aircraft 
    # Specs are from GSI read_prepbufr.f90
    'Aircraft': {
        'bufr_type'   : 'prepBUFR',
        'num_levels'  : 1,
        'msg_type_re' : 'AIRC[AF][RT]',
        'hdr_list'    : [ 'SID', 'ACID', 'XOB', 'YOB', 'DHR', 'TYP', 'ELV', 'SAID', 'T29' ],
        'obs_list'    : [ 'POB', 'QOB', 'TOB', 'ZOB', 'UOB', 'VOB', 'PWO', 'MXGS', 'PRSS', 'TDO', 'PMO' ],
        'qm_list'     : [ 'PQM', 'QQM', 'TQM', 'ZQM', 'WQM', 'PWQ', 'PMQ' ],
        'err_list'    : [ 'POE', 'QOE', 'TOE', 'WOE', 'PWE' ],
        'misc_list'   : [ 'HOVI', 'CAT', 'XDR', 'YDR', 'HRDR', 'POAF', 'IALR' ],
        'evn_list'    : [ 'TPC', 'TOB', 'TQM' ],
        },

    # Radiosondes
    'Sondes': {
        'bufr_type'   : 'prepBUFR',
        'num_levels'  : 255,
        'msg_type_re' : 'ADPUPA',
        'hdr_list'    : [ 'SID', 'XOB', 'YOB', 'DHR', 'TYP', 'ELV', 'T29' ], 
        'obs_list'    : [ 'POB', 'QOB', 'TOB', 'ZOB', 'UOB', 'VOB', 'PWO', 'TDO' ],
        'qm_list'     : [ 'PQM', 'QQM', 'TQM', 'ZQM', 'WQM', 'PWQ', 'PMQ' ],
        'err_list'    : [ 'POE', 'QOE', 'TOE', 'WOE', 'PWE' ],
        'misc_list'   : [ 'XDR', 'YDR', 'HRDR' ],
        'evn_list'    : [ ],
        },

#        # prepBUFR data types for Sondes
#        # Clara: THIS LIST IS NOT EXHAUSTIVE!!!!
#        #        it is based on dumping a few messages, 
#        #        then screening for vars read in by the gsi
#        #          1. Header
#        #          2. Obs types
#        #          3. quality markers
#        #          4. error ests.
#        #          5. location info?
#        ['POB',  'QOB',  'TOB',  'ZOB',  'UOB',  'VOB',  'PWO', 'TDO',
#         'PQM',  'QQM',  'TQM',  'ZQM',  'WQM',  'PWQ',  'PMQ',
#         'POE',  'QOE',  'TOE',  'WOE',  'PWE',
#         'XDR',  'YDR',  'HRDR'], 

    #################### raw BUFR obs types #############################

    # Aircraft
    'Aircraft_raw': {
        'bufr_type'   : 'BUFR',
        'num_levels'  : 1,
        'msg_type_re' : '^NC004001',
        'hdr_list'    : [ 'YEAR', 'MNTH', 'DAYS', 'HOUR', 'MINU', 'ACID', 'CORN', 'CLAT', 'CLON', 'FLVL' ], 
        'obs_list'    : [ 'TMDB', 'TMDP', 'REHU', 'WSPD', 'WDIR' ],
        'qm_list'     : [ 'QMAT', 'QMDD', 'QMWN' ],
        'err_list'    : [ ],
        'misc_list'   : [ 'SEQNUM', 'BUHD', 'BORG', 'BULTIM', 'BBB', 'RPID' ],
        'evn_list'    : [ ],
        },


# Clara: PREPBUFR FILES INCLUDE (BUT NOT READ BY GSI): 
#            'TSB',  'ITP',  'SQN','PROCN',  'RPT', 'TCOR', 'SIRC',
#        EVENTS VARS? *PC, *RC, *FC , TVO

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
