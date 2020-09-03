#!/usr/bin/env python

import ncepbufr
from netCDF4 import Dataset
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from datetime import datetime as dt
import sys
import os
import yaml

from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import bufr2ncCommon as cm
from bufr2ncObsTypes import ObsType

NCEP_CONFIG_PATH = (IODA_CONV_PATH/'config').resolve()

##########################################################################
# SUBROUTINES To be deleted (maybe).
##########################################################################


def MessageCounter(BufrFname):
    # This function counts the number of messages in the file BufrFname
    bufr = ncepbufr.open(BufrFname)
    NumMessages = 0
    Obs.start_msg_selector()
    while (Obs.select_next_msg(bufr)):
        NumMessages += 1

    bufr.close()
    return [NumMessages]


def BfilePreprocess(BufrFname, Obs):
    # This routine will read the BUFR file and figure out how many observations
    # will be read when recording data.

    bufr = ncepbufr.open(BufrFname)

    # The number of observations will be equal to the total number of subsets
    # contained in the selected messages.
    NumObs = 0
    Obs.start_msg_selector()
    while (Obs.select_next_msg(bufr)):
        NumObs += Obs.msg_obs_count(bufr)
    bufr.close()

    return [NumObs, Obs.num_msg_selected, Obs.num_msg_mtype]

# ########################################################################
# ########                  (Prep-) BUFR NCEP Observations               #
# ########################################################################
#
# The class is developed to import all the entries to any BUFR family data set
# that the tables A, B, D of BUFR are embedded to the files.
#


class NcepObsType(ObsType):
    # # initialize data elements ###
    def __init__(self, bf_type, alt_type, tablefile, dictfile):

        super(NcepObsType, self).__init__()

        self.bufr_ftype = bf_type
        self.multi_level = False
        # Put the time and date vars in the subclasses so that their dimensions
        # can vary ( [nlocs], [nlocs,nlevs] ).
        self.misc_spec[0].append(
            ['datetime@MetaData', '', cm.DTYPE_STRING, ['nlocs', 'nstring'], [self.nlocs, self.nstring]])

        if (bf_type == cm.BFILE_BUFR):
            self.mtype_re = alt_type  # alt_type is the BUFR mnemonic

            if os.path.isfile(dictfile):
                # i.e. 'NC001007.dict'
                full_table = read_yaml(dictfile)
                _, blist = read_table(tablefile)
            else:
                full_table, blist = read_table(
                    tablefile)  # i.e. 'NC001007.tbl'
                write_yaml(full_table, dictfile)

            spec_list = get_int_spec(alt_type, blist)
            intspec = []
            intspecDum = []

            if not os.path.isfile(Lexicon):
                for i in spec_list[alt_type]:
                    if i in full_table:
                        intspecDum = [
                            full_table[i]['name'].replace(
                                ' ',
                                '_').replace('/', '_'),
                            i,
                            full_table[i]['dtype'],
                            full_table[i]['ddims']]
                        if intspecDum not in intspec:
                            intspec.append([full_table[i]['name'].replace(
                                ' ', '_').replace('/', '_'),
                                i, full_table[i]['dtype'],
                                full_table[i]['ddims']])
                    # else:
                    # TODO what to do if the spec is not in the full_table (or
                    # in this case, does not have a unit in the full_table)
                for j, dname in enumerate(intspec):
                    if len(dname[3]) == 1:
                        intspec[j].append([self.nlocs])
                    elif len(dname[3]) == 2:
                        intspec[j].append([self.nlocs, self.nstring])
                    else:
                        print('walked off the edge')

                write_yaml(intspec, Lexicon)
            else:
                intspec = read_yaml(Lexicon)
            self.nvars = 0
            for k in intspec:
                if '@ObsValue' in (" ".join(map(str, k))):
                    self.nvars += 1
            # The last mnemonic (RRSTG) corresponds to the raw data, instead
            # of -1 below, it is explicitly removed. The issue with RRSTG is
            # the Binary length of it, which makes the system to crash
            # during at BufrFloatToActual string convention. Probably, there
            # are more Mnemonics with the same problem.

            self.int_spec = [intspec[x:x + 1]
                             for x in range(0, len(intspec), 1)]
            self.evn_spec = []

            spec_list = get_rep_spec(alt_type, blist)
            repspec = []
            repspecDum = []

            for i in spec_list[alt_type]:
                if i in full_table:
                    repspecDum = [
                        full_table[i]['name'].replace(
                            ' ', '_').replace('/', '_'), i,
                        full_table[i]['dtype'],
                        full_table[i]['ddims']]
                    if repspecDum not in repspec:
                        repspec.append([full_table[i]['name'].replace(
                            ' ', '_').replace('/', '_'), i,
                            full_table[i]['dtype'], full_table[i]['ddims']])
            for j, dname in enumerate(repspec):
                if len(dname[3]) == 1:
                    repspec[j].append([self.nlocs])
                elif len(dname[3]) == 2:
                    repspec[j].append([self.nlocs, self.nstring])
                else:
                    print('walked off the edge')

            # write_yaml(repspec, Lexicon)
            self.rep_spec = [repspec[x:x + 1]
                             for x in range(0, len(repspec), 1) if not repspec[x] in intspec]

            # TODO Check the intspec for "SQ" if exist, added at seq_spec
            self.seq_spec = []

            self.nrecs = 1  # place holder

        # Set the dimension specs.
        super(NcepObsType, self).init_dim_spec()
##########################################################################
# read bufr table and return new table with bufr names and units
##########################################################################


def write_yaml(dictionary, dictfileName):
    f = open(dictfileName, 'w')
    yaml.dump(dictionary, f)
    f.close()


def read_yaml(dictfileName):
    f = open(dictfileName, 'r')
    dictionary = yaml.safe_load(f)
    f.close()

    return dictionary


def read_table(filename):
    all = []
    with open(filename) as f:
        for line in f:
            if line[:11] != '|' + '-' * 10 + '|' \
                    and line[:11] != '|' + ' ' * 10 + '|' \
                    and line.find('-' * 20) == -1:
                all.append(line)

    all = all[1:]
    stops = []
    for ndx, line in enumerate(all[1:]):
        if line.find('MNEMONIC') != -1:
            stops.append(ndx)

    part_a = all[2:stops[0]]
    part_b = all[stops[0] + 3:stops[1]]
    part_c = all[stops[1] + 3:]
    dum = []
    for x in part_a:
        dum.append(
            ' '.join(
                x.replace(
                    "(",
                    "").replace(
                    ")",
                    "").replace(
                    "-",
                    "").split()))

    part_a = dum

    tbl_a = {line.split('|')[1].strip(): line.split(
        '|')[3].strip().lower() for line in part_a}
    tbl_c = {line.split('|')[1].strip(): line.split(
        '|')[5].strip().lower() for line in part_c}

    full_table = {i: {'name': tbl_a[i], 'units': tbl_c[i]}
                  for i in tbl_c.keys()}

# TODO Double check the declarations below.
    # DTYPE_INTEGER
    integer_types = [
        'CODE TABLE',
        'FLAG TABLE',
        'YEAR',
        'MONTH',
        'DAY',
        'MINUTE',
        'MINUTES',
        'PASCALS']
    # DTYPE_FLOAT
    float_types = [
        'SECOND',
        'NUMERIC',
        'DEGREES',
        'METERS',
        'METERS/SECOND',
        'M',
        'DECIBELS',
        'HZ',
        'DB',
        'K',
        'KG/M**2',
        'M/S',
        'DEGREE**2',
        'M**2',
        'DEGREES TRUE',
        'PERCENT',
        '%',
        'KG/METER**2',
        'SIEMENS/M',
        'METERS**3/SECOND',
        'JOULE/METER**2',
        'PART PER THOUSAND',
        'PARTS/1000',
        'METERS**2/HZ',
        'S',
        'METERS**2/SECOND',
        'VOLTS',
        'V',
        'DEGREE TRUE',
        'DEGREES KELVIN',
        'HERTZ',
        'HOURS',
        'HOUR',
        'METER/SECOND',
        'DEGREE',
        'SECONDS']
    # DTYPE_STRING
    string_types = ['CCITT IA5']

    string_dims = ['nlocs', 'nstring']
    nums_dims = ['nlocs']

    for key, item in full_table.items():
        if item['units'].upper() in integer_types:
            full_table[key]['dtype'] = cm.DTYPE_INTEGER
            full_table[key]['ddims'] = nums_dims
        elif item['units'].upper() in float_types:
            full_table[key]['dtype'] = cm.DTYPE_FLOAT
            full_table[key]['ddims'] = nums_dims
        elif item['units'].upper() in string_types:
            full_table[key]['dtype'] = cm.DTYPE_STRING
            full_table[key]['ddims'] = string_dims
        else:
            full_table[key]['dtype'] = cm.DTYPE_UNDEF
            full_table[key]['ddims'] = nums_dims
    return full_table, part_b

##########################################################################
# get the int_spec entries from satellite table
##########################################################################


def get_int_spec(mnemonic, part_b):
    # mnemonic is the BUFR msg_type, i.e. 'NC001007'
    # part_b from the read_table, table entries associated with the mnemonic
    # Here part_b=blist
    # find the table entries for the bufr msg_type (mnemonic):
    bentries = {}
    for line in part_b:
        line = line.replace(
            '{',
            '').replace(
            '}',
            '').replace(
            # '(',
            # '').replace(
            # ')',
            # '').replace(
            '<',
            '').replace(
                '>',
            '')
        if line.find(mnemonic) != -1:
            if mnemonic in bentries:
                bentries[mnemonic] = bentries[mnemonic] + \
                    ''.join(line.split('|')[2:]).strip().split()
            else:
                bentries[mnemonic] = ''.join(
                    line.split('|')[2:]).strip().split()
                # bentries is a dictionary for the mnemonic

    for i in range(3):
        for b_monic in bentries[mnemonic]:
            for line in part_b:
                line = line.replace(
                    '{',
                    '').replace(
                    '}',
                    '').replace(
                    # '(',
                    # '').replace(
                    # ')',
                    # '').replace(
                    '<',
                    '').replace(
                    '>',
                    '')
                if line.split('|')[1].find(b_monic) != -1:
                    bentries[mnemonic] = bentries[mnemonic] + \
                        ''.join(line.split('|')[2:]).strip().split()
    return bentries


##########################################################################
# get the rep_spec entries
##########################################################################


def get_rep_spec(mnemonic, part_b):
    # mnemonic is the BUFR msg_type, i.e. 'NC001007'
    # part_b from the read_table, table entries associated with the mnemonic
    #
    # find the table entries for the bufr msg_type (mnemonic):
    bentries = {}
    for line in part_b:
        line = line.replace(
            '{',
            '').replace(
            '}',
            '').replace(
            # '(',
            # '').replace(
            # ')',
            # '').replace(
            '<',
            '').replace(
            '>',
            '')
        if line.find(mnemonic) != -1:
            if mnemonic in bentries:
                bentries[mnemonic] = bentries[mnemonic] + \
                    ''.join(line.split('|')[2:]).strip().split()
            else:
                bentries[mnemonic] = ''.join(
                    line.split('|')[2:]).strip().split()
                # bentries is a dictionary for the mnemonic
    bentries[mnemonic] = [x[1:-1] for x in bentries[mnemonic] if '(' in x]

    for b_monic in bentries[mnemonic]:
        for line in part_b:
            line = line.replace(
                '(',
                '').replace(
                ')',
                '')
            if line.split('|')[1].find(b_monic) != -1:
                bentries[mnemonic] = bentries[mnemonic] + \
                    ''.join(line.split('|')[2:]).strip().split()
    return bentries

##########################################################################
# function to create the full path of
##########################################################################


def get_fname(base_mnemo, BufrPath):
    BufrFname = BufrPath + BufrFname
    BufrTname = base_mnemo + '.tbl'
    NetcdfFname = 'xx' + base_mnemo[5:] + '.nc'

    return BufrFname, BufrTname, NetcdfFname


def create_bufrtable(BufrFname, ObsTable):
    bufr = ncepbufr.open(BufrFname)
    bufr.advance()
    bufr.dump_table(ObsTable)
    bufr.close()
    return

##########################################################################
# MAIN
##########################################################################


if __name__ == '__main__':

    desc = ('Read NCEP BUFR data and convert to IODA netCDF4 format'
            'example: ./ncep_classes.py -p /path/to/obs/ -i obs_filename'
            ' -ot observation type -l yamlfile -m number_of_messages')

    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument(
        '-p', '--obs_path', help='path with the observations',
        type=str, required=True)

    parser.add_argument(
        '-i', '--input_bufr', help='name of the input BUFR file',
        type=str, required=True)

    parser.add_argument(
        '-ot', '--obs_type', help='Submessage of the input BUFR file, e.g., NC001007',
        type=str, required=True)

    parser.add_argument(
        '-o', '--output_netcdf', help='name of the output NC file',
        type=str, required=False, default=None)

    parser.add_argument(
        '-m', '--maxmsgs', help="maximum number of messages to keep",
        type=int, required=False, default=0, metavar="<max_num_msgs>")

    parser.add_argument(
        '-Th', '--thin', type=int, default=1,
        help="select every nth message (thinning)", metavar="<thin_interval>")

    parser.add_argument(
        '-d', '--date', help='file date', metavar='YYYYMMDDHH',
        type=str, required=True)

    parser.add_argument(
        '-l', '--lexicon', help='yaml file with the dictionary', metavar="name_of_dict",
        type=str, required=True)

    parser.add_argument(
        '-Pr', '--bufr', action='store_true', default=1,
        help='input BUFR file is in prepBUFR or BUFR format')

    args = parser.parse_args()

    BufrPath = args.obs_path    # Path of the observations
    MaxNumMsg = args.maxmsgs    # Maximum number of messages to be imported
    ThinInterval = args.thin    # Thinning window. TODO: To be removed, legacy
    ObsType = args.obs_type     # Observation type. e.g., NC001007
    BufrFname = BufrPath + args.input_bufr  # path and name of BUFR name
    DateCentral = dt.strptime(args.date, '%Y%m%d%H')  # DateHH of analysis

    if Path(args.lexicon).is_absolute():
        Lexicon = args.lexicon   # User defined Lexicon path
    else:
        Lexicon = str((NCEP_CONFIG_PATH/args.lexicon).resolve())  # Default Lexicon path

    if (args.bufr):
        BfileType = cm.BFILE_BUFR  # BUFR or prepBUFR. TODO: To be removed
    else:
        BfileType = cm.BFILE_PREPBUFR

    if (args.output_netcdf):
        NetcdfFname = args.output_netcdf   # Filename of the netcdf ioda file
    else:
        NetcdfFname = 'ioda.' + ObsType + '.' + \
            DateCentral.strftime("%Y%m%d%H") + '.nc'

    date_time = DateCentral.strftime("%Y%m%d%H")

    ObsTable = ObsType + '.tbl'       # Bufr table from the data.
    DictObs = ObsType + '.dict'      # Bufr dict

    # Check if BufrFname exists

    if os.path.isfile(BufrFname):
        bufr = ncepbufr.open(BufrFname)
        bufr.advance()
        mnemonic = bufr.msg_type
        bufr.close()
        print('Mnemonic name is ', mnemonic)
    else:
        sys.exit('The ', BufrFname, 'does not exist.')

    #  Check if Bufr Observation Table exists, if not created.
    #  The table is defined as base_mnemo.tbl, it is a text file.

    if os.path.isfile(ObsTable):
        print('ObsTable exists: ', ObsTable)
    else:
        print('ObsTable does not exist, the ', ObsTable, 'is created!')
        create_bufrtable(BufrFname, ObsTable)

    # Create the observation instance

    Obs = NcepObsType(BfileType, mnemonic, ObsTable, DictObs)
    NumMessages = MessageCounter(BufrFname)
    if MaxNumMsg > 0:
        Obs.max_num_msg = MaxNumMsg
    else:
        Obs.max_num_msg = NumMessages[0]
    print("NumMessages = ", NumMessages[0])

    Obs.thin_interval = ThinInterval
    Obs.date_central = DateCentral

    [NumObs, NumMsgs, TotalMsgs] = BfilePreprocess(BufrFname, Obs)
    Obs.set_nlocs(NumObs)

    nc = Dataset(NetcdfFname, 'w', format='NETCDF4')

    nc.date_time = int(date_time[0:10])

    bufr = ncepbufr.open(BufrFname)
    pf_list = ['NC001003', 'NC001103', 'NC031001', 'NC031002', 'NC031003',
               'NC031004', 'NC031005', 'NC031006', 'NC031007']
    if ObsType in pf_list:
        Obs.create_nc_datasets(nc, True)
        Obs.fill_coords(nc)
        Obs.convert(bufr, nc, True)
    else:
        Obs.create_nc_datasets(nc, False)
        Obs.fill_coords(nc)
        Obs.convert(bufr, nc, False)
    bufr.close()
