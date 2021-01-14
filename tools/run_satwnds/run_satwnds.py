#!/usr/bin/env python3
#
# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
from datetime import datetime
import glob
import multiprocessing
import os
import pathlib
import re
import subprocess


# Regex patterns that identify subset categories
OLD_SUBSET_PATTERN = r'NC0050([1245678]\d|90)'
NEW_SUBSET_PATTERN = r'NC0050(3\d|91)'

# Define template file paths
EXE_DIR = pathlib.Path(__file__).parent.absolute()
OLD_SUBSET_TEMPLATE_PATH = os.path.join(EXE_DIR,
                                        'satwnds_old_subset_template.yaml')
NEW_SUBSET_TEMPLATE_PATH = os.path.join(EXE_DIR,
                                        'satwnds_new_subset_template.yaml')


def _make_file_from_template(template_path, replacements, output_path):
    tag_re = re.compile(r'{{\s*(?P<key>\w+)\s*}}')

    lines = []
    with open(template_path, 'r') as template_file:
        for line in template_file.readlines():
            matches = tag_re.findall(line)
            for match_key in matches:
                if match_key in replacements:
                    line = tag_re.sub(replacements[match_key], line)
                else:
                    raise Exception(f'Unknown tag with key {match_key} in \
                                      {template_path}')
            lines.append(line)

    with open(output_path, 'w') as new_file:
        new_file.writelines(lines)


def _process_bufr_path(path):
    file_name = os.path.split(path)[1]
    print(f'Running {file_name}.')

    if re.match(OLD_SUBSET_PATTERN, file_name):
        yaml_template = OLD_SUBSET_TEMPLATE_PATH
    elif re.match(NEW_SUBSET_PATTERN, file_name):
        yaml_template = NEW_SUBSET_TEMPLATE_PATH
    else:
        print(f'Warning: Found undefined subset {file_name}')
        return

    yaml_out_path = f'{file_name}.yaml'
    _make_file_from_template(yaml_template,
                            {'obsdatain': file_name,
                             'obsdataout': f'{file_name}.nc'},
                            yaml_out_path)

    subprocess.call(f'bufr2ioda.x {yaml_out_path}', shell=True)

    # Cleanup
    os.remove(yaml_out_path)


def run(bufr_path, num_threads):
    """
    Splits a Sat Winds file into its subset components and runs bufr2ioda on
    each one.
    :param bufr_path: Path to the Sat winds Bufr file.
    :param num_threads: Number of concurrent converters.
    """

    def _set_up_working_dir():
        timestamp_str = datetime.now().strftime("%Y%m%d%H%M%S")
        working_dir = f'satwnd_processing_{timestamp_str}'
        os.mkdir(working_dir)
        os.chdir(working_dir)

    def _clean_working_dir(bufr_paths):
        for path in bufr_paths:
            os.remove(path)

    input_path = os.path.realpath(bufr_path)

    _set_up_working_dir()

    # Split the input file
    subprocess.call(f'split_by_subset.x {input_path}', shell=True)

    # Process each subset bufr file.
    bufr_paths = glob.glob('NC*')
    pool = multiprocessing.Pool(num_threads)
    pool.map(_process_bufr_path, bufr_paths)

    # Cleanup
    _clean_working_dir(bufr_paths)


if __name__ == '__main__':
    DESCRIPTION = 'Split a sat wind bufr file into its subsets, and runs \
                   bufr2ioda.x on each one with the proper configuration.'

    parser = argparse.ArgumentParser(description=DESCRIPTION)
    parser.add_argument('file',
                        type=str,
                        help="SatWnd file to split.")

    parser.add_argument('-t',
                        '--threads',
                        default=1,
                        type=int,
                        help="Number of concurrent instances of bufr2ioda.")

    args = parser.parse_args()

    start_time = datetime.now()
    run(args.file, args.threads)
    print((datetime.now() - start_time).total_seconds())
