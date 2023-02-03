#!/usr/bin/env python3

import sys
import subprocess
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

from utils import collect_sources


def ignore(p):
    """ Ignore hidden and test files """
    parts = p.splitall()
    if any(x.startswith(".") for x in parts):
        return True
    if 'test' in parts:
        return True

    return False


def run_pyflakes():
    cmd = ["pyflakes"]
    cmd.extend(collect_sources(ignore_func=ignore))
    return subprocess.call(cmd)


if __name__ == "__main__":
    rc = run_pyflakes()
    sys.exit(rc)
