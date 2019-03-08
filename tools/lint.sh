#!/bin/sh

set -xe

pycodestyle .
python run-pyflakes.py
python run-mccabe.py
pylint mymodule
